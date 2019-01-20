#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(magrittr)
library(tidyr)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  bomstationids <- reactiveVal(value = character(0))
  bomstationdata <- reactiveValues()

  bomstation_ledger <- reactiveVal()

  # Reset input box on add
  observeEvent(input$bomstation_add, {
    if(nchar(input$bomstation) == 6 & !is.na(as.numeric(input$bomstation))) {
      bomstationids(unique(append(bomstationids(), input$bomstation)))
      message("Added ", input$bomstation, ": { ", paste0(bomstationids(), collapse = " | ") ," }")
      updateTextInput(session, inputId = "bomstation", value = "")
    }
  })

  # Download data and metadata for added stationids
  observe({
    req(bomstationids)
    missing_ids <- bomstationids()[!bomstationids() %in% names(bomstationdata)]
    missing_data <- purrr::map(
      missing_ids,
      function(station) {
        # Trigger the data coverage image generation
        withProgress(value = 0.1, message = "Calculating data coverage", {
          status = httr::status_code(
            httr::GET(
              paste0("http://www.bom.gov.au/jsp/ncc/cdio/wData/wdata?p_stn_num=", station, "&p_display_type=holdingGraph&p_nccObsCode=136&sid=")
            )
          )
          # Retrieve data coverage image as:
          #

          download_path <- paste0(station, ".zip")
          download_flag <- TRUE

          if(file.exists(download_path)) {
            if(lubridate::interval(file.info(download_path)$mtime, lubridate::now()) / lubridate::days(1) < 1) {
              download_flag <- FALSE
            }
          }
          if(download_flag) {
            setProgress(0.3, message = "Downloading rainfall data")
            # Download the zipped data
            download.file(
              paste0(
                "http://www.bom.gov.au",
                rvest::html_session(
                  paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", station)
                ) %>%
                  rvest::html_node(xpath = "//ul[@class='downloads']/li[2]/a") %>%
                  rvest::html_attr("href")
              ),
              destfile = download_path,
              mode = "wb"
            )
          }

          setProgress(0.7, message = "Loading rainfall csv")

          csv <- readr::read_csv(
            unz(download_path, filename = unzip(download_path, list = TRUE) %>% filter(str_detect(Name, ".csv")) %>% pull(Name)),
            col_names = c("bom_product", "bom_station", "year", "month", "day", "rainfall_mm", "rainfall_days", "quality"),
            skip = 1,
            col_types = cols(
              bom_product = col_character(),
              bom_station = col_character(),
              year = col_integer(),
              month = col_integer(),
              day = col_integer(),
              rainfall_mm = col_double(),
              rainfall_days = col_integer(),
              quality = col_character()
            )
          )

          setProgress(0.9, message = "Loading rainfall meta")
          txt <- readr::read_lines(
            unz(download_path, filename = unzip(download_path, list = TRUE) %>% filter(str_detect(Name, ".txt")) %>% pull(Name))
          )
          meta <- list(
            station = stringr::str_split(txt[grepl(txt, pattern = "Bureau of Meteorology station number: ")], ": ", simplify = TRUE)[[2]],
            coverage = paste0("http://www.bom.gov.au/tmp/cdio/", station, "_136.gif"),
            name = stringr::str_split(txt[grepl(txt, pattern = "Station name: ")], ": ", simplify = TRUE)[[2]],
            lat = stringr::str_split(txt[grepl(txt, pattern = "Latitude \\(decimal degrees, south negative\\): ")], ": ", simplify = TRUE)[[2]],
            lon = stringr::str_split(txt[grepl(txt, pattern = "Longitude \\(decimal degrees, east positive\\): ")], ": ", simplify = TRUE)[[2]]
          )

          setProgress(1, message = "Complete")
          return(list(data = csv, meta = meta))
        })
      }
    )
    for(i in seq_along(missing_ids)) {
      bomstationdata[[missing_ids[i]]] <- missing_data[[i]]
    }
    isolate(saveRDS(reactiveValuesToList(bomstationdata), "debug_bomstationdata.RDS"))
  })

  # Parse and reformat rainfall ledger
  observe({
    req(bomstationdata)
    if(length(names(bomstationdata)) > 0) {
      ledger <- bind_rows(purrr::map(names(bomstationdata), ~ bomstationdata[[.]][["data"]]))
      ledger <- mutate(ledger, date = lubridate::ymd(paste(year, month, day, sep = "-")))
      # Convert multi-day rain events back to daily (as average)
      ledger %<>% arrange(bom_station, date) %>% mutate(rainfall_mm_day = rainfall_mm)
      for (i in seq_len(nrow(ledger))) {
        if(!is.na(ledger$rainfall_days[i])) {
          if(ledger$rainfall_days[i] > 1) {
            ledger$rainfall_mm_day[i:(i - ledger$rainfall_days[i] + 1)] <- ledger$rainfall_mm[i] / ledger$rainfall_days[i]
          }
        }
      }
      meta <- bind_rows(purrr::map(names(bomstationdata), ~bomstationdata[[.]][["meta"]])) %>%
        select(-coverage) %>%
        mutate(station = stringr::str_pad(station, 6, side = "left", pad = "0"))

      ledger <- ledger %>% left_join(meta, by = c("bom_station" = "station")) %>% arrange(name)
      bomstation_ledger(ledger)
      saveRDS(ledger, "debug_bomstation_ledger.RDS")
    }
  })

  # Update the available date range as new station data is added
  observe({
    req(bomstation_ledger)
    if(!is.null(bomstation_ledger())) {
      print("updateDateRangeInput(bomstation_ledger())")
      updateDateRangeInput(
        session,
        "explore_datefilter",
        start = min(bomstation_ledger()$date),
        end = max(bomstation_ledger()$date),
        min = lubridate::floor_date(min(bomstation_ledger()$date), "year"),
        max = lubridate::ceiling_date(max(bomstation_ledger()$date), "year")
      )
    } else {
      print("updateDateRangeInput()")
      updateDateRangeInput(
        session,
        "explore_datefilter",
        start = lubridate::now(),
        end = lubridate::now(),
        min = lubridate::floor_date(lubridate::now(), "year"),
        max = lubridate::ceiling_date(lubridate::now(), "year")
      )
    }
  })

  # Update metadata table with stationid info
  output$bomstation_summary <- DT::renderDataTable({
    req(bomstationdata)
    message("output$bomstation_summary: ", names(bomstationdata))

    if(length(names(bomstationdata)) > 0) {
      meta <- bind_rows(purrr::map(names(bomstationdata), ~ bomstationdata[[.]][["meta"]]))
      meta %<>% mutate(coverage = paste0('<img src="', coverage, '" height="30" style="align:left;border:0px;vspace:0px"/><img src="http://www.bom.gov.au/climate/cdo/images/about/timeline.png" height="19" style="align:left;border:0px;vspace:0px"/>'))
      meta %<>% select(station, name, lon, lat, coverage)

      DT::datatable(
        meta,
        options = list(
          orderClasses = TRUE,
          columnDefs = list(list(width = '400px', targets = -1))
        ),
        escape = FALSE)
    }
  })

  # Update ledger with new data
  output$bomstation_data <- DT::renderDataTable({
    DT::datatable(bomstation_ledger())
  })

  # Generate cumulative rainfall distribution plot
  output$explore_cumulativeplot <- plotly::renderPlotly({
    req(bomstationdata)
    req(input$explore_datefilter)
    req(input$explore_timespan)
    cat("explore_cumulativeplot\n")
    cat("  input$explore_datefilter: ", paste(input$explore_datefilter, collapse = " to "), "\n")
    cat("  input$explore_timespan: ", ui_timespan_months[input$explore_timespan + 1], "\n")
    cat("  bomstation_ledger: [", paste(dim(bomstation_ledger()), collapse = " x "), "]\n")

    #bomstation_ledger <- readRDS("Rshiny/debug_bomstation_ledger.RDS")
    #bomstationdata <- readRDS("Rshiny/debug_bomstationdata.RDS")
    # input <- list(
    #   explore_datefilter = c("1800-01-01", "2018-12-31"),
    #   explore_timespan = which(names(ui_timespan_months) == "5y") - 1,
    #   explore_lowfilter = 5
    # )

    # Calculate a day-of-year index
    ledger <- bomstation_ledger() %>%
      arrange(date) %>%
      filter(date >= lubridate::ymd(input$explore_datefilter[1])) %>%
      filter(date <= lubridate::ymd(input$explore_datefilter[2])) %>%
      mutate(rainfall_mm_day = ifelse(rainfall_mm_day < input$explore_lowfilter, 0, rainfall_mm_day)) %>%
      mutate(doy = lubridate::yday(date))
    cat("  ledger: [", paste(dim(ledger), collapse = " x "), "]\n")

    # Calculate the number of cumulative sequences in the dataset
    ledger_seq <- ledger %>%
      distinct(bom_station, name, year, month = lubridate::month(input$explore_datefilter[2]), day = lubridate::day(input$explore_datefilter[2])) %>%
      mutate(timespan_mon = ui_timespan_months[input$explore_timespan + 1]) %>%
      mutate(date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>%
      mutate(date_start = date %m+% months(-ui_timespan_months[input$explore_timespan + 1])) %>%
      tidyr::unite("id", bom_station, date, sep = "_", remove = FALSE)
    ledger_seq$days = lubridate::interval(ledger_seq$date_start, ledger_seq$date) %/% lubridate::days(1)
    cat("  ledger_seq: [", paste(dim(ledger_seq), collapse = " x "), "]\n")

    # Split ledger by unique data sequences and timespans, calculate cumulative rainfall
    ledger_seq_data <- purrr::map(
      seq_len(nrow(ledger_seq)),
      function(i) {
        ledger %>%
          # Filter to data for the sequence
          filter(bom_station == ledger_seq[i, "bom_station"][[1]]) %>%
          filter(date <= ledger_seq[i, "date"][[1]]) %>%
          filter(date > ledger_seq[i, "date_start"][[1]]) %>%
          # Calculate cumulative rainfall
          filter(!is.na(rainfall_mm_day)) %>%
          arrange(date) %>%
          mutate(rainfall_mm_cuml = cumsum(rainfall_mm_day)) %>%
          mutate(date_norm = lubridate::ymd(input$explore_datefilter[2]) + (date - ledger_seq[i, ]$date))
      }
    )

    # Remove incomplete seqences
    ledger_seq_valid <- 1 - (ledger_seq$days - purrr::map_int(ledger_seq_data, nrow)) / ledger_seq$days > 0.95
    ledger_seq_valid[is.na(ledger_seq_valid)] <- FALSE
    cat("  ledger_seq_valid: ", sum(ledger_seq_valid), "\n")

    # index numbers of all valid sequences, plus the last sequence if it has data (which may be partial/invalid)
    ledger_seq_valid_plot <- seq_len(nrow(ledger_seq))[ledger_seq_valid]
    if(nrow(ledger_seq_data[[length(ledger_seq_data)]]) > 0)
    {
      ledger_seq_valid_plot <- unique(c(ledger_seq_valid_plot, length(ledger_seq_data)))
    }

    # Generate cumulative rainfall plot
    p1 <- plot_ly(height=800)

    purrr::walk(
      ledger_seq_valid_plot,
      function(i) {
        data <- ledger_seq_data[[i]] #%>% group_by(rainfall_mm_cuml) %>% dplyr::slice(1 | n()) %>% ungroup()
        p1 <<- add_trace(
          p1,
          data = data,
          x = ~date_norm,
          y = ~rainfall_mm_cuml,
          color = ~name,
          type = 'scatter',
          mode = 'lines',
          text = paste0(data$bom_station, " ", data$name, " | ", data$date, ": ", formatC(round(data$rainfall_mm_cuml), big.mark=","), "mm"),
          hoverinfo = 'text',
          opacity = ifelse(ledger_seq[i, "date"][[1]] == input$explore_datefilter[2], 1, 0.5),
          line = list(width = ifelse(ledger_seq[i, "date"][[1]] == input$explore_datefilter[2], 5, 1)),
          showlegend = FALSE
        )
      }
    )

    # Show abbr month by default
    p1 <- p1 %>% layout(xaxis = list(tickformat = '%b'))
    # Show day-Month if less or equal to than 6 months
    if(ui_timespan_months[input$explore_timespan + 1] <= 6) { p1 <- p1 %>% layout(xaxis = list(tickformat = '%d-%b')) }
    # Show Year/Mon if greater than 12 months
    if(ui_timespan_months[input$explore_timespan + 1] > 12) { p1 <- p1 %>% layout(xaxis = list(tickformat = '%Y/%m')) }
    # Show only year if more than 5y
    if(ui_timespan_months[input$explore_timespan + 1] > 60) { p1 <- p1 %>% layout(xaxis = list(tickformat = '%Y')) }

    ledger_seq_max = tibble(
      bom_station = purrr::map_chr(ledger_seq_data, ~ .[1, "bom_station"][[1]]),
      name = purrr::map_chr(ledger_seq_data, ~ .[1, "name"][[1]]),
      rainfall_mm_cuml = suppressWarnings(purrr::map_dbl(purrr::map(ledger_seq_data, "rainfall_mm_cuml"), max, na.rm = TRUE))
    ) %>%
      mutate(rainfall_mm_cuml = ifelse(is.infinite(rainfall_mm_cuml), 0, rainfall_mm_cuml)) %>%
      filter(ledger_seq_valid)

    ledger_seq_dens <- with(ledger_seq_max, tapply(rainfall_mm_cuml, INDEX = name, density))
    ledger_seq_dens <- tibble(
      rainfall_mm_cuml = unlist(lapply(ledger_seq_dens, "[[", "x")),
      density = unlist(lapply(ledger_seq_dens, "[[", "y")),
      name = rep(names(ledger_seq_dens), each = length(ledger_seq_dens[[1]]$x))
    )

    # Density plot per station
    p2 <- plot_ly(
      ledger_seq_dens,
      x = ~density,
      y = ~rainfall_mm_cuml,
      color = ~name,
      text = paste0(ledger_seq_dens$name, ": ", formatC(round(ledger_seq_dens$rainfall_mm_cuml), big.mark=","), "mm"),
      type = 'scatter', mode = 'lines',
      height=800,
      hoverinfo = 'text',
      fill = 'tozerox'
    ) %>% layout(
      xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
      yaxis = list(hoverformat = '.0f'),
      showlegend = TRUE
    )

    subplot(p1, p2, nrows = 1, shareY = TRUE, widths = c(0.8, 0.2)) %>%
      layout(
        title = "Cumulative Rainfall & Frequency Distribution",
        yaxis = list(rangemode = "tozero"),
        legend = list(x = 0.1, y = 0.9)
      )
  })

  output$explore_timespan <- renderUI({
    # Manually build arguments for the sliderInput
    args <- list(
      inputId = "explore_timespan",
      label = "Select the cumulative time-span to explore",
      ticks = TRUE,
      value = which(names(ui_timespan_months) == "1 year") - 1,
      min = 1,
      max   <- length(ui_timespan_months),
      width = "100%"
    )

    # Build slider html
    html  <- do.call('sliderInput', args)

    # Override dom with custom tick labels
    html$children[[2]]$attribs[['data-values']] <- paste0(names(ui_timespan_months), collapse=',');

    html
  })

})
