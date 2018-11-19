library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(scales)
library(cowplot)

source = "IDCJAC0009_040094_1800.zip"

rainfall_ledger <- readr::read_csv(
  unz(file.path("data", source), filename = paste0(stringr::str_replace(basename(source), ".zip", "_Data.csv"))),
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


rainfall_meta <- readr::read_lines(
  unz(file.path("data", source), filename = paste0(stringr::str_replace(basename(source), ".zip", "_Note.txt")))
)
bom_station <- stringr::str_split(rainfall_meta[grepl(rainfall_meta, pattern = "Bureau of Meteorology station number: ")], ": ", simplify = TRUE)[[2]]
bom_name <- stringr::str_split(rainfall_meta[grepl(rainfall_meta, pattern = "Station name: ")], ": ", simplify = TRUE)[[2]]
bom_lat <- stringr::str_split(rainfall_meta[grepl(rainfall_meta, pattern = "Latitude \\(decimal degrees, south negative\\): ")], ": ", simplify = TRUE)[[2]]
bom_lon <- stringr::str_split(rainfall_meta[grepl(rainfall_meta, pattern = "Longitude \\(decimal degrees, east positive\\): ")], ": ", simplify = TRUE)[[2]]

# summary(rainfall_ledger)
# purrr::map(rainfall_ledger, unique)

# Calculate a date object
rainfall_ledger %<>%
  mutate(date = lubridate::ymd(paste(year, month, day, sep = "-")))

rainfall_ledger %<>%
  mutate(doy = lubridate::yday(date))

rainfall_year_cuml <-
  rainfall_ledger %>%
  filter(max(doy) > 360) %>%
  filter(year != 1902) %>%
  filter(!is.na(rainfall_mm)) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(rainfall_mm = cumsum(rainfall_mm)) %>%
  ungroup()

rainfall_year_cuml_summary <-
  rainfall_year_cuml %>%
  group_by(year) %>%
  summarise(rainfall_mm = max(rainfall_mm))

plot_grid(
  rainfall_year_cuml %>%
    ggplot(aes(x = as.Date(lubridate::floor_date(now(), "year") + (lubridate::period(1, "day") * doy)), y = rainfall_mm, group = year)) +
    geom_line(aes(colour = "hist")) +
    geom_line(data = rainfall_year_cuml %>% filter(year == lubridate::year(now() - period(1, "year"))), colour="blue", aes(size = "last")) +
    geom_line(data = rainfall_year_cuml %>% filter(year == lubridate::year(now())), colour = "blue", aes(size = "curr")) +
    scale_colour_manual(name="", labels = c("History"), values=c("hist" = "grey")) +
    scale_size_manual(name="", labels = c("last" = "2017", "curr" = "2018"), values=c("last" = 0.5, "curr" = 2)) +
    scale_x_date(
      breaks = as.Date(floor_date(now(), "year") + (period(1, "month") * seq(0,12)) + period(1, "day")),
      labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "J+")) +
    coord_cartesian(ylim = c(0, ceiling(max(rainfall_year_cuml_summary$rainfall_mm)))) +
    theme(
      legend.position = "bottom",
      plot.margin = margin(r = -10, unit = "pt"),
      plot.title = element_text(hjust = 0),
      axis.title.x = element_blank(),
      plot.caption = element_text(size = 8)
    ) +
    labs(
      title = stringr::str_to_title(bom_name),
      subtitle = paste0("bom: ", bom_station, " {", bom_lat, ", ", bom_lon, "} | ", paste0(range(rainfall_year_cuml$year), collapse = "-")),
      caption =""
    )
  ,
  rainfall_year_cuml_summary %>%
    ggplot(aes(x = rainfall_mm)) +
    geom_density(fill = "lightgrey", col = "grey") +
    geom_vline(xintercept = rainfall_year_cuml_summary %>% filter(year == year(now())) %>% pull(rainfall_mm), colour = "blue") +
    geom_rug() +
    xlab("") +
    coord_flip(xlim = c(0, ceiling(max(rainfall_year_cuml_summary$rainfall_mm)))) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "white"),
      plot.margin = margin(l = -10, unit = "pt"),
      axis.line = element_line(colour = "white"),
      axis.title.x = element_blank(),
      plot.caption = element_text(size = 8, margin = margin(t = 49, unit = "pt"))
    ) +
    labs(title = " ", subtitle = "", caption = "source: https://github.com/kevstark/climate-rainfall")
  , axis = "b"
  , rel_widths = c(0.8, 0.2)
)
ggsave(file.path("img", stringr::str_replace(source, ".zip$", ".png")), width = 12, height = 9)

# Inputs
# History (months: 1:n)
# Jitter (weeks: 1:n)
