#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "rainfall"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("data", tabName = "data", icon = icon("table")),
      menuItem("explore", tabName = "explore", icon = icon("tint")),
      menuItem("predict", tabName = "predict", icon = icon("line-chart")),
      menuItem("about", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Instructions",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            shiny::h4("Historical data"),
            shiny::em("Add nearby BOM rainfall stations which will provide historical context for distributions and predictions."),
            shiny::em("Find stations using the link below and searching for rainfall stations near a location/address."),
            shiny::br(),
            shiny::a("http://www.bom.gov.au/climate/data/index.shtml", href = "http://www.bom.gov.au/climate/data/index.shtml"),
            shiny::br(),
            shiny::em("Select stations which have a long history of observations, and/or multiple nearby stations which have more recent data."),
            shiny::em("Historical data will be downloaded from the BOM website for each station selected."),
            shiny::h4("Local observations"),
            shiny::em("Select the closest BOM station to use as recent observations, or upload your own CSV.")
          ),
          box(
            title = "BOM Stations",
            width = 12,
            tags$div(
              textInput("bomstation", label = "Add Station ID", placeholder = "000000"),
              actionButton("bomstation_add", "Add", icon = icon("plus"))
            ),
            tags$p(),
            tags$div(
              DT::dataTableOutput("bomstation_summary")
            ),
            footer = shiny::a("Climate Data Online, Bureau of Meteorology", href = "http://www.bom.gov.au/climate/data/index.shtml")
          ),
          box(
            title = "Observations",
            width = 12,
            DT::dataTableOutput("bomstation_data")
          )
        )
      ),

      # Interactive exploration of data
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            title = "Instructions",
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            shiny::h4("Cumulative rainfall profiles and distribution chart"),
            shiny::em("These charts show the cumulative rainfall totals for the same time window over many years of history."),
            shiny::em("The distribution of cumulative total for each historical period is shown as a density plot on the right.")
          )
          ,
          box(
            title = "Inputs",
            collapsible = TRUE,
            width = 12,
            dateRangeInput("explore_datefilter", label = "Select date range to explore", startview = "year"),
            uiOutput("explore_timespan", width = 12),
            numericInput("explore_lowfilter", label = "Remove low-rainfall days below: (mm)", min = 0, value = 0)
          )
          ,
          box(
            title = "Cumulative Rainfall",
            collapsible = FALSE,
            width = 12,
            height = 700,
            plotlyOutput("explore_cumulativeplot")
          )
        )
      )
    )
  )
)
