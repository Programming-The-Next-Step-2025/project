#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import sf
#' @import utils
#' @import ggplot2

startApp <- function() {
  library(shiny)
  library(shinydashboard)
  library(leaflet)
  library(ggplot2)

  # Load and clean the data
  air_data <- read.csv("inst/extdata/openaq.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
  names(air_data)[4] <- "PM25"

  #remove rows with missing PM2.5 or City
  air_data <- air_data[!is.na(air_data$PM25) & air_data$PM25 != "", ]
  air_data$PM25 <- as.numeric(air_data$PM25)
  air_data$Year <- as.numeric(air_data$Year)

  set.seed(123)
  air_data$City <- air_data$Entity  # use Entity as City placeholder
  air_data$Latitude <- jitter(rep(48.85, nrow(air_data)))  # e.g. near Paris
  air_data$Longitude <- jitter(rep(2.35, nrow(air_data)))  # e.g. near Paris

  # UI
  ui <- dashboardPage(
    dashboardHeader(title = "PM2.5 Trends by Country"),
    dashboardSidebar(
      selectInput("selected_country", "Select a country:", choices = sort(unique(air_data$Entity)))
    ),
    dashboardBody(
      fluidRow(
        box(
          title = "PM2.5 Trend",
          width = 6,
          status = "primary",
          plotOutput("pm25_plot")
        ),
        box(
          title = "Click a City on the Map",
          width = 6,
          status = "info",
          leafletOutput("pm25_map", height = 400)
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    filtered <- reactive({
      air_data[air_data$Entity == input$selected_country, ]
    })

    output$pm25_plot <- renderPlot({
      data <- filtered()
      data <- data[order(data$Year), ]
      data <- data[!is.na(data$Year) & !is.na(data$PM25), ]

      if (nrow(data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available", cex = 1.5)
      } else {
        ggplot(data, aes(x = Year, y = PM25)) +
          geom_line(color = "darkred", linewidth = 1) +
          geom_point(color = "darkred", size = 2) +
          labs(
            title = paste("PM2.5 Over Time in", input$selected_country),
            x = "Year", y = "PM2.5 (µg/m³)"
          ) +
          theme_minimal()
      }
    })

    output$pm25_map <- renderLeaflet({
      leaflet(air_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          layerId = ~City,
          label = ~paste(City, "<br>PM2.5:", round(PM25, 1)),
          color = "darkblue",
          radius = 4,
          fillOpacity = 0.7
        )
    })

    observeEvent(input$pm25_map_marker_click, {
      clicked_city <- input$pm25_map_marker_click$id
      if (!is.null(clicked_city)) {
        matched_country <- air_data$Entity[air_data$City == clicked_city][1]
        updateSelectInput(session, "selected_country", selected = matched_country)
      }
    })
  }

  # Launch app
  shinyApp(ui = ui, server = server)
}
