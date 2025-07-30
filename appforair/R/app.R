#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import ggplot2
#' @export

startApp <- function() {
  # Load cleaned PM2.5 data
  air_data <- load_pm25_data()

  # Load country coordinates
  coords_path <- system.file("extdata", "country_coordinates.csv", package = "appforair")
  coords <- read.csv(coords_path, stringsAsFactors = FALSE)

  # Merge coordinates into the air quality data
  air_data <- merge(air_data, coords, by.x = "Entity", by.y = "Entity", all.x = TRUE)

  # Rename coordinate columns if needed
  colnames(air_data)[colnames(air_data) == "Latitude.y"] <- "Latitude"
  colnames(air_data)[colnames(air_data) == "Longitude.y"] <- "Longitude"

  # Remove rows with missing values after merging
  air_data <- air_data[!is.na(air_data$PM25) & !is.na(air_data$Latitude) & !is.na(air_data$Longitude), ]

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
      data <- filtered()

      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          layerId = ~City,
          label = ~paste(City, "<br>PM2.5:", round(PM25, 1)),
          color = "darkblue",
          radius = 6,
          fillOpacity = 0.7
        ) %>%
        setView(
          lng = mean(data$Longitude, na.rm = TRUE),
          lat = mean(data$Latitude, na.rm = TRUE),
          zoom = 4
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
