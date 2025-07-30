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

  # Merge coordinates into air quality data
  air_data <- merge(air_data, coords, by = "Entity", all.x = TRUE)

  # Rename coordinate columns if duplicated (from merge)
  if ("Latitude.y" %in% names(air_data)) {
    air_data$Latitude <- air_data$Latitude.y
    air_data$Longitude <- air_data$Longitude.y
  }

  # Remove incomplete records
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
          title = "Click a Country on the Map",
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
      req(input$selected_country)
      air_data[air_data$Entity == input$selected_country, ]
    })

    output$pm25_plot <- renderPlot({
      data <- filtered()
      data <- data[order(data$Year), ]
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
      summary_data <- aggregate(cbind(Latitude, Longitude) ~ Entity, air_data, mean)

      leaflet(summary_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,
          label = ~Entity,
          layerId = ~Entity,
          radius = 6,
          fillColor = "blue",
          fillOpacity = 0.7,
          color = "darkblue"
        )
    })

    # Update input and zoom map when clicking
    observeEvent(input$pm25_map_marker_click, {
      clicked_country <- input$pm25_map_marker_click$id
      updateSelectInput(session, "selected_country", selected = clicked_country)

      country_data <- air_data[air_data$Entity == clicked_country, ]
      lat <- mean(country_data$Latitude, na.rm = TRUE)
      lng <- mean(country_data$Longitude, na.rm = TRUE)

      leafletProxy("pm25_map") %>%
        setView(lng = lng, lat = lat, zoom = 5)
    })

    # Zoom map when changing dropdown
    observeEvent(input$selected_country, {
      country_data <- air_data[air_data$Entity == input$selected_country, ]
      lat <- mean(country_data$Latitude, na.rm = TRUE)
      lng <- mean(country_data$Longitude, na.rm = TRUE)

      leafletProxy("pm25_map") %>%
        setView(lng = lng, lat = lat, zoom = 5)
    })
  }

  shinyApp(ui = ui, server = server)
}


