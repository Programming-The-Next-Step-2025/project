#' Launch the PM2.5 Shiny App
#'
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import sf
#' @import utils
#' @import ggplot2
#' @export
startApp <- function() {

  # Load the cleaned PM2.5 data
  air_data <- load_pm25_data()

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
