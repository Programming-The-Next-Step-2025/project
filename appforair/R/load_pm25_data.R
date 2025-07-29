#' Load and Clean PM2.5 Data
#'
#' This function reads and cleans PM2.5 air quality data from a CSV file
#' bundled with the package. It filters out invalid city entries and ensures
#' cities have multiple observations and time points.
#'
#' @return A data frame of cleaned PM2.5 observations.
#' @examples
#' \dontrun{
#'   pm25_data <- load_pm25_data()
#' }
#' @export
load_pm25_data <- function() {
  csv_path <- system.file("extdata", "openaq.csv", package = "appforair")

  if (csv_path == "") {
    stop("File 'openaq.csv' not found. Make sure it exists in inst/extdata/ before building the package.")
  }

  data <- read.csv(csv_path, sep = ";", stringsAsFactors = FALSE)

  coords <- strsplit(data$Coordinates, ",")
  data$Latitude <- sapply(coords, function(x) as.numeric(x[1]))
  data$Longitude <- sapply(coords, function(x) as.numeric(x[2]))
  data$LastUpdated <- as.POSIXct(data$Last.Updated, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  pm25 <- data[data$Pollutant == "PM2.5" & !is.na(data$City) & data$City != "", ]
  pm25 <- pm25[grepl("[A-Za-z]", pm25$City), ]

  city_counts <- table(pm25$City)
  multi_obs <- names(city_counts[city_counts > 1])
  multi_time <- aggregate(LastUpdated ~ City, pm25, function(x) length(unique(x)))
  valid_cities <- intersect(multi_obs, multi_time$City[multi_time$LastUpdated > 1])

  pm25[pm25$City %in% valid_cities, ]
}
