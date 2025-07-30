#' Load and Clean PM2.5 Data
#'
#' This function reads and cleans PM2.5 air quality data from a CSV file
#' bundled with the package. It assigns placeholder coordinates and filters out missing data.
#'
#' @return A data frame of cleaned PM2.5 observations.
#' @export
load_pm25_data <- function() {
  csv_path <- system.file("extdata", "openaq.csv", package = "appforair")

  if (csv_path == "") {
    stop("File 'openaq.csv' not found. Make sure it exists in inst/extdata/ before building the package.")
  }

  data <- read.csv(csv_path, sep = ",", stringsAsFactors = FALSE)

  # Rename PM2.5 column
  names(data)[4] <- "PM25"

  # Basic cleaning
  data <- data[!is.na(data$PM25) & data$PM25 != "", ]
  data$PM25 <- as.numeric(data$PM25)
  data$Year <- as.numeric(data$Year)

  # Generate dummy location info for mapping
  set.seed(123)
  data$City <- data$Entity
  data$Latitude <- jitter(rep(48.85, nrow(data)))
  data$Longitude <- jitter(rep(2.35, nrow(data)))

  return(data)
}
