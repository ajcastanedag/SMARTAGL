#' Export flight path to CSV format
#'
#' Converts a flight path object to CSV format suitable for drone flight controllers,
#' with options to include/exclude headers and set flight speed. The function handles
#' coordinate transformation to WGS84 (EPSG:4326) automatically.
#'
#' @param flight_path Flight path object created by `create_flight_path()`, containing:
#' \itemize{
#'   \item `final_coords` - Data frame with X, Y, Z coordinates
#'   \item `parameters$crs` - Original coordinate reference system
#' }
#' @param output_path Path where the CSV file should be saved (character)
#' @param include_headers Logical indicating whether to include column headers in output
#' (default = TRUE). When FALSE, outputs only Latitude, Longitude, Altitude columns.
#' @param speed Flight speed to assign to all waypoints in m/s (numeric, default = 5)
#'
#' @return Invisibly returns NULL. Writes CSV file to specified path with columns:
#' \itemize{
#'   \item WP - Waypoint number
#'   \item Latitude - WGS84 latitude (decimal degrees)
#'   \item Longitude - WGS84 longitude (decimal degrees)
#'   \item AltitudeAMSL - Altitude above mean sea level (meters)
#'   \item Speed - Flight speed (m/s)
#'   \item Picture - Logical for camera trigger (always FALSE in current implementation)
#'   \item UavYaw - Drone yaw angle (currently unused)
#'   \item CameraTilt - Gimbal tilt angle (currently unused)
#'   \item WaitTime - Waypoint hold time (currently unused)
#' }
#' When `include_headers = FALSE`, outputs only Latitude, Longitude, Altitude columns.
#'
#' @examples
#' \dontrun{
#' # Create flight path first
#' path <- create_flight_path(...)
#'
#' # Export with headers
#' export_flight_path(path, "flight_plan.csv")
#'
#' # Export without headers for simple controllers
#' export_flight_path(path, "minimal_plan.csv", include_headers = FALSE)
#' }
#'
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom dplyr mutate row_number select
#' @importFrom readr write_csv
#' @importFrom utils write.table
#' @export
export_flight_path <- function(flight_path, output_path, include_headers = TRUE, speed = 5) {

  # Transform coordinates to WGS84
  final_coords_sf <- sf::st_as_sf(
    flight_path$final_coords,
    coords = c("X", "Y"),
    crs = flight_path$parameters$crs
  ) %>%
    sf::st_transform(4326)

  # Prepare CSV data
  csv_data <- final_coords_sf %>%
    dplyr::mutate(
      Longitude = sf::st_coordinates(.)[,1],
      Latitude = sf::st_coordinates(.)[,2],
      AltitudeAMSL = .data$Z,
      Speed = speed,
      WP = dplyr::row_number(),
      Picture = FALSE,
      UavYaw = "",                                                              # Temporary disabled
      CameraTilt = "",                                                          # Temporary disabled
      WaitTime = ""                                                             # Temporary disabled
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      .data$WP, .data$Latitude, .data$Longitude, .data$AltitudeAMSL, .data$Speed,
      .data$Picture, .data$UavYaw, .data$CameraTilt, .data$WaitTime
    )

  # Export with or without headers
  if (include_headers) {
    readr::write_csv(csv_data, output_path)
  } else {
    utils::write.table(
      csv_data[, c("Latitude", "Longitude", "AltitudeAMSL")],
      output_path,
      col.names = FALSE,
      row.names = FALSE,
      sep = ","
    )
  }

  invisible(NULL)
}
