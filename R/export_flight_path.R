#' Export flight path to CSV
#'
#' @param flight_path Flight path object created by create_flight_path()
#' @param output_path Path to save the CSV file
#' @param include_headers Logical, whether to include column headers (default TRUE)
#' @param speed Trajectory speed (default = 5)
#' @return Invisible NULL, writes file to disk
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
