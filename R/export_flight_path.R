#' Export Flight Path to CSV/KML Formats
#'
#' @description
#' Exports flight path data to industry-standard formats for drone operations, with automatic
#' coordinate transformation to WGS84 (EPSG:4326). Creates organized output folders with
#' version control to prevent accidental overwrites.
#'
#' @details
#' Key features:
#' \itemize{
#'   \item Automatic coordinate transformation to WGS84 (Lat/Lon)
#'   \item Versioned output folders (0_FlightPath_H_angle, 1_FlightPath_H_angle, etc.)
#'   \item Multiple export options (full CSV headers or minimal waypoint format)
#'   \item Optional KML export for visualization in GIS software
#'   \item Clear console feedback about export status
#' }
#'
#' @param flight_path Flight path object created by \code{\link{create_sagl_flight_path}} containing:
#'        \itemize{
#'          \item \code{final_coords}: Data frame with X, Y, Z coordinates and Speed
#'          \item \code{parameters$crs}: Original coordinate reference system
#'          \item \code{parameters$H}: Flight height parameter (for folder naming)
#'          \item \code{parameters$angle}: Sensor angle (for folder naming)
#'          \item \code{aoi_kml}: Flight corridor polygon (for KML export)
#'        }
#' @param output_path Character string specifying directory path for output files
#' @param include_headers Logical indicating whether to include full column headers (default = TRUE).
#'        When FALSE, exports minimal 3-column format (Latitude, Longitude, Altitude).
#' @param exportkml Logical indicating whether to create additional KML file (default = FALSE)
#'
#' @return Invisibly returns NULL. Creates these output files:
#' \itemize{
#'   \item \strong{CSV File} containing:
#'     \itemize{
#'       \item Full format (when \code{include_headers = TRUE}):
#'         \itemize{
#'           \item WP: Waypoint number
#'           \item Latitude, Longitude: WGS84 coordinates
#'           \item AltitudeAMSL: Elevation in meters
#'           \item Speed: Flight speed (m/s)
#'           \item Picture: Camera trigger flag
#'           \item UavYaw, CameraTilt, WaitTime: Reserved for future use
#'         }
#'       \item Minimal format (when \code{include_headers = FALSE}):
#'         Only Latitude, Longitude, Altitude columns
#'     }
#'   \item \strong{KML File} (when \code{exportkml = TRUE}):
#'     Flight corridor polygon for visualization in Google Earth/QGIS
#' }
#'
#' @section Folder Structure:
#' Creates automatically numbered folders to prevent overwrites:
#' \preformatted{
#' output_path/
#'   0_FlightPath_H_angle/
#'     └── 0_FlightPath_H_angle.csv
#'     └── 0_FlightPath_H_angle.kml (if exportkml=TRUE)
#'   1_FlightPath_H_angle/ (if 0_ exists)
#'   ...
#' }
#'
#' @examples
#' \dontrun{
#' # Create flight path first
#' fp <- create_sagl_flight_path(
#'   deploy_coords = c(580000, 5510000),
#'   land_coords = c(580500, 5510500),
#'   dsm_path = "path/to/dsm.tif",
#'   H = 50,
#'   angle = 30
#' )
#'
#' # Export to default CSV with headers
#' export_flight_path(fp, "output/flight_plans")
#'
#' # Export minimal format without headers
#' export_flight_path(fp, "output/minimal_plans", include_headers = FALSE)
#'
#' # Export both CSV and KML
#' export_flight_path(fp, "output/full_export", exportkml = TRUE)
#' }
#'
#' @seealso
#' Related functions:
#' \itemize{
#'   \item \code{\link{create_sagl_flight_path}} to generate exportable flight paths
#'   \item \code{\link{plot_flight_map}} for visual verification before export
#' }
#'
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry st_write
#' @importFrom dplyr mutate row_number select
#' @importFrom readr write_csv
#' @importFrom utils write.table
#' @export
export_flight_path <- function(flight_path, output_path, include_headers = TRUE, exportkml = FALSE) {

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
      Speed = .data$Speed,
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

  # Generate base folder name from parameters
  base_name <- paste0("FlightPath_", flight_path$parameters$H, "_", flight_path$parameters$angle)

  # Check if output_path exists
  if (!dir.exists(output_path)) {
    cat(sprintf("\033[31m❌ Output path does not exist: '%s'\033[0m\n", output_path))
    stop("Aborting: output_path not found.")
  }

  # Start naming from 0_
  counter <- 0
  folder_name <- paste0(counter, "_", base_name)
  target_path <- file.path(output_path, folder_name)

  # If folder exists, increment prefix and warn about overwrite
  while (dir.exists(target_path)) {
    if (counter == 0) {
      cat(sprintf("\033[33m⚠️  Folder '%s' already exists, creating a new version...\033[0m\n", folder_name))
    }
    counter <- counter + 1
    folder_name <- paste0(counter, "_", base_name)
    target_path <- file.path(output_path, folder_name)
  }

  # Create the directory
  dir.create(target_path, recursive = TRUE)

  # Inform the user
  cat(sprintf("\033[32m✅ Created output folder: %s\033[0m\n", target_path))

  # === EXPORT KML ===
  if (exportkml) {
    kml_filename <- paste0(folder_name, ".kml")  # Match KML to folder name
    kml_path <- file.path(target_path, kml_filename)

    suppressMessages(
      suppressWarnings(
        sf::st_write(
          flight_path$aoi_kml,
          dsn = kml_path,
          driver = "KML",
          delete_dsn = TRUE,
          quiet = TRUE
        )
      )
    )

    if (file.exists(kml_path)) {
      cat(sprintf("\033[32m✅ AOI KML exported to: %s\033[0m\n", kml_path))
    } else {
      cat(sprintf("\033[31m❌ AOI KML export failed at: %s\033[0m\n", kml_path))
    }
  }

  # === EXPORT CSV ===
  csv_filename <- paste0(folder_name, ".csv")  # Match CSV to folder name
  csv_path <- file.path(target_path, csv_filename)

  if (include_headers) {
    readr::write_csv(csv_data, csv_path)
    cat(sprintf("\033[32m✅ CSV with headers exported to: %s\033[0m\n", csv_path))
  } else {
    utils::write.table(
      csv_data[, c("Latitude", "Longitude", "AltitudeAMSL")],
      csv_path,
      col.names = FALSE,
      row.names = FALSE,
      sep = ","
    )
    cat(sprintf("\033[32m✅ CSV without headers exported to: %s\033[0m\n", csv_path))
  }

  invisible(NULL)
}
