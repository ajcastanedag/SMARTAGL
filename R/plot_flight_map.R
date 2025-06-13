#' Create Interactive Flight Path Map
#'
#' @description
#' Generates an interactive Leaflet map visualizing flight path components including:
#' \itemize{
#'   \item Full flight trajectory colored by altitude
#'   \item Start/end points with distinct markers
#'   \item DSM coverage area and flight corridor
#' }
#'
#' @details
#' The map provides these interactive elements:
#' \itemize{
#'   \item Clickable points showing coordinate information
#'   \item Color-coded altitude visualization (using YlOrRd palette)
#'   \item Clear visual distinction between different path components
#'   \item Automatic zoom to fit all elements
#' }
#'
#' @param flight_path A flight path object created by \code{\link{create_sagl_flight_path}}
#'        containing these required elements:
#'        \itemize{
#'          \item \code{final_coords}: Data frame with columns (X, Y, Z)
#'          \item \code{deploy_coords}: Numeric vector (x,y) of start position
#'          \item \code{land_coords}: Numeric vector (x,y) of end position
#'          \item \code{parameters$crs}: Original coordinate reference system
#'          \item \code{dsm_extent}: Polygon of DSM coverage area (sf object)
#'          \item \code{aoi_kml}: Flight corridor polygon (sf object)
#'        }
#'
#' @return An interactive Leaflet map object with:
#' \itemize{
#'   \item \strong{Markers}:
#'     \itemize{
#'       \item Green - Deployment point
#'       \item Orange - Landing point
#'     }
#'   \item \strong{Polygons}:
#'     \itemize{
#'       \item Gray - DSM coverage area (20\% opacity)
#'       \item Red - Flight corridor (20\% opacity)
#'     }
#'   \item \strong{Path}:
#'     \itemize{
#'       \item Colored points - Altitude gradient (YlOrRd palette)
#'       \item Black dashed line - Direct start-end connection
#'     }
#'   \item \strong{Interactive Features}:
#'     \itemize{
#'       \item Popups with coordinate details on click
#'       \item Legend identifying all map elements
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # First create flight path
#' flight_data <- create_sagl_flight_path(
#'   deploy_coords = c(580000, 5510000),
#'   land_coords = c(580500, 5510500),
#'   dsm_path = "path/to/dsm.tif"
#' )
#'
#' # Generate and display map
#' flight_map <- plot_flight_map(flight_data)
#' flight_map
#'
#' # Save to HTML
#' htmlwidgets::saveWidget(flight_map, "flight_path_map.html")
#' }
#'
#' @seealso
#' Related functions:
#' \itemize{
#'   \item \code{\link{plot_flight_path}} for 2D profile view
#'   \item \code{\link{create_sagl_flight_path}} to generate required input
#'   \item \code{\link[leaflet]{leaflet}} for advanced map customization
#' }
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers addPolylines colorNumeric addLegend addPolygons
#' @importFrom sf st_as_sf st_transform st_sfc st_point st_geometry st_bbox st_coordinates
#' @export
plot_flight_map <- function(flight_path) {

  # Transform coordinates to WGS84
  coords_sf <- sf::st_as_sf(
    flight_path$final_coords,
    coords = c("X", "Y"),
    crs = flight_path$parameters$crs
  )

  # Project coordinates to WGS84
  final_coords_sf <- coords_sf %>% sf::st_transform(4326)

  # Extract and Transform Deploy coordinates to WGS84
  deploy_wgs84 <- sf::st_sfc(sf::st_point(flight_path$deploy_coords),
                             crs = flight_path$parameters$crs) %>%
    sf::st_transform(4326)

  # Extract and Transform Land coordinates to WGS84
  land_wgs84 <- sf::st_sfc(sf::st_point(flight_path$land_coords),
                           crs = flight_path$parameters$crs) %>%
    sf::st_transform(4326)

  # Combine all spatial objects for bounding box calculation
  all_points <- c(
    sf::st_geometry(final_coords_sf),
    deploy_wgs84,
    land_wgs84
  )

  # Calculate bounding box
  bbox <- sf::st_bbox(all_points)

  # # Create Leaflet map
  leaflet_map <-  leaflet::leaflet() %>%
    leaflet::addTiles() %>%

    # Add the DSM extention for reference
    leaflet::addPolygons(data = flight_path$dsm_extent,stroke = F, fillOpacity = 0.2, fillColor = "black") %>%

    # Add the DSM extention for reference
    leaflet::addPolygons(data = flight_path$aoi_kml, stroke = F, fillOpacity = 0.2, fillColor = "red") %>%

    # Add start and end points
    leaflet::addCircleMarkers(data = deploy_wgs84, color = "darkgreen", radius = 8,
                              stroke = FALSE,
                              fillOpacity = 1,
                              popup = "Start Point") %>%

    leaflet::addCircleMarkers(data = land_wgs84, color = "#ff8c00", radius = 8,
                              stroke = FALSE,
                              fillOpacity = 1,
                              popup = "End Point")  %>%

    # Add line between start and end points
    leaflet::addPolylines(
      lng = c(sf::st_coordinates(deploy_wgs84)[1], sf::st_coordinates(land_wgs84)[1]),
      lat = c(sf::st_coordinates(deploy_wgs84)[2], sf::st_coordinates(land_wgs84)[2]),
      color = "black",
      weight = 2,
      dashArray = "5, 5"
    ) %>%

    # # Add flight path points with color based on SAGL (Z coordinate)
    leaflet::addCircleMarkers(
      data = final_coords_sf,
      radius = 5,
      stroke = FALSE,
      color = ~leaflet::colorNumeric("YlOrRd", domain = final_coords_sf$Z)(Z), # Color by Z value
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>Flight Point</b><br>",
        "<b>X:</b> ", round(sf::st_coordinates(coords_sf)[,1], 2), "<br>",
        "<b>Y:</b> ", round(sf::st_coordinates(coords_sf)[, 2], 2), "<br>",
        "<b>SAGL:</b> ", paste0(round(Z, 2), " m"))
    ) %>%

    leaflet::addLegend(colors = c("darkgreen", "red", "#ff8c00", "black"),
                         labels = c("Start", "AOI", "End", "DSM"),
                         position = "bottomright")

  return(leaflet_map)

}
