#' Create interactive map of flight path
#'
#' @param flight_path Flight path object created by create_flight_path()
#' @return A leaflet map object
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

    leaflet::addLegend(colors = c("darkgreen", "black", "#ff8c00"),
                         labels = c("Start", "Flight Path", "End"),
                         position = "bottomright")

  return(leaflet_map)

}
