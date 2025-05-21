#' Create interactive map of flight path
#'
#' @param flight_path Flight path object created by create_flight_path()
#' @return A leaflet map object
#' @export
plot_flight_map <- function(flight_path) {
  # Transform to WGS84
  line_wgs84 <- sf::st_transform(flight_path$terrain_line, 4326)
  deploy_wgs84 <- sf::st_sfc(sf::st_point(flight_path$deploy_coords),
                             crs = flight_path$parameters$crs) %>%
    sf::st_transform(4326)
  land_wgs84 <- sf::st_sfc(sf::st_point(flight_path$land_coords),
                           crs = flight_path$parameters$crs) %>%
    sf::st_transform(4326)

  sampled_wgs84 <- sf::st_as_sf(sf::st_transform(flight_path$sampled_points, 4326))

  # Create Leaflet map
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines(data = line_wgs84, color = "red", weight = 3) %>%
    leaflet::addCircleMarkers(data = deploy_wgs84, color = "green", radius = 8,
                              popup = "Deployment Point") %>%
    leaflet::addCircleMarkers(data = land_wgs84, color = "red", radius = 8,
                              popup = "Landing Point") %>%
    leaflet::addCircleMarkers(data = sampled_wgs84, color = "blue", radius = 4,
                              popup = paste("Elevation:", sampled_wgs84$Elevation, "m<br>",
                                            "Altitude:", sampled_wgs84$AltitudeAMSL, "m")) %>%
    leaflet::addLegend(colors = c("green", "red", "blue", "orange"),
                       labels = c("Deploy", "Landing", "Sampled Points", "Flight Path"),
                       position = "bottomright")
}
