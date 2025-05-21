#' Create a smart AGL flight path
#'
#' @param deploy_coords Numeric vector of length 2 with deployment coordinates (x,y)
#' @param land_coords Numeric vector of length 2 with landing coordinates (x,y)
#' @param dsm_path Path to Digital Surface Model (DSM) raster file
#' @param N Number of sample points along the path (default = 100)
#' @param H Desired height above ground level (AGL) in meters (default = 10)
#' @param crs_proj Projected CRS (default is UTM Zone 32N - EPSG:32632)
#' @return A list containing flight path components
#' @importFrom magrittr %>%
#' @export
create_flight_path <- function(deploy_coords, land_coords, dsm_path, N = 100, H = 10, crs_proj = "EPSG:32632") {

  # Validate inputs
  validate_inputs(deploy_coords, land_coords, dsm_path, N, H)

  # Load and project DSM
  DSM <- load_dsm(dsm_path, crs_proj)

  # Create sf points and line
  points_and_line <- create_points_and_line(deploy_coords, land_coords, crs_proj)
  deploy_sf <- points_and_line$deploy_sf
  land_sf <- points_and_line$land_sf
  line <- points_and_line$line

  # Sample points along line
  sampled_pts <- sample_points(line, N)

  # Extract elevations
  sampled_pts <- extract_elevations(sampled_pts, DSM)
  sampled_pts$AltitudeAMSL <- sampled_pts$Elevation + H

  # Create sampled dataframe with distances
  sampled_df <- create_sampled_df(sampled_pts, deploy_sf)

  # Create buffer and upper line
  buffer_results <- create_buffer(sampled_df, H)
  upper_line_simplified <- buffer_results$upper_line_simplified
  filtered_points <- buffer_results$filtered_points

  # Reconstruct final coordinates
  final_coords <- reconstruct_coords(
    shifted_df = filtered_points,
    deploy_coords = deploy_coords,
    land_coords = land_coords,
    N = N
  )

  # Return all components
  list(
    deploy_coords = deploy_coords,
    land_coords = land_coords,
    sampled_points = sampled_pts,
    terrain_line = buffer_results$line_sf,
    simple_shift_line = buffer_results$shift_sf,
    smart_agl_line = upper_line_simplified,
    final_coords = final_coords,
    parameters = list(N = N, H = H, crs = crs_proj)
  )
}

# Helper functions (not exported)
validate_inputs <- function(deploy_coords, land_coords, dsm_path, N, H) {
  stopifnot(
    length(deploy_coords) == 2,
    length(land_coords) == 2,
    is.numeric(N), N > 0,
    is.numeric(H), H >= 0,
    file.exists(dsm_path))
}

load_dsm <- function(dsm_path, crs_proj) {
  DSM <- terra::rast(dsm_path)
  terra::project(DSM, crs_proj)
}

create_points_and_line <- function(deploy_coords, land_coords, crs_proj) {
  deploy_sf <- sf::st_sfc(sf::st_point(deploy_coords), crs = crs_proj)
  land_sf <- sf::st_sfc(sf::st_point(land_coords), crs = crs_proj)
  line <- sf::st_sfc(
    sf::st_linestring(rbind(
      sf::st_coordinates(deploy_sf),
      sf::st_coordinates(land_sf))),
    crs = crs_proj)

  list(deploy_sf = deploy_sf, land_sf = land_sf, line = line)
}

sample_points <- function(line, N) {
  sf::st_line_sample(line, n = N, type = "regular") %>%
    sf::st_cast("POINT") %>%
    sf::st_as_sf()
}

extract_elevations <- function(sampled_pts, DSM) {
  sampled_pts$Elevation <- terra::extract(DSM, terra::vect(sampled_pts))[, 2]
  sampled_pts
}

create_sampled_df <- function(sampled_pts, deploy_sf) {
  sampled_df <- sampled_pts
  sampled_df$distance <- as.numeric(sf::st_distance(deploy_sf, sampled_pts)[1, ])
  sampled_df
}

create_buffer <- function(sampled_df, H) {
  coords_T <- cbind(as.numeric(sampled_df$distance), as.numeric(sampled_df$Elevation))
  coords_S <- cbind(as.numeric(sampled_df$distance), as.numeric(sampled_df$Elevation) + H)

  line_sf <- sf::st_linestring(coords_T) %>% sf::st_sfc()
  shift_sf <- sf::st_linestring(coords_S) %>% sf::st_sfc()
  densified_line <- sf::st_segmentize(line_sf, dfMaxLength = 0.1)
  buffered <- sf::st_buffer(densified_line, dist = H, endCapStyle = "ROUND", joinStyle = "ROUND")
  boundary <- sf::st_boundary(buffered)
  boundary_points <- sf::st_cast(boundary, "POINT") %>% sf::st_as_sf()

  boundary_coords <- sf::st_coordinates(boundary_points) %>%
    as.data.frame() %>%
    dplyr::rename(distance = X, Elevation = Y)

  original_elevation <- approx(
    x = sampled_df$distance,
    y = sampled_df$Elevation,
    xout = boundary_coords$distance,
    rule = 2)$y

  filtered_points <- boundary_coords %>%
    dplyr::filter(
      distance >= 0,
      distance <= max(sampled_df$distance),
      Elevation >= original_elevation) %>%
    dplyr::arrange(distance)

  upper_line <- sf::st_linestring(as.matrix(filtered_points[, c("distance", "Elevation")]))
  upper_line_sf <- sf::st_sfc(upper_line)
  upper_line_simplified <- sf::st_simplify(upper_line_sf, dTolerance = 0.1)

  list(
    line_sf = line_sf,
    shift_sf = shift_sf,
    upper_line_simplified = upper_line_simplified,
    filtered_points = filtered_points
  )
}
