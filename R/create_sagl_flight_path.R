#' Create a Smart AGL Flight Path
#'
#' @description
#' Generates an optimized 3D flight path that maintains constant height above ground level (AGL)
#' between deployment and landing coordinates. The function uses digital surface model (DSM) data
#' to create terrain-aware trajectories that automatically adjust elevation to maintain safe
#' clearance above varying terrain.
#'
#' @details
#' The function performs the following key operations:
#' \enumerate{
#'   \item Validates input coordinates and parameters
#'   \item Loads and projects DSM data to the specified coordinate system
#'   \item Creates a direct line between deployment and landing points
#'   \item Samples points along the line and extracts terrain elevations
#'   \item Generates an optimized path that maintains constant AGL height
#'   \item Optionally adds calibration patterns at start/end of flight
#'   \item Performs safety checks and provides warnings for potential issues
#' }
#'
#' The output includes both the raw terrain data and optimized flight path, along with
#' visualization-ready components and safety metrics.
#'
#' @param deploy_coords Numeric vector of length 2 containing deployment coordinates (x,y)
#'        in the specified CRS (default UTM Zone 32N)
#' @param land_coords Numeric vector of length 2 containing landing coordinates (x,y)
#'        in the same CRS as deployment coordinates
#' @param dsm_path Character string specifying path to Digital Surface Model (DSM) raster file.
#'        The DSM should cover the entire flight path area.
#' @param N Integer specifying number of sample points along the path (default = 100).
#'        Higher values provide more precise terrain following but increase computation time.
#' @param H Numeric value for desired height above ground level (AGL) in meters (default = 10).
#'        This is the constant clearance maintained above terrain.
#' @param speed_p Numeric value for flight speed to assign to waypoints in m/s (default = 5)
#' @param crs_proj Character string or CRS object specifying the projected coordinate
#'        reference system (default: "EPSG:32632" - UTM Zone 32N)
#' @param minDist Numeric value specifying minimum distance between trajectory points
#'        (enforces UgCS restriction, default = 1 meter)
#' @param calfig_params Optional list of parameters for calibration figure pattern:
#'        \itemize{
#'          \item \code{Lat}: Latitude coordinate for calibration figure center (numeric)
#'          \item \code{Lon}: Longitude coordinate for calibration figure center (numeric)
#'          \item \code{angle}: Initial angle in degrees (default = 90)
#'          \item \code{res}: Number of points per circle (default = 12)
#'          \item \code{diam}: Diameter of circles in meters (default = 15)
#'          \item \code{speed_c}: Speed value for calibration points (default = 5 m/s)
#'          \item \code{H_cal}: Height for calibration points (default = same as main H)
#'        }
#'
#' @return A list object containing:
#' \itemize{
#'   \item \code{deploy_coords}: Original deployment coordinates
#'   \item \code{land_coords}: Original landing coordinates
#'   \item \code{sampled_points}: sf object of sampled terrain points with elevations
#'   \item \code{terrain_line}: LineString of raw terrain profile
#'   \item \code{simple_shift_line}: LineString of naively elevated path (constant offset)
#'   \item \code{smart_agl_line}: Optimized AGL path (simplified LineString)
#'   \item \code{final_coords}: Data frame of final flight coordinates with columns:
#'     \itemize{
#'       \item X, Y, Z: Coordinates
#'       \item Type: Point type ("Trajectory" or "Cal")
#'       \item Speed: Assigned speed in m/s
#'     }
#'   \item \code{dsm_extent}: Polygon of DSM coverage area (WGS84)
#'   \item \code{aoi_kml}: Simplified flight corridor polygon (KML-ready, WGS84)
#'   \item \code{parameters}: List of input parameters used
#' }
#'
#' @section Warning:
#' The function will warn if:
#' \itemize{
#'   \item The flight path exceeds 99 waypoints (DJI limit)
#'   \item The flight path extends beyond DSM coverage
#'   \item Calibration figures are not included when expected
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' flight_path <- create_sagl_flight_path(
#'   deploy_coords = c(580000, 5510000),
#'   land_coords = c(580500, 5510500),
#'   dsm_path = "path/to/dsm.tif",
#'   H = 15
#' )
#'
#' # With calibration figures
#' flight_path <- create_sagl_flight_path(
#'   deploy_coords = c(580000, 5510000),
#'   land_coords = c(580500, 5510500),
#'   dsm_path = "path/to/dsm.tif",
#'   calfig_params = list(Lat = 49.7, Lon = 8.3, diam = 20)
#' )
#' }
#'
#' @seealso
#' For visualization of results, see \code{\link{plot_flight_profile}}. For KML export,
#' use the \code{aoi_kml} output with \code{\link[sf]{st_write}}.
#'
#' @importFrom magrittr %>%
#' @importFrom terra rast project extract vect
#' @importFrom sf st_sfc st_point st_linestring st_coordinates st_line_sample
#' @importFrom sf st_cast st_as_sf st_distance st_segmentize st_buffer
#' @importFrom sf st_boundary st_simplify
#' @importFrom stats approx
#' @importFrom dplyr rename filter arrange
#' @export
create_sagl_flight_path <- function(deploy_coords, land_coords, dsm_path, N = 100, H = 10, speed_p = 5, crs_proj = "EPSG:32632", minDist=1, calfig_params = NULL) {

  # Validate inputs
  validate_inputs(deploy_coords, land_coords, dsm_path, N, H)

  # Load and project DSM
  DSM <- load_dsm(dsm_path, crs_proj)

  # Create sf points and line
  points_and_line <- create_points_and_line(deploy_coords, land_coords, crs_proj)
  deploy_sf <- points_and_line$deploy_sf
  land_sf <- points_and_line$land_sf
  line <- points_and_line$line

  # Create polygon from the raster's extent
  extent_poly <- terra::as.polygons(ext(DSM), crs = crs_proj)

  # Reproject the polygon to WGS84
  extent_poly_wgs84 <- project(extent_poly, "EPSG:4326") %>% sf::st_as_sf()

  # Double check that the line is included within the DSM pn the given projection
  if (!sf::st_intersects(sf::st_as_sf(line), sf::st_as_sf(extent_poly), sparse = FALSE)) {
    stop("The flight path line is not within the DSM extent in the specified projection.")
  }

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
    N = N,
    minDist = minDist,
    speed = speed_p,
    type = "Trajectory"
  )

  # Create temporary line of final trajectory coordinates to make KML
  trajectory_pts <- sf::st_as_sf(final_coords, coords = c("X", "Y"), crs = crs_proj)
  trajectory_line <- trajectory_pts %>%
    sf::st_coordinates() %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs = crs_proj) %>%
    sf::st_sf()

  # Create an area around the trajectoryline to export in the future as KML and project to wgs84
  AOI_KML <- trajectory_line %>%
    sf::st_simplify(dTolerance = 0.01) %>%
    sf::st_buffer(dist = 5, endCapStyle = "SQUARE") %>%
    sf::st_transform(4326)

  # Merge final coordinates from trajectory and from calibration figures
  if (!is.null(calfig_params) && length(calfig_params) > 0) {

    # Get AMSL heith for starting point
    startH <- terra::extract(DSM, matrix(c(calfig_params[["Lat"]], calfig_params[["Lon"]]), ncol = 2))

    # Add startH to the list
    calfig_params$startH <- startH[,1]

    # Make calibration figures (use speed_c to append)
    calfig <- do.call(make_calfig, calfig_params)
    #plot(calfig[, "X"], calfig[, "Y"], type = "b", asp = 1)
    #print(calfig)

    # Convert calfig matrix to dataframe
    calfig_df <- as.data.frame(calfig)

    # Add 'type' column filled with "Cal"
    calfig_df$Type <- "Cal"

    # Append calibration figures at the beginning and at the end of final_coords
    final_coords <- dplyr::bind_rows(calfig_df, final_coords, calfig_df)

    # Remove consecutive duplicates while preserving order
    final_coords <- final_coords[c(TRUE, !(final_coords$X[-1] == final_coords$X[-nrow(final_coords)] &
                                             final_coords$Y[-1] == final_coords$Y[-nrow(final_coords)] &
                                             final_coords$Z[-1] == final_coords$Z[-nrow(final_coords)])), ]
  } else {
  }

  message("Flight parameter summary")
  # Total number of points with warning if >99
  if (nrow(final_coords) > 99) {
    cat(sprintf("\033[31m❗️  Total points in final flight path: %d (exceeds DJI limit of 99), please reduce SamplePoints parameter.\033[0m\n", nrow(final_coords)))
  } else {
    cat(sprintf("\033[32m✅ Total points in final flight path: %d\033[0m\n", nrow(final_coords)))
  }

  # Calibration figure check
  if (any(final_coords$Type == "Cal")) {
    cat("\033[32m✅ Two calibration figures were included in flight path.\033[0m\n")
  } else {
    cat("\033[33m⚠️ Calibration figures were not included in flight path — check `calfig_params()`.\033[0m\n")
  }
  message("\n")

  # Return all components
  list(
    deploy_coords = deploy_coords,
    land_coords = land_coords,
    sampled_points = sampled_pts,
    terrain_line = buffer_results$line_sf,
    simple_shift_line = buffer_results$shift_sf,
    smart_agl_line = upper_line_simplified,
    final_coords = final_coords,
    dsm_extent = extent_poly_wgs84,
    aoi_kml = AOI_KML,
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
