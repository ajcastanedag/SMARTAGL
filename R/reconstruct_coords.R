#' Reconstruct projected coordinates from distance-elevation data
#'
#' @param shifted_df Data frame with distance and elevation columns
#' @param deploy_coords Deployment coordinates
#' @param land_coords Landing coordinates
#' @param N Number of output points
#' @return Data frame with X, Y, Z coordinates
#' @keywords internal
reconstruct_coords <- function(shifted_df, deploy_coords, land_coords, N) {
  original_line <- sf::st_sfc(sf::st_linestring(rbind(deploy_coords, land_coords)))
  total_length <- as.numeric(sf::st_length(original_line))

  shifted_df$frac <- shifted_df$distance / max(shifted_df$distance)

  interp_x <- approxfun(
    x = c(0, total_length),
    y = c(deploy_coords[1], land_coords[1]),
    method = "linear")

  interp_y <- approxfun(
    x = c(0, total_length),
    y = c(deploy_coords[2], land_coords[2]),
    method = "linear")

  shifted_df$X <- interp_x(shifted_df$distance)
  shifted_df$Y <- interp_y(shifted_df$distance)

  if (nrow(shifted_df) > N) {
    sampled_indices <- seq(1, nrow(shifted_df), length.out = N) %>% round()
    result <- shifted_df[sampled_indices, c("X", "Y", "Elevation")]
  } else {
    result <- shifted_df[, c("X", "Y", "Elevation")]
  }

  names(result)[3] <- "Z"
  return(result)
}
