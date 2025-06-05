#' Create a calibration figure using two intersecting circles
#'
#' Generates a flight path consisting of two circles (one clockwise, one counter-clockwise)
#' that intersect at the given coordinates. The path starts and ends at the intersection point.
#'
#' @param Lat Latitude coordinate of the intersection point (numeric)
#' @param Lon Longitude coordinate of the intersection point (numeric)
#' @param diam Diameter of the circles in meters (numeric, default = 15)
#' @param angle Initial angle in degrees (0-360) for the first circle's orientation (numeric, default = 90)
#' @param speed_c Speed value to assign to all points (numeric, default = 5)
#' @param res Number of points to generate per circle (minimum 4) (integer, default = 10)
#' @param H Height value to assign to all points (numeric, default = 80)
#' @param startH Height value of the DSM point (numeric, default = -500)
#'
#' @return A matrix containing the flight path coordinates and parameters with columns:
#' \itemize{
#'   \item Lat - Latitude coordinates
#'   \item Lon - Longitude coordinates
#'   \item speed - Speed values
#'   \item H - Height values
#' }
#'
#' @examples
#' # Create calibration figure
#' calfig <- make_calfig(Lat = 570524.16, Lon = 5513700.11, angle = 45)
#'
#' # Plot the result
#' plot(calfig[, "Lon"], calfig[, "Lat"], type = "b", asp = 1)
#' points(570524.16, 5513700.11, col = "red", pch = 19)
#'
#' @export
make_calfig <- function(Lat, Lon, diam = 15, angle = 90, speed_c = 5, res = 10, H = 80, startH = -500) {

  # Assign the final height AMSL
  finalH <- startH + H

  # Create angle at +90° for second calibration figure
  angleS <- angle + 90

  # Ensure resolution is at least 4
  res <- max(res, 4)

  # Calculate radius from diameter
  radius <- diam / 2

  # Convert angle from degrees to radians
  angle_rad <- angle * pi / 180
  opposite_angle_rad <- (angle + 180) * pi / 180

  # Calculate circle centers along the angle direction
  center1 <- c(
    Lat + radius * cos(angle_rad),
    Lon + radius * sin(angle_rad)
  )

  center2 <- c(
    Lat + radius * cos(opposite_angle_rad),
    Lon + radius * sin(opposite_angle_rad)
  )

  # Generate points for first circle (clockwise)
  angles_cw <- seq(angle_rad + pi, angle_rad - pi, length.out = res + 1)[1:res]

  circle1 <- t(sapply(angles_cw, function(a) {
    c(center1[1] + radius * cos(a),
      center1[2] + radius * sin(a),
      finalH, speed_c)
  }))

  # Generate points for second circle (counter-clockwise)
  angles_ccw <- seq(opposite_angle_rad - pi, opposite_angle_rad + pi, length.out = res + 1)[1:res]

  circle2 <- t(sapply(angles_ccw, function(a) {
    c(center2[1] + radius * cos(a),
      center2[2] + radius * sin(a),
      finalH, speed_c)
  }))

  # Combine all points in the correct order:
  points <- rbind(
    c(Lat, Lon, finalH, speed_c),  # Starting point
    circle1,                # First circle
    c(Lat, Lon, finalH, speed_c), # Return to start
    circle2,               # Second circle
    c(Lat, Lon, finalH, speed_c)  # Final point
  )

  colnames(points) <- c("X", "Y", "Z", "Speed")
  rownames(points) <- NULL

  return(points)
}
