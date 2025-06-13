#' Visualize Flight Path Comparison
#'
#' @description
#' Generates a comparative plot of different flight path versions against terrain profile.
#' Shows the original terrain, simple elevation-shifted path, and optimized SMARTAGL path.
#'
#' @details
#' Creates a 2D profile view comparing:
#' \itemize{
#'   \item \strong{Terrain}: Raw ground elevation profile (black solid line)
#'   \item \strong{Simple Shift}: Naive elevation shift (red dashed line)
#'   \item \strong{Smart AGL}: Optimized terrain-following path (orange solid line)
#' }
#'
#' @param flight_path Flight path object created by \code{\link{create_sagl_flight_path}}
#'        containing these elements:
#'        \itemize{
#'          \item \code{terrain_line}: Raw terrain profile (sf LINESTRING)
#'          \item \code{simple_shift_line}: Simple elevation-shifted path (sf LINESTRING)
#'          \item \code{smart_agl_line}: Optimized SMARTAGL path (sf LINESTRING)
#'        }
#'
#' @return A ggplot2 object showing the flight path comparison with:
#'         \itemize{
#'           \item Distance from deployment on x-axis (meters)
#'           \item Elevation/height on y-axis (meters)
#'           \item Automatic legend and proper axis labels
#'         }
#'
#' @examples
#' \dontrun{
#' # Create flight path first
#' fp <- create_sagl_flight_path(
#'   deploy_coords = c(580000, 5510000),
#'   land_coords = c(580500, 5510500),
#'   dsm_path = "path/to/dsm.tif"
#' )
#'
#' # Visualize the comparison
#' plot_flight_path(fp)
#'
#' # Customize the plot
#' library(ggplot2)
#' plot_flight_path(fp) +
#'   ggtitle("Custom Title") +
#'   theme(legend.position = "top")
#' }
#'
#' @seealso
#' Related plotting functions:
#' \itemize{
#'   \item \code{\link{plot_flight_map}} for 2D map view
#'   \item \code{\link{create_sagl_flight_path}} to generate input data
#' }
#'
#' @importFrom ggplot2 ggplot geom_sf scale_color_manual coord_sf labs theme_minimal theme
#' @export
plot_flight_path <- function(flight_path) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = flight_path$terrain_line,
                     ggplot2::aes(color = "Terrain"), linewidth = 1) +
    ggplot2::geom_sf(data = flight_path$simple_shift_line,
                     ggplot2::aes(color = "Simple Shift"), linetype = "dashed") +
    ggplot2::geom_sf(data = flight_path$smart_agl_line,
                     ggplot2::aes(color = "Smart AGL"), linewidth = 1) +
    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "Terrain" = "black",
        "Simple Shift" = "red",
        "Smart AGL" = "orange"
      )
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title = "Smart AGL Flight Transect",
      x = "Distance from deploy spot (m)",
      y = "Height (m)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.justification = "right"
    )
}
