#' Plot flight path visualization
#'
#' @param flight_path Flight path object created by create_flight_path()
#' @return A ggplot object showing the flight path comparison
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
