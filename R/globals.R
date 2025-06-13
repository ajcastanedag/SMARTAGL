#' @title UgCS Flight Planning Toolkit
#' @name SMARTAGL
#' @description
#' Provides an intermediate planning layer for complex flight operations destined for UgCS execution.
#'
#' @details
#' This package serves as a specialized planning environment for flights requiring:
#' \itemize{
#'   \item Advanced terrain-following capabilities
#'   \item Precise sensor geometry control
#'   \item Custom flight path optimization
#'   \item Detailed pre-flight visualization
#' }
#'
#' Designed to generate flight plans that can be exported to UgCS for final execution.
#'
#' Available functions:
#' \itemize{
#'   \item \code{\link{create_angled_flight_path}}: Creates terrain-following path with constant sensor angle
#'   \item \code{\link{create_sagl_flight_path}}: Generates true constant AGL flight path
#'   \item \code{\link{plot_flight_map}}: Visualizes flight path on 2D map
#'   \item \code{\link{plot_flight_path}}: Displays 3D flight path profile
#'   \item \code{\link{export_flight_path}}: Exports flight plan to UgCS-compatible format
#' }
#'
#' @seealso
#' Useful resources:
#' \itemize{
#'   \item UgCS official documentation: \url{https://www.ugcs.com}
#' }
NULL
