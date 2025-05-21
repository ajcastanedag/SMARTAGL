# Package utility functions

#' Check if required packages are installed
#' @keywords internal
check_packages <- function() {
  required <- c("sf", "terra", "dplyr", "ggplot2", "leaflet", "units", "purrr", "magrittr")
  missing <- setdiff(required, rownames(installed.packages()))

  if (length(missing) > 0) {
    stop("Missing required packages: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}

#' Package startup message
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("SMARTAGL package loaded - for smart above ground level flight planning")
}
