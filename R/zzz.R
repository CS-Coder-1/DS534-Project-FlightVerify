#' Package environment for storing authentication state
#' @keywords internal
#' @noRd
.opensky_env <- new.env(parent = emptyenv())

#' @noRd
.onLoad <- function(libname, pkgname) {
  .opensky_env$username <- NULL
  .opensky_env$password <- NULL
}