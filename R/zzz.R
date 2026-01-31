# Package environment for auth state
.opensky_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .opensky_env$access_token <- NULL
  .opensky_env$token_expires_at <- as.POSIXct(0, tz = "UTC")
  .opensky_env$client_id <- NULL
  .opensky_env$client_secret <- NULL
  .opensky_env$token_url <- NULL
  .opensky_env$scope <- NULL
}
