#' Get current aircraft states
#'
#' Retrieves state vectors for all aircraft currently tracked by OpenSky.
#' Can be filtered by time, ICAO24 addresses, or bounding box.
#'
#' @param time Optional time in seconds (POSIXct, Date, numeric epoch, or string).
#'   If NULL, retrieves current states.
#' @param icao24 Optional character vector of ICAO24 addresses to filter.
#' @param bbox Optional bounding box as named list: list(min_lat, max_lat, min_lon, max_lon).
#' @param auth Whether to include authentication token.
#' @return A tibble of aircraft state vectors.
#' @export
get_states <- function(time = NULL, icao24 = NULL, bbox = NULL, auth = TRUE) {
  query <- list()
  
  if (!is.null(time)) {
    query$time <- as.integer(as_epoch(time))
  }
  
  if (!is.null(icao24)) {
    if (!is.character(icao24)) {
      cli::cli_abort("`icao24` must be a character vector.")
    }
    query$icao24 <- paste(icao24, collapse = ",")
  }
  
  if (!is.null(bbox)) {
    if (!is.list(bbox) || !all(c("min_lat", "max_lat", "min_lon", "max_lon") %in% names(bbox))) {
      cli::cli_abort("`bbox` must be a named list with min_lat, max_lat, min_lon, max_lon.")
    }
    query$lamin <- bbox$min_lat
    query$lamax <- bbox$max_lat
    query$lomin <- bbox$min_lon
    query$lomax <- bbox$max_lon
  }
  
  resp <- opensky_request(
    path = "/states/all",
    query = query,
    auth = auth
  )
  
  parse_states_response(resp)
}

#' Get states for your own sensors (authenticated users only)
#'
#' @inheritParams get_states
#' @return A tibble of aircraft state vectors from your sensors.
#' @export
get_own_states <- function(time = NULL, icao24 = NULL, auth = TRUE) {
  if (!isTRUE(auth)) {
    cli::cli_abort("`get_own_states()` requires authentication.")
  }
  
  query <- list()
  
  if (!is.null(time)) {
    query$time <- as.integer(as_epoch(time))
  }
  
  if (!is.null(icao24)) {
    query$icao24 <- paste(icao24, collapse = ",")
  }
  
  resp <- opensky_request(
    path = "/states/own",
    query = query,
    auth = TRUE
  )
  
  parse_states_response(resp)
}