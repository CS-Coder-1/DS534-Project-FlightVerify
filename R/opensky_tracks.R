#' Get flight track by aircraft
#'
#' Retrieves the trajectory for a specific aircraft at a given time.
#'
#' @param icao24 ICAO24 address of the aircraft (6-character hex string).
#' @param time Time in seconds (POSIXct, Date, numeric epoch, or string).
#' @param auth Whether to include authentication token.
#' @return A list with aircraft info and track waypoints as a tibble.
#' @export
get_track_by_aircraft <- function(icao24, time, auth = TRUE) {
  if (missing(icao24) || !nzchar(icao24)) {
    cli::cli_abort("`icao24` is required.")
  }
  
  icao24 <- tolower(trimws(icao24))
  if (!grepl("^[a-f0-9]{6}$", icao24)) {
    cli::cli_abort("`icao24` must be a 6-character hexadecimal string.")
  }
  
  time_epoch <- as.integer(as_epoch(time))
  
  resp <- opensky_request(
    path = "/tracks/all",
    query = list(
      icao24 = icao24,
      time = time_epoch
    ),
    auth = auth
  )
  
  parse_track_response(resp, icao24)
}

parse_track_response <- function(x, icao24) {
  if (is.null(x) || is.null(x$path) || length(x$path) == 0) {
    return(list(
      icao24 = icao24,
      callsign = NA_character_,
      start_time = NA,
      end_time = NA,
      path = tibble::tibble()
    ))
  }
  
  # Parse path waypoints
  path_df <- do.call(rbind, lapply(x$path, function(p) {
    data.frame(
      time = p[1],
      latitude = p[2],
      longitude = p[3],
      baro_altitude = p[4],
      true_track = p[5],
      on_ground = p[6],
      stringsAsFactors = FALSE
    )
  }))
  
  path_df <- tibble::as_tibble(path_df)
  path_df$time <- as.POSIXct(path_df$time, origin = "1970-01-01", tz = "UTC")
  path_df$on_ground <- as.logical(path_df$on_ground)
  
  list(
    icao24 = x$icao24 %||% icao24,
    callsign = trimws(x$callsign %||% NA_character_),
    start_time = as.POSIXct(x$startTime, origin = "1970-01-01", tz = "UTC"),
    end_time = as.POSIXct(x$endTime, origin = "1970-01-01", tz = "UTC"),
    path = path_df
  )
}