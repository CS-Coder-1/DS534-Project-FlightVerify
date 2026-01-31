`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

opensky_base_url <- function() {
  getOption("flightverify.opensky_base_url", "https://opensky-network.org/api")
}

opensky_token_url <- function() {
  getOption("flightverify.opensky_token_url", paste0(opensky_base_url(), "/oauth/token"))
}

parse_datetime_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(lubridate::with_tz(x, tzone = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }
  if (is.character(x)) {
    dt <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c("ymd HMS", "ymd HM", "ymd"),
      tz = "UTC",
      quiet = TRUE
    ))
    if (all(is.na(dt))) {
      cli::cli_abort("Could not parse datetime: {x}")
    }
    return(dt)
  }
  cli::cli_abort("Unsupported datetime type: {class(x)}")
}

as_epoch <- function(x) {
  as.numeric(parse_datetime_utc(x))
}

validate_airport <- function(airport) {
  if (length(airport) != 1 || is.na(airport) || !nzchar(airport)) {
    cli::cli_abort("`airport` must be a non-empty string.")
  }
  airport <- toupper(trimws(airport))
  if (!grepl("^[A-Z0-9]{4}$", airport)) {
    cli::cli_abort("`airport` must be a 4-character ICAO code (e.g., CYVR).")
  }
  airport
}

check_time_window <- function(begin, end, max_hours, endpoint) {
  if (end <= begin) {
    cli::cli_abort("`end` must be after `begin`.")
  }
  hours <- as.numeric(difftime(as.POSIXct(end, origin = "1970-01-01", tz = "UTC"),
                               as.POSIXct(begin, origin = "1970-01-01", tz = "UTC"),
                               units = "hours"))
  if (hours > max_hours) {
    cli::cli_abort("`{endpoint}` requests are limited to {max_hours} hours.")
  }
  invisible(TRUE)
}

parse_flights_response <- function(x) {
  if (is.null(x)) {
    return(tibble::tibble())
  }
  if (is.data.frame(x)) {
    df <- tibble::as_tibble(x)
  } else if (is.list(x) && length(x) == 0) {
    df <- tibble::tibble()
  } else {
    df <- tibble::as_tibble(x)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  if ("callsign" %in% names(df)) {
    df$callsign <- trimws(df$callsign)
  }
  if ("firstSeen" %in% names(df)) {
    df$first_seen <- as.POSIXct(df$firstSeen, origin = "1970-01-01", tz = "UTC")
  }
  if ("lastSeen" %in% names(df)) {
    df$last_seen <- as.POSIXct(df$lastSeen, origin = "1970-01-01", tz = "UTC")
  }

  df
}

is_token_valid <- function() {
  token <- .opensky_env$access_token
  expires_at <- .opensky_env$token_expires_at
  if (is.null(token) || is.null(expires_at) || is.na(expires_at)) {
    return(FALSE)
  }
  as.POSIXct(Sys.time(), tz = "UTC") < expires_at
}

store_token <- function(access_token, expires_in) {
  .opensky_env$access_token <- access_token
  expires_in <- as.numeric(expires_in %||% 1800)
  .opensky_env$token_expires_at <- as.POSIXct(Sys.time(), tz = "UTC") + expires_in
}

clear_token <- function() {
  .opensky_env$access_token <- NULL
  .opensky_env$token_expires_at <- as.POSIXct(0, tz = "UTC")
}
