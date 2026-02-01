`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

opensky_base_url <- function() {
  getOption("flightverify.opensky_base_url", "https://opensky-network.org/api")
}

opensky_token_url <- function() {
  getOption("flightverify.opensky_token_url", 
            "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token")
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
  
  hours <- as.numeric(difftime(
    as.POSIXct(end, origin = "1970-01-01", tz = "UTC"),
    as.POSIXct(begin, origin = "1970-01-01", tz = "UTC"),
    units = "hours"
  ))
  
  if (hours > max_hours) {
    cli::cli_abort(
      "`{endpoint}` requests are limited to {max_hours} hours. Your request spans {round(hours, 1)} hours."
    )
  }
  
  if (hours > max_hours * 0.9) {
    cli::cli_warn(
      "Time window is close to the {max_hours}-hour limit ({round(hours, 1)} hours)."
    )
  }
  
  invisible(TRUE)
}

parse_flights_response <- function(x) {
  if (is.null(x) || (is.list(x) && length(x) == 0)) {
    return(tibble::tibble())
  }
  
  df <- tibble::as_tibble(x)
  
  if (nrow(df) == 0) {
    return(df)
  }

  # Standardize column names to snake_case
  if ("callsign" %in% names(df)) {
    df$callsign <- trimws(df$callsign)
  }
  if ("firstSeen" %in% names(df)) {
    df$first_seen <- as.POSIXct(df$firstSeen, origin = "1970-01-01", tz = "UTC")
    df$firstSeen <- NULL  # Remove original column
  }
  if ("lastSeen" %in% names(df)) {
    df$last_seen <- as.POSIXct(df$lastSeen, origin = "1970-01-01", tz = "UTC")
    df$lastSeen <- NULL  # Remove original column
  }
  
  # Add more field conversions as needed
  if ("estDepartureAirport" %in% names(df)) {
    df$est_departure_airport <- df$estDepartureAirport
    df$estDepartureAirport <- NULL
  }
  if ("estArrivalAirport" %in% names(df)) {
    df$est_arrival_airport <- df$estArrivalAirport
    df$estArrivalAirport <- NULL
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

#' Check if requesting data beyond typical historical limits
#'
#' @param time_point Unix timestamp or POSIXct
#' @param max_days_back Maximum days to look back (default 30)
#' @return Invisible TRUE, warns if beyond limit
#' @keywords internal
check_historical_limit <- function(time_point, max_days_back = 30) {
  time_point <- as.POSIXct(time_point, origin = "1970-01-01", tz = "UTC")
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  days_back <- as.numeric(difftime(now, time_point, units = "days"))
  
  if (days_back > max_days_back) {
    cli::cli_warn(
      "Requesting data from {round(days_back)} days ago. OpenSky historical data may be limited beyond {max_days_back} days."
    )
  }
  
  invisible(TRUE)
}

#' Parse OpenSky state vectors response
#'
#' @param x Response from OpenSky states endpoint
#' @return A tibble of state vectors
#' @keywords internal
parse_states_response <- function(x) {
  if (is.null(x) || is.null(x$states) || length(x$states) == 0) {
    return(tibble::tibble())
  }
  
  # OpenSky returns states as a list of arrays
  states <- x$states
  
  # Column names for state vectors (as per OpenSky API documentation)
  col_names <- c(
    "icao24", "callsign", "origin_country", "time_position", "last_contact",
    "longitude", "latitude", "baro_altitude", "on_ground", "velocity",
    "true_track", "vertical_rate", "sensors", "geo_altitude", "squawk",
    "spi", "position_source"
  )
  
  # Convert list of arrays to data frame
  df <- do.call(rbind, lapply(states, function(s) {
    # Pad with NA if necessary
    s <- c(s, rep(NA, length(col_names) - length(s)))
    s[1:length(col_names)]
  }))
  
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- col_names
  
  df <- tibble::as_tibble(df)
  
  # Convert types
  if ("callsign" %in% names(df)) {
    df$callsign <- trimws(as.character(df$callsign))
  }
  if ("time_position" %in% names(df)) {
    df$time_position <- as.POSIXct(as.numeric(df$time_position), origin = "1970-01-01", tz = "UTC")
  }
  if ("last_contact" %in% names(df)) {
    df$last_contact <- as.POSIXct(as.numeric(df$last_contact), origin = "1970-01-01", tz = "UTC")
  }
  
  # Convert numeric columns
  numeric_cols <- c("longitude", "latitude", "baro_altitude", "velocity", 
                    "true_track", "vertical_rate", "geo_altitude")
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  # Convert logical
  if ("on_ground" %in% names(df)) {
    df$on_ground <- as.logical(df$on_ground)
  }
  if ("spi" %in% names(df)) {
    df$spi <- as.logical(df$spi)
  }
  
  df
}