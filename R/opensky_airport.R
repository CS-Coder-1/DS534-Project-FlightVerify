#' Get arrivals for an airport
#'
#' @param airport ICAO airport code (e.g., CYVR).
#' @param begin Start time (POSIXct, Date, numeric epoch, or string).
#' @param end End time (POSIXct, Date, numeric epoch, or string).
#' @param auth Whether to include authentication token.
#' @return A tibble of arrival flights.
#' @export
get_airport_arrivals <- function(airport, begin, end, auth = TRUE) {
  airport <- validate_airport(airport)
  begin_epoch <- as.integer(as_epoch(begin))
  end_epoch <- as.integer(as_epoch(end))

  check_time_window(begin_epoch, end_epoch, max_hours = 48, endpoint = "arrivals")

  resp <- opensky_request(
    path = "/flights/arrival",
    query = list(
      airport = airport,
      begin = begin_epoch,
      end = end_epoch
    ),
    auth = auth
  )

  flights <- parse_flights_response(resp)
  attr(flights, "airport") <- airport
  flights
}

#' Get departures for an airport
#'
#' @inheritParams get_airport_arrivals
#' @return A tibble of departure flights.
#' @export
get_airport_departures <- function(airport, begin, end, auth = TRUE) {
  airport <- validate_airport(airport)
  begin_epoch <- as.integer(as_epoch(begin))
  end_epoch <- as.integer(as_epoch(end))

  check_time_window(begin_epoch, end_epoch, max_hours = 48, endpoint = "departures")

  resp <- opensky_request(
    path = "/flights/departure",
    query = list(
      airport = airport,
      begin = begin_epoch,
      end = end_epoch
    ),
    auth = auth
  )

  flights <- parse_flights_response(resp)
  attr(flights, "airport") <- airport
  flights
}
