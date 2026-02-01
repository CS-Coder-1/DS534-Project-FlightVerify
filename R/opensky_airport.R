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
  check_historical_limit(begin_epoch)

  resp <- opensky_request(
    path = "/flights/all",
    query = list(
      begin = begin_epoch,
      end = end_epoch,
      airport = airport
    ),
    auth = auth
  )

  flights <- parse_flights_response(resp)
  
  # Filter to only arrivals (where estArrivalAirport matches our airport)
  if (nrow(flights) > 0 && "est_arrival_airport" %in% names(flights)) {
    flights <- flights[!is.na(flights$est_arrival_airport) & 
                       flights$est_arrival_airport == airport, ]
  }
  
  attr(flights, "airport") <- airport
  attr(flights, "type") <- "arrivals"
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
  check_historical_limit(begin_epoch)

  resp <- opensky_request(
    path = "/flights/all",
    query = list(
      begin = begin_epoch,
      end = end_epoch,
      airport = airport
    ),
    auth = auth
  )

  flights <- parse_flights_response(resp)
  
  # Filter to only departures (where estDepartureAirport matches our airport)
  if (nrow(flights) > 0 && "est_departure_airport" %in% names(flights)) {
    flights <- flights[!is.na(flights$est_departure_airport) & 
                       flights$est_departure_airport == airport, ]
  }
  
  attr(flights, "airport") <- airport
  attr(flights, "type") <- "departures"
  flights
}