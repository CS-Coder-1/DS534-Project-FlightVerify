#' Canadian airport reference table (MVP)
#'
#' Provides a small reference table of major Canadian airports. Coordinates are
#' intentionally left blank in the MVP and can be filled in during data curation.
#'
#' @return A tibble with columns: icao, iata, name, city, province, country, lat, lon.
#' @export
get_canadian_airports <- function() {
  path <- system.file("extdata", "canadian_airports.csv", package = "flightverify")
  if (path == "") {
    cli::cli_abort("Could not locate canadian_airports.csv in extdata.")
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}