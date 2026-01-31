opensky_request <- function(path, query = list(), auth = TRUE) {
  url <- paste0(opensky_base_url(), path)
  req <- httr2::request(url)
  req <- httr2::req_headers(req, `Accept` = "application/json")
  req <- httr2::req_user_agent(req, "flightverify (R)")

  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  if (isTRUE(auth)) {
    ensure_token()
    if (!is.null(.opensky_env$access_token)) {
      req <- httr2::req_auth_bearer_token(req, .opensky_env$access_token)
    }
  }

  req <- httr2::req_retry(
    req,
    max_tries = 3,
    backoff = function(i) 1 + (i - 1) * 2,
    is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    }
  )

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    body <- httr2::resp_body_string(resp)
    cli::cli_abort("OpenSky API request failed [{status}]: {body}")
  }

  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}