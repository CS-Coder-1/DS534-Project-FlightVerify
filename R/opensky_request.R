opensky_request <- function(path, query = list(), auth = TRUE) {
  url <- paste0(opensky_base_url(), path)
  req <- httr2::request(url)
  req <- httr2::req_headers(req, `Accept` = "application/json")
  req <- httr2::req_user_agent(req, "flightverify (R)")

  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }

  # Add OAuth token if authenticated
  if (isTRUE(auth)) {
    ensure_token()
    if (!is.null(.opensky_env$access_token)) {
      req <- httr2::req_auth_bearer_token(req, .opensky_env$access_token)
    } else {
      cli::cli_warn("No authentication token available. Using anonymous access (rate-limited).")
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

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort("Failed to connect to OpenSky API: {conditionMessage(e)}")
    }
  )
  
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    body <- httr2::resp_body_string(resp)
    error_msg <- switch(
      as.character(status),
      "400" = "Bad request - check your parameters",
      "401" = "Unauthorized - check your OAuth credentials or token has expired",
      "403" = "Forbidden - you may not have access to this endpoint",
      "404" = "Not found - check the airport code or time range",
      "429" = "Rate limit exceeded - try again later or use authentication",
      paste0("Request failed with status ", status)
    )
    cli::cli_abort("{error_msg}: {body}")
  }

  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}