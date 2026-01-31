#' Authenticate with the OpenSky Network API
#'
#' Exchanges client credentials for a short-lived access token and stores it
#' in the package environment for subsequent requests.
#'
#' @param client_id OAuth2 client id.
#' @param client_secret OAuth2 client secret.
#' @param token_url Token endpoint. Defaults to the OpenSky OAuth token URL.
#' @param scope Optional OAuth scope.
#' @return The access token (invisibly).
#' @export
opensky_auth <- function(client_id, client_secret, token_url = NULL, scope = NULL) {
  if (missing(client_id) || !nzchar(client_id)) {
    cli::cli_abort("`client_id` is required.")
  }
  if (missing(client_secret) || !nzchar(client_secret)) {
    cli::cli_abort("`client_secret` is required.")
  }

  token_url <- token_url %||% opensky_token_url()

  .opensky_env$client_id <- client_id
  .opensky_env$client_secret <- client_secret
  .opensky_env$token_url <- token_url
  .opensky_env$scope <- scope

  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = token_url
  )

  token <- httr2::oauth_flow_client_credentials(client = client, scope = scope)

  access_token <- token$access_token %||% token$token
  if (is.null(access_token)) {
    cli::cli_abort("OAuth token response did not include an access token.")
  }

  store_token(access_token, token$expires_in %||% token$expires)
  cli::cli_inform("OpenSky token acquired.")
  invisible(access_token)
}

#' Use anonymous access
#'
#' Clears any stored token and credentials.
#'
#' @export
opensky_auth_anonymous <- function() {
  clear_token()
  .opensky_env$client_id <- NULL
  .opensky_env$client_secret <- NULL
  .opensky_env$token_url <- NULL
  .opensky_env$scope <- NULL
  cli::cli_inform("Using anonymous access.")
  invisible(NULL)
}

ensure_token <- function() {
  if (is_token_valid()) {
    return(invisible(TRUE))
  }
  if (!is.null(.opensky_env$client_id) && !is.null(.opensky_env$client_secret)) {
    opensky_auth(
      client_id = .opensky_env$client_id,
      client_secret = .opensky_env$client_secret,
      token_url = .opensky_env$token_url,
      scope = .opensky_env$scope
    )
  }
  invisible(is_token_valid())
}