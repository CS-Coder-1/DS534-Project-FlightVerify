#' Authenticate with the OpenSky Network API
#'
#' Exchanges OAuth2 client credentials for an access token.
#' OpenSky Network uses OAuth2 Client Credentials flow.
#'
#' @param client_id OAuth2 client ID from your OpenSky account.
#' @param client_secret OAuth2 client secret from your OpenSky account.
#' @param token_url Token endpoint URL. Usually auto-detected.
#' @return The access token (invisibly).
#' @export
opensky_auth <- function(client_id, client_secret, token_url = NULL) {
  if (missing(client_id) || !nzchar(client_id)) {
    cli::cli_abort("`client_id` is required. Get it from https://opensky-network.org/")
  }
  if (missing(client_secret) || !nzchar(client_secret)) {
    cli::cli_abort("`client_secret` is required. Get it from https://opensky-network.org/")
  }

  # Store credentials for token refresh
  .opensky_env$client_id <- client_id
  .opensky_env$client_secret <- client_secret
  .opensky_env$token_url <- token_url %||% opensky_token_url()

  # Get the token
  token <- get_oauth_token(client_id, client_secret, .opensky_env$token_url)
  
  cli::cli_inform("✓ OpenSky OAuth token acquired successfully.")
  invisible(token)
}

#' Use anonymous access
#'
#' Clears any stored credentials and tokens.
#'
#' @export
opensky_auth_anonymous <- function() {
  clear_token()
  .opensky_env$client_id <- NULL
  .opensky_env$client_secret <- NULL
  .opensky_env$token_url <- NULL
  cli::cli_inform("Using anonymous access.")
  invisible(NULL)
}

#' Get OAuth2 token
#' @keywords internal
#' @noRd
get_oauth_token <- function(client_id, client_secret, token_url) {
  # Create OAuth client
  client <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = token_url,
    name = "opensky"
  )
  
  # Get token using client credentials flow
  token <- httr2::oauth_flow_client_credentials(
    client = client,
    scope = NULL
  )
  
  # Extract access token
  access_token <- token$access_token %||% token$token
  if (is.null(access_token)) {
    cli::cli_abort("OAuth token response did not include an access token.")
  }
  
  # Store token with expiry
  expires_in <- token$expires_in %||% 3600
  store_token(access_token, expires_in)
  
  access_token
}

#' Ensure valid token exists
#' @keywords internal
#' @noRd
ensure_token <- function() {
  # If token is still valid, we're good
  if (is_token_valid()) {
    return(invisible(TRUE))
  }
  
  # If we have credentials, refresh the token
  if (!is.null(.opensky_env$client_id) && !is.null(.opensky_env$client_secret)) {
    tryCatch({
      get_oauth_token(
        .opensky_env$client_id,
        .opensky_env$client_secret,
        .opensky_env$token_url
      )
      return(invisible(TRUE))
    }, error = function(e) {
      cli::cli_warn("Failed to refresh OAuth token: {conditionMessage(e)}")
      return(invisible(FALSE))
    })
  }
  
  invisible(FALSE)
}