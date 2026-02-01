test_that("opensky_auth validates inputs", {
  expect_error(opensky_auth(client_id = ""), "required")
  expect_error(opensky_auth(client_id = "test", client_secret = ""), "required")
})

test_that("opensky_auth_anonymous clears credentials", {
  .opensky_env$access_token <- "test_token"
  .opensky_env$client_id <- "test_id"
  
  expect_message(opensky_auth_anonymous(), "anonymous")
  expect_null(.opensky_env$access_token)
  expect_null(.opensky_env$client_id)
})

test_that("token validation works", {
  clear_token()
  expect_false(is_token_valid())
  
  store_token("test_token", 3600)
  expect_true(is_token_valid())
  
  # Expired token
  store_token("test_token", -10)
  expect_false(is_token_valid())
})