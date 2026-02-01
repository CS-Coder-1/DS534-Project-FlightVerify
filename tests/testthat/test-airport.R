test_that("get_airport_arrivals validates inputs", {
  skip_if_offline()
  skip_on_cran()
  
  expect_error(
    get_airport_arrivals("YVR", Sys.time() - 3600, Sys.time()),
    "4-character ICAO"
  )
  
  expect_error(
    get_airport_arrivals("CYVR", Sys.time(), Sys.time() - 3600),
    "after"
  )
})

# Add integration tests only if credentials are available
test_that("get_airport_arrivals returns data with auth", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not(nzchar(Sys.getenv("OPENSKY_CLIENT_ID")), "No OpenSky credentials")
  
  opensky_auth(
    client_id = Sys.getenv("OPENSKY_CLIENT_ID"),
    client_secret = Sys.getenv("OPENSKY_CLIENT_SECRET")
  )
  
  end_time <- Sys.time()
  begin_time <- end_time - 3600  # 1 hour ago
  
  result <- get_airport_arrivals("CYVR", begin_time, end_time)
  
  expect_s3_class(result, "tbl_df")
  expect_true(attr(result, "airport") == "CYVR")
})