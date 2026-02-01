library(testthat)

test_that("parse_datetime_utc handles various formats", {
  # POSIXct
  dt <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  expect_equal(parse_datetime_utc(dt), dt)
  
  # Date
  d <- as.Date("2024-01-15")
  expect_s3_class(parse_datetime_utc(d), "POSIXct")
  
  # Numeric (epoch)
  epoch <- 1705320000
  result <- parse_datetime_utc(epoch)
  expect_s3_class(result, "POSIXct")
  
  # Character
  char_dt <- "2024-01-15 12:00:00"
  result <- parse_datetime_utc(char_dt)
  expect_s3_class(result, "POSIXct")
})

test_that("validate_airport validates ICAO codes", {
  expect_equal(validate_airport("CYVR"), "CYVR")
  expect_equal(validate_airport("cyvr"), "CYVR")
  expect_equal(validate_airport(" CYVR "), "CYVR")
  
  expect_error(validate_airport("YVR"), "4-character ICAO")
  expect_error(validate_airport(""), "non-empty")
  expect_error(validate_airport(NA), "non-empty")
})

test_that("check_time_window validates time ranges", {
  begin <- as.POSIXct("2024-01-15 00:00:00", tz = "UTC")
  end <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  
  expect_silent(check_time_window(as.numeric(begin), as.numeric(end), 24, "test"))
  
  # End before begin
  expect_error(
    check_time_window(as.numeric(end), as.numeric(begin), 24, "test"),
    "after"
  )
  
  # Exceeds limit
  end_far <- as.POSIXct("2024-01-17 00:00:00", tz = "UTC")
  expect_error(
    check_time_window(as.numeric(begin), as.numeric(end_far), 24, "test"),
    "limited to"
  )
})

test_that("as_epoch converts to Unix timestamp", {
  dt <- as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
  epoch <- as_epoch(dt)
  expect_type(epoch, "double")
  expect_gt(epoch, 0)
})