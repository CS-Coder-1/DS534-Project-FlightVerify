library(testthat)

test_that("as_epoch handles numeric and character inputs", {
  expect_equal(flightverify:::as_epoch(0), 0)
  expect_true(is.numeric(flightverify:::as_epoch("2026-01-21 14:30")))
})

test_that("validate_airport enforces ICAO format", {
  expect_equal(flightverify:::validate_airport("cyvr"), "CYVR")
  expect_error(flightverify:::validate_airport("YVR"))
})