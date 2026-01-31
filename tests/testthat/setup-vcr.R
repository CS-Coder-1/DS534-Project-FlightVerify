if (requireNamespace("vcr", quietly = TRUE)) {
  vcr::vcr_configure(
    dir = file.path("tests", "fixtures"),
    record = "once"
  )
}