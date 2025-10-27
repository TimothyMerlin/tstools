test_that("ts, adds 1 period", {
  x <- AirPassengers
  out <- fill_year_with_nas(x)

  expect_equal(max(time(out)), 1961)
})

test_that("ts, fills year and adds 1 period", {
  x <- AirPassengers
  x <- stats::window(x, end = c(1960, 10))
  out <- fill_year_with_nas(x)

  expect_equal(max(time(out)), 1961)
})

test_that("ts, fill up start", {
  x <- AirPassengers
  x <- stats::window(x, start = c(1949, 4))
  out <- fill_year_with_nas(x, add_periods = 0, fill_up_start = TRUE)

  expect_equal(min(time(out)), 1949)
  expect_equal(out[1:3], rep(NA_real_, 3))
})

test_that("xts, fills year and adds 1 period", {
  # daily
  data("sample_matrix", package = "xts")
  x <- xts::as.xts(sample_matrix)
  out <- fill_year_with_nas(x, fill_up_start = FALSE)

  expect_s3_class(out, "xts")

  expect_equal(
    xts::periodicity(out)$scale,
    xts::periodicity(x)$scale
  )

  # It should not fill up start of the year
  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(as.Date(first_idx, tz = ""), as.Date("2007-01-02"))

  # It should fill to the end of the year + 1 extra period
  last_idx <- zoo::index(utils::tail(out, 1))
  expected_last <- seq(
    from = as.Date("2007-12-31"),
    by = "day", length.out = 2
  )[2]

  expect_equal(as.Date(last_idx, tz = ""), expected_last)

  # There should be NA rows beyond original data
  expect_true(any(is.na(utils::tail(out, 2))))
})

test_that("xts, fills up start", {
  # monthly
  x <- stats::window(AirPassengers, start = c(1949, 4))
  x <- xts::as.xts(x)
  out <- fill_year_with_nas(x, add_periods = 0, fill_up_start = TRUE)

  expect_s3_class(out, "xts")

  expect_equal(
    xts::periodicity(out)$scale,
    xts::periodicity(x)$scale
  )

  # It should fill up start of the year
  first_idx <- zoo::index(utils::head(out, 1))
  expect_equal(zoo::as.Date(first_idx, tz = ""), as.Date("1949-01-01"))

  # It should fill to the end of the year
  last_idx <- zoo::index(utils::tail(out, 1))
  expect_equal(zoo::as.Date(last_idx, tz = ""), as.Date("1960-12-01"))

  # There should be NA rows beyond original data
  expect_true(any(is.na(utils::head(out, 2))))
})
