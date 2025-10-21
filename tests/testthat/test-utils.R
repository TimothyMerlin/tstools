test_that("getGlobalXInfo_tsggplot, ts", {
  theme <- init_tsggplot_theme()
  tsl <- AirPassengers
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    theme$fill_year_with_nas,
    theme$fill_up_start,
    theme$axis_x_tick_dt,
    theme$axis_x_label_dt,
    NULL
  )

  expected <- list(
    x_range = c(1949, 1961),
    yearly_tick_pos = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955,
      1956, 1957, 1958, 1959, 1960, 1961
    ),
    year_labels_start = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955,
      1956, 1957, 1958, 1959, 1960, 1961
    ),
    min_year = 1949,
    max_year = 1961,
    quarterly_tick_pos = c(
      1949, 1949.25, 1949.5, 1949.75, 1950,
      1950.25, 1950.5, 1950.75, 1951, 1951.25, 1951.5, 1951.75,
      1952, 1952.25, 1952.5, 1952.75, 1953, 1953.25, 1953.5, 1953.75,
      1954, 1954.25, 1954.5, 1954.75, 1955, 1955.25, 1955.5, 1955.75,
      1956, 1956.25, 1956.5, 1956.75, 1957, 1957.25, 1957.5, 1957.75,
      1958, 1958.25, 1958.5, 1958.75, 1959, 1959.25, 1959.5, 1959.75,
      1960, 1960.25, 1960.5, 1960.75, 1961
    )
  )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts", {
  theme <- init_tsggplot_theme(fill_year_with_nas = TRUE)
  data("sample_matrix", package = "xts")
  tsl <- xts::as.xts(sample_matrix)
  out <- getGlobalXInfo_tsggplot(
    x, NULL,
    theme$fill_year_with_nas,
    theme$fill_up_start,
    theme$axis_x_tick_dt,
    theme$axis_x_label_dt,
    NULL
  )

  expected <-
    list(
      x_range = c(2007, 2008),
      yearly_tick_pos = c(2007, 2008),
      year_labels_start = c(2007, 2008),
      min_year = 2007, max_year = 2008,
      quarterly_tick_pos = c(2007, 2007.25, 2007.5, 2007.75, 2008)
    )
})
