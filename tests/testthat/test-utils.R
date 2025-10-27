test_that("getGlobalXInfo_tsggplot, ts", {
  theme <- init_tsggplot_theme()
  tsl <- list(AirPassengers = AirPassengers)
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
    ), dominant_freq = "ts"
  )
  expect_equal(out, expected)

  # with monthly xts instead of ts
  tsl <- list(AirPassengers = xts::as.xts(AirPassengers))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )
  expected$dominant_freq <- "monthly"
  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts", {
  theme <- init_tsggplot_theme(fill_year_with_nas = TRUE)
  data("sample_matrix", package = "xts")
  tsl <- list(sample = xts::as.xts(sample_matrix))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )

  expected <-
    list(
      x_range = structure(c(1167692400, 1199142000), class = c(
        "POSIXct",
        "POSIXt"
      )), yearly_tick_pos = structure(c(13514, 13879), class = "Date"),
      year_labels_start = c(2007, 2008), min_year = 2007, max_year = 2008,
      quarterly_tick_pos = structure(c(
        13514, 13604, 13695, 13787,
        13879
      ), class = "Date"), dominant_freq = "daily"
    )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts, don't fill year", {
  theme <- init_tsggplot_theme(fill_year_with_nas = FALSE)
  data("sample_matrix", package = "xts")
  tsl <- list(sample = xts::as.xts(sample_matrix))
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    fill_up = theme$fill_year_with_nas,
    fill_up_start = theme$fill_up_start,
    tick_dt = theme$axis_x_tick_dt,
    label_dt = theme$axis_x_label_dt,
    manual_ticks = NULL
  )

  expected <-
    list(
      x_range = structure(c(1167692400, 1183154400), class = c(
        "POSIXct",
        "POSIXt"
      )), yearly_tick_pos = structure(13514, class = "Date"),
      year_labels_start = 2007, min_year = 2007, max_year = 2007,
      quarterly_tick_pos = structure(c(13514, 13604, 13695), class = "Date"),
      dominant_freq = "daily"
    )

  expect_equal(out, expected)
})

test_that("getGlobalXInfo_tsggplot, xts", {
  theme <- init_tsggplot_theme()
  tsl <- list(
    AirPassengers = xts::as.xts(AirPassengers),
    JohnsonJohnson = xts::as.xts(JohnsonJohnson)
  )
  out <- getGlobalXInfo_tsggplot(
    tsl, NULL,
    theme$fill_year_with_nas,
    theme$fill_up_start,
    theme$axis_x_tick_dt,
    theme$axis_x_label_dt,
    NULL
  )

  expected <- list(
    x_range = c(1949, 1981),
    yearly_tick_pos = c(
      1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972,
      1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    year_labels_start = c(
      1949,
      1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960,
      1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971,
      1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981
    ),
    min_year = 1949,
    max_year = 1981,
    quarterly_tick_pos = c(
      1949, 1949.25, 1949.5,
      1949.75, 1950, 1950.25, 1950.5, 1950.75, 1951, 1951.25, 1951.5,
      1951.75, 1952, 1952.25, 1952.5, 1952.75, 1953, 1953.25, 1953.5,
      1953.75, 1954, 1954.25, 1954.5, 1954.75, 1955, 1955.25, 1955.5,
      1955.75, 1956, 1956.25, 1956.5, 1956.75, 1957, 1957.25, 1957.5,
      1957.75, 1958, 1958.25, 1958.5, 1958.75, 1959, 1959.25, 1959.5,
      1959.75, 1960, 1960.25, 1960.5, 1960.75, 1961, 1961.25, 1961.5,
      1961.75, 1962, 1962.25, 1962.5, 1962.75, 1963, 1963.25, 1963.5,
      1963.75, 1964, 1964.25, 1964.5, 1964.75, 1965, 1965.25, 1965.5,
      1965.75, 1966, 1966.25, 1966.5, 1966.75, 1967, 1967.25, 1967.5,
      1967.75, 1968, 1968.25, 1968.5, 1968.75, 1969, 1969.25, 1969.5,
      1969.75, 1970, 1970.25, 1970.5, 1970.75, 1971, 1971.25, 1971.5,
      1971.75, 1972, 1972.25, 1972.5, 1972.75, 1973, 1973.25, 1973.5,
      1973.75, 1974, 1974.25, 1974.5, 1974.75, 1975, 1975.25, 1975.5,
      1975.75, 1976, 1976.25, 1976.5, 1976.75, 1977, 1977.25, 1977.5,
      1977.75, 1978, 1978.25, 1978.5, 1978.75, 1979, 1979.25, 1979.5,
      1979.75, 1980, 1980.25, 1980.5, 1980.75, 1981
    ),
    dominant_freq = "monthly"
  )

  expect_equal(out, expected)
})
