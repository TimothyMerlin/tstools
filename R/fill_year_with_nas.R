#' Fill Up a Time Series with NAs
#'
#' When plotting a time series you might want set the range of the plot a little wider than just the start and end date of the original series. This function add fills up the current period (typically year) with NA.
#'
#' @param x object of class ts
#' @param add_periods integer periods to add.
#' @param fill_up_start logical should start year be filled up? Defaults to FALSE.
#' @export
#' @importFrom stats start end
fill_year_with_nas <- function(x, add_periods = 1,
                               fill_up_start = FALSE) {
  UseMethod("fill_year_with_nas")
}

#' @export
fill_year_with_nas.ts <- function(x, add_periods = 1,
                                  fill_up_start = FALSE) {
  frq <- frequency(x)
  de <- frq - end(x)[2]
  ds <- start(x)[2] - 1
  new_start <- c(start(x)[1], 1)
  if (fill_up_start) {
    ts(c(rep(NA, ds), x, rep(NA, de + add_periods)),
      start = new_start,
      frequency = frq
    )
  } else {
    ts(c(x, rep(NA, de + add_periods)),
      start = start(x),
      frequency = frq
    )
  }
}

#' @export
fill_year_with_nas.xts <- function(x, add_periods = 1,
                                   fill_up_start = FALSE) {
  if (!xts::is.xts(x)) stop("Input must be an xts object.")
  if (add_periods < 0) stop("add_periods must be >= 0.")

  idx <- zoo::index(x)
  tz <- attr(idx, "tzone")

  per <- xts::periodicity(x)$scale
  step <- switch(per,
    "daily" = "day",
    "weekly" = "week",
    "monthly" = "month",
    "quarterly" = "quarter",
    stop("Unsupported frequency: ", per)
  )

  start_d <- as.Date(start(x), tz = tz)
  end_d <- as.Date(end(x), tz = tz)

  year_start <- as.Date(paste0(format(start_d, "%Y"), "-01-01"))
  year_end <- as.Date(paste0(format(end_d, "%Y"), "-12-31"))

  seq_start <- if (fill_up_start) year_start else start_d
  full_idx <- seq(seq_start, year_end, by = step)

  if (add_periods > 0) {
    extra_idx <- seq(
      from = utils::tail(full_idx, 1),
      by = step, length.out = add_periods + 1
    )[-1]
    full_idx <- c(full_idx, extra_idx)
  }

  full_idx <- as.POSIXct(paste(full_idx, "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = tz)

  merge(x, full_idx)
}


#' @export
fill_year_with_nas.zoo <- function(x, add_periods = 1,
                                   fill_up_start = FALSE) {
  stop("zoo support for filling up NAs not supported yet.")
}
