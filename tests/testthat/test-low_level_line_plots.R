test_that("draw_tsggplot_lines, ts", {
  x <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  x <- lapply(x, xts::as.xts)
  p <- ggplot()
  theme <- init_tsggplot_theme()
  theme$NA_continue_line <- rep(theme$NA_continue_line, 2)
  theme$show_points <- rep(theme$show_points, 2)
  out <- draw_tsggplot_lines(p, x, theme = theme)
})

test_that("draw_tsggplot_lines, xts", {
  data("sample_matrix", package = "xts")
  x <- list(x = xts::as.xts(sample_matrix)[, "Open"])
  p <- ggplot()
  theme <- init_tsggplot_theme()
  out <- draw_tsggplot_lines(p, x, theme = theme)
})
