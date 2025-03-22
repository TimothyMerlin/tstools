test_that("tsggplot", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  tsg <- list(
    AirPassengers = diff(log(AirPassengers)) * 100,
    JohnsonJohnson = diff(log(JohnsonJohnson)) * 100
  )

  tsggplot(tsl)

  # plot title and subtitle
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands",
    caption = "(based on data from ...)",
    tag = "A"
  )
  tsggplot(tsl, labs = labs)

  tstools::tsplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson))
  tsggplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson), labs = labs)

  # bar
  t <- tstools::init_tsplot_theme(x_tick_dt = 2)
  tstools::tsplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )
  t <- init_tsggplot_theme(axis_y_show = FALSE, axis_x_show = FALSE)
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE,
    theme = t
  )

  t <- init_tsggplot_theme(quarterly_ticks = FALSE)
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE,
    theme = t
  )

  # grouped bar
  tstools::tsplot(list(tsg$JohnsonJohnson, lag(tsg$JohnsonJohnson, 2)),
    left_as_bar = TRUE,
    relative_bar_chart = TRUE
  )
  tsggplot(list(tsg$JohnsonJohnson, lag(tsg$JohnsonJohnson, 2)),
    left_as_bar = TRUE,
    group_bar_chart = FALSE
  )

  # band
  tstools::tsplot(list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_band = TRUE
  )
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_band = TRUE
  )

  tstools::tsplot(
    list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE
  )
  t <- init_tsggplot_theme(grids_x_show = TRUE, axis_x_label_dt = 1)
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = t
  )

  tstools::tsplot(
    list(tsl$JohnsonJohnson),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE
  )
  tsggplot(
    list(tsl$JohnsonJohnson),
    tsr = list(tsg$JohnsonJohnson),
    left_as_bar = TRUE
  )

  tstools::tsplot(
    list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_bar = TRUE
  )
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE
  )
})

test_that("tsggplot sets custom labels correctly", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands",
    caption = "(based on data from ...)",
    tag = "A"
  )
  p <- tsggplot(tsl, labs = labs)

  # Check if the labels are set correctly
  expect_equal(p$labels$x, labs$x)
  expect_equal(p$labels$y, labs$y)
  expect_equal(p$labels$y_right, labs$y_right)
  expect_equal(p$labels$title, labs$title)
  expect_equal(p$labels$subtitle, labs$subtitle)
  expect_equal(p$labels$caption, labs$caption)
  expect_equal(p$labels$tag, labs$tag)
})

test_that("tsggplot applies custom themes correctly", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(AirPassengers = diff(log(AirPassengers)) * 100)
  t <- init_tsggplot_theme(grids_x_show = TRUE, axis_x_label_dt = 1)
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = t
  )

  # Check if the theme attributes are applied correctly
  expect_true(length(p$theme$panel.grid.major.x) > 0)
})

test_that("tsggplot ticks", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(AirPassengers = diff(log(AirPassengers)) * 100)
  t <- init_tsggplot_theme(
    axis.minor.ticks.length = ggplot2::unit(15, "pt"),
    axis.minor.ticks.x.bottom = element_line(color = "blue", linewidth = 2),
    axis.ticks.length = ggplot2::unit(20, "pt"),
    axis.ticks.x.bottom = element_line(color = "red", linewidth = 3)
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = t
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(20, "pt"))
  expect_equal(
    p$theme$axis.minor.ticks.x.bottom,
    structure(list(
      colour = "blue", linewidth = 2, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
  expect_equal(p$theme$axis.ticks.length, ggplot2::unit(20, "pt"))
  expect_equal(
    p$theme$axis.ticks.x.bottom,
    structure(list(
      colour = "red", linewidth = 3, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
})
