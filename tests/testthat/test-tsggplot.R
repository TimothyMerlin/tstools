test_that("tsggplot", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  tsg <- list(
    AirPassengers = diff(log(AirPassengers)) * 100,
    JohnsonJohnson = diff(log(JohnsonJohnson)) * 100
  )

  tsggplot(tsl)

  tstools::tsplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson))
  tsggplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson), labs = labs)

  # bar
  t <- tstools::init_tsplot_theme(x_tick_dt = 2)
  tstools::tsplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
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
  t <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15)
  )
  p <- tsggplot(tsl, labs = labs, theme = t)

  # Check if the labels are set correctly
  expect_equal(p$labels$x, labs$x)
  expect_equal(p$labels$y, labs$y)
  expect_equal(p$labels$y_right, labs$y_right)
  expect_equal(p$labels$title, labs$title)
  expect_equal(p$labels$subtitle, labs$subtitle)
  expect_equal(p$labels$caption, labs$caption)
  expect_equal(p$labels$tag, labs$tag)

  # Check if the theme attributes are applied correctly
  expect_equal(
    p$theme$plot.title,
    structure(list(
      family = NULL, face = "bold", colour = NULL, size = 30,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
  expect_equal(
    p$theme$plot.subtitle,
    structure(list(
      family = NULL, face = NULL, colour = NULL, size = 20,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
  expect_equal(
    p$theme$plot.caption,
    structure(list(
      family = NULL, face = NULL, colour = NULL, size = 25,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
  expect_equal(
    p$theme$plot.tag,
    structure(list(
      family = NULL, face = NULL, colour = NULL, size = 15,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
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
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(15, "pt"))
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

test_that("tsggplot axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.line.x = element_line(color = "blue", linewidth = 2),
    axis.line.y.left = element_line(color = "red", linewidth = 1),
    axis.line.y.right = element_line(color = "green", linewidth = 3)
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  # Check if the theme attributes are applied correctly
  expect_equal(
    p$theme$axis.line.x,
    structure(list(
      colour = "blue", linewidth = 2, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
  expect_equal(
    p$theme$axis.line.y.left,
    structure(list(
      colour = "red", linewidth = 1, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
  expect_equal(
    p$theme$axis.line.y.right,
    structure(list(
      colour = "green", linewidth = 3, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
})

test_that("tsggplot hide y axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.line.y = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_equal(
    p$theme$axis.line.y,
    structure(list(), class = c("element_blank", "element"))
  )
})

test_that("tsggplot hide x axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.line.x = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_equal(
    p$theme$axis.line.x,
    structure(list(), class = c("element_blank", "element"))
  )
})

test_that("tsggplot y axis overrides left and right", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.line.y = element_line(color = "red")
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_equal(
    p$theme$axis.line.y,
    structure(list(
      colour = "red", linewidth = NULL, linetype = NULL,
      lineend = NULL, arrow = FALSE, inherit.blank = FALSE
    ), class = c("element_line", "element"))
  )
})

test_that("tsggplot axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.text = element_text(color = "blue", size = 10),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_equal(
    p$theme$axis.text,
    structure(list(
      family = NULL, face = NULL, colour = "blue", size = 10,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
})

test_that("tsggplot hide axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.text = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_true(inherits(p$theme$axis.text, "element_blank"))
})

test_that("tsggplot x and y axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  t <- init_tsggplot_theme(
    axis.text.x = element_text(color = "blue", size = 10),
    axis.text.x.pos = "mid",
    axis.text.y.left = element_text(color = "green", size = 15),
    axis.text.y.right = element_text(color = "red", size = 20),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )

  expect_equal(
    p$theme$axis.text.x,
    structure(list(
      family = NULL, face = NULL, colour = "blue", size = 10,
      hjust = 0, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
  expect_equal(
    p$theme$axis.text.y.left,
    structure(list(
      family = NULL, face = NULL, colour = "green", size = 15,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
  expect_equal(
    p$theme$axis.text.y.right,
    structure(list(
      family = NULL, face = NULL, colour = "red", size = 20,
      hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL,
      margin = NULL, debug = NULL, inherit.blank = FALSE
    ), class = c("element_text", "element"))
  )
})
