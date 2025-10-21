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
  theme <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold", family = "sans"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15)
  )
  p <- tsggplot(tsl, labs = labs, theme = theme)

  # Check if the labels are set correctly
  expect_equal(p$labels$x, labs$x)
  expect_equal(p$labels$y, labs$y)
  expect_equal(p$labels$y_right, labs$y_right)
  expect_equal(p$labels$title, labs$title)
  expect_equal(p$labels$subtitle, labs$subtitle)
  expect_equal(p$labels$caption, labs$caption)
  expect_equal(p$labels$tag, labs$tag)

  expect_equal(p$theme$plot.title$face, "bold")
  expect_equal(p$theme$plot.title$size, 30)
  expect_equal(p$theme$plot.title$family, "sans")
  expect_false(isTRUE(p$theme$plot.title$inherit.blank))

  expect_equal(p$theme$plot.subtitle$size, 20)

  expect_equal(p$theme$plot.caption$size, 25)

  expect_equal(p$theme$plot.tag$size, 15)
})

test_that("tsggplot, grids", {
  theme <- init_tsggplot_theme(
    grids_x_count = c(5, 6, 8, 10),
    grids_x_count_strict = FALSE,
    panel.grid.major.x = element_line(
      color = "red",
      linewidth = 4,
      linetype = "dashed"
    ),
    panel.grid.major.y = element_line(
      color = "blue",
      linewidth = 5,
    )
  )
  p <- tsggplot(AirPassengers,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$panel.grid.major.x$colour, "red")
  expect_equal(p$theme$panel.grid.major.x$linewidth, 4)
  expect_equal(p$theme$panel.grid.major.x$linetype, "dashed")
  expect_null(p$theme$panel.grid.major.x$lineend)
  expect_false(p$theme$panel.grid.major.x$arrow)
  expect_false(p$theme$panel.grid.major.x$inherit.blank)

  expect_equal(p$theme$panel.grid.major.y$colour, "blue")
  expect_equal(p$theme$panel.grid.major.y$linewidth, 5)
  expect_null(p$theme$panel.grid.major.y$linetype)
  expect_null(p$theme$panel.grid.major.y$lineend)
  expect_false(p$theme$panel.grid.major.y$arrow)
  expect_false(p$theme$panel.grid.major.y$inherit.blank)
})

test_that("tsggplot tick labels centered", {
  tsl <- list(AirPassengers = AirPassengers)
  theme <- init_tsggplot_theme(
    axis.minor.ticks.length = ggplot2::unit(5, "pt"),
    axis.minor.ticks.x.bottom = aes(
      color = "blue", linewidth = 2, linetype = "dashed"
    ),
    axis.ticks.length = ggplot2::unit(50, "pt"),
    axis.ticks.x.bottom = element_line(color = "red", linewidth = 3),
    axis.text.x.pos = "mid"
  )
  p <- tsggplot(list(tsl$AirPassengers),
    left_as_bar = TRUE,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(-50, "pt"))

  expect_equal(p$theme$axis.minor.ticks.x.bottom$colour, "red")
  expect_equal(p$theme$axis.minor.ticks.x.bottom$linewidth, 3)
  expect_null(p$theme$axis.minor.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.minor.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.minor.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.minor.ticks.x.bottom$inherit.blank)

  # main ticks are set to 0, only labels are showing
  expect_equal(p$theme$axis.ticks.length, ggplot2::unit(0, "cm"))

  seg_layers <- which(
    sapply(p$layers, function(l) inherits(l$geom, "GeomSegment"))
  )
  seg_layer <- p$layers[[seg_layers]]

  expected_segment <- structure(
    list(
      x = c(
        1949.25, 1949.5, 1949.75, 1950.25, 1950.5, 1950.75, 1951.25, 1951.5,
        1951.75, 1952.25, 1952.5, 1952.75, 1953.25, 1953.5, 1953.75, 1954.25,
        1954.5, 1954.75, 1955.25, 1955.5, 1955.75, 1956.25, 1956.5, 1956.75,
        1957.25, 1957.5, 1957.75, 1958.25, 1958.5, 1958.75, 1959.25, 1959.5,
        1959.75, 1960.25, 1960.5, 1960.75
      ),
      xend = c(
        1949.25, 1949.5, 1949.75, 1950.25, 1950.5, 1950.75, 1951.25, 1951.5,
        1951.75, 1952.25, 1952.5, 1952.75, 1953.25, 1953.5, 1953.75, 1954.25,
        1954.5, 1954.75, 1955.25, 1955.5, 1955.75, 1956.25, 1956.5, 1956.75,
        1957.25, 1957.5, 1957.75, 1958.25, 1958.5, 1958.75, 1959.25, 1959.5,
        1959.75, 1960.25, 1960.5, 1960.75
      ),
      y = rep(0, 36),
      yend = rep(40, 36)
    ),
    class = "data.frame",
    row.names = c(NA, -36L)
  )
  expect_equal(seg_layer$data, expected_segment)

  expect_equal(
    seg_layer$aes_params,
    list(colour = "blue", linewidth = 2, linetype = "dashed")
  )
})

test_that("tsggplot ticks", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(AirPassengers = diff(log(AirPassengers)) * 100)
  theme <- init_tsggplot_theme(
    axis.minor.ticks.length = ggplot2::unit(15, "pt"),
    axis.minor.ticks.x.bottom = element_line(color = "blue", linewidth = 2),
    axis.ticks.length = ggplot2::unit(20, "pt"),
    axis.ticks.x.bottom = element_line(color = "red", linewidth = 3),
    axis.text.x.pos = "start"
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.minor.ticks.length, ggplot2::unit(15, "pt"))

  expect_equal(p$theme$axis.minor.ticks.x.bottom$colour, "blue")
  expect_equal(p$theme$axis.minor.ticks.x.bottom$linewidth, 2)
  expect_null(p$theme$axis.minor.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.minor.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.minor.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.minor.ticks.x.bottom$inherit.blank)

  expect_equal(p$theme$axis.ticks.length, ggplot2::unit(20, "pt"))

  expect_equal(p$theme$axis.ticks.x.bottom$colour, "red")
  expect_equal(p$theme$axis.ticks.x.bottom$linewidth, 3)
  expect_null(p$theme$axis.ticks.x.bottom$linetype)
  expect_null(p$theme$axis.ticks.x.bottom$lineend)
  expect_false(p$theme$axis.ticks.x.bottom$arrow)
  expect_false(p$theme$axis.ticks.x.bottom$inherit.blank)
})

test_that("tsggplot axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.x = element_line(color = "blue", linewidth = 2),
    axis.line.y.left = element_line(color = "red", linewidth = 1),
    axis.line.y.right = element_line(color = "green", linewidth = 3)
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  # Check if the theme attributes are applied correctly
  expect_equal(p$theme$axis.line.x$colour, "blue")
  expect_equal(p$theme$axis.line.x$linewidth, 2)
  expect_null(p$theme$axis.line.x$linetype)
  expect_null(p$theme$axis.line.x$lineend)
  expect_false(p$theme$axis.line.x$arrow)
  expect_false(p$theme$axis.line.x$inherit.blank)

  expect_equal(p$theme$axis.line.y.left$colour, "red")
  expect_equal(p$theme$axis.line.y.left$linewidth, 1)
  expect_null(p$theme$axis.line.y.left$linetype)
  expect_null(p$theme$axis.line.y.left$lineend)
  expect_false(p$theme$axis.line.y.left$arrow)
  expect_false(p$theme$axis.line.y.left$inherit.blank)

  expect_equal(p$theme$axis.line.y.right$colour, "green")
  expect_equal(p$theme$axis.line.y.right$linewidth, 3)
  expect_null(p$theme$axis.line.y.right$linetype)
  expect_null(p$theme$axis.line.y.right$lineend)
  expect_false(p$theme$axis.line.y.right$arrow)
  expect_false(p$theme$axis.line.y.right$inherit.blank)
})

test_that("tsggplot hide y axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.y = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.line.y, "ggplot2::element_blank"))
})

test_that("tsggplot hide x axis", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.x = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.line.x, "ggplot2::element_blank"))
})

test_that("tsggplot y axis overrides left and right", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.line.y = element_line(color = "red")
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.line.y.left$colour, "red")
  expect_null(p$theme$axis.line.y.left$linewidth)
  expect_null(p$theme$axis.line.y.left$linetype)
  expect_null(p$theme$axis.line.y.left$lineend)
  expect_false(p$theme$axis.line.y.left$arrow)
  expect_false(p$theme$axis.line.y.left$inherit.blank)
})

test_that("tsggplot axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text = element_text(color = "blue", size = 10),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.text$colour, "blue")
  expect_equal(p$theme$axis.text$size, 10)
  expect_null(p$theme$axis.text$family)
  expect_null(p$theme$axis.text$face)
  expect_null(p$theme$axis.text$hjust)
  expect_null(p$theme$axis.text$vjust)
  expect_null(p$theme$axis.text$angle)
  expect_null(p$theme$axis.text$lineheight)
  expect_null(p$theme$axis.text$margin)
  expect_null(p$theme$axis.text$debug)
  expect_false(p$theme$axis.text$inherit.blank)
})

test_that("tsggplot hide axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text = element_blank()
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_true(inherits(p$theme$axis.text, "element_blank"))
  expect_true(inherits(p$theme$axis.text.x, "element_blank"))
  expect_true(inherits(p$theme$axis.text.y.left, "element_blank"))
  expect_true(inherits(p$theme$axis.text.y.right, "element_blank"))
})

test_that("tsggplot x and y axis text", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  theme <- init_tsggplot_theme(
    axis.text.x = element_text(color = "blue", size = 10),
    axis.text.x.pos = "mid",
    axis.text.y.left = element_text(color = "green", size = 15),
    axis.text.y.right = element_text(color = "red", size = 20),
  )
  p <- tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$axis.text.x$colour, "blue")
  expect_equal(p$theme$axis.text.x$size, 10)
  expect_equal(c(p$theme$axis.text.x$margin), c(10, 0, 0, 0))
  expect_null(p$theme$axis.text.x$family)
  expect_null(p$theme$axis.text.x$face)
  expect_null(p$theme$axis.text.x$hjust)
  expect_null(p$theme$axis.text.x$vjust)
  expect_null(p$theme$axis.text.x$angle)
  expect_null(p$theme$axis.text.x$lineheight)
  expect_null(p$theme$axis.text.x$debug)
  expect_false(p$theme$axis.text.x$inherit.blank)

  expect_equal(p$theme$axis.text.y.left$colour, "green")
  expect_equal(p$theme$axis.text.y.left$size, 15)
  expect_null(p$theme$axis.text.y.left$family)
  expect_null(p$theme$axis.text.y.left$face)
  expect_null(p$theme$axis.text.y.left$hjust)
  expect_null(p$theme$axis.text.y.left$vjust)
  expect_null(p$theme$axis.text.y.left$angle)
  expect_null(p$theme$axis.text.y.left$lineheight)
  expect_null(p$theme$axis.text.y.left$debug)
  expect_false(p$theme$axis.text.y.left$inherit.blank)

  expect_equal(p$theme$axis.text.y.right$colour, "red")
  expect_equal(p$theme$axis.text.y.right$size, 20)
  expect_null(p$theme$axis.text.y.right$family)
  expect_null(p$theme$axis.text.y.right$face)
  expect_null(p$theme$axis.text.y.right$hjust)
  expect_null(p$theme$axis.text.y.right$vjust)
  expect_null(p$theme$axis.text.y.right$angle)
  expect_null(p$theme$axis.text.y.right$lineheight)
  expect_null(p$theme$axis.text.y.right$debug)
  expect_false(p$theme$axis.text.y.right$inherit.blank)
})

test_that("tsggplot legend", {
  # Plot with left and right axes - lines
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson * 2),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    labs = list(color = "Legend Title"),
    theme = theme
  )

  expect_equal(p$labels$colour, "Legend Title")

  pb <- ggplot_build(p)
  # line 1
  colour_1 <- unique(pb$data[[1]]$colour)
  expect_equal(colour_1, unname(theme$line_colors[1]))
  # line 2
  colour_2 <- unique(pb$data[[2]]$colour)
  expect_equal(colour_2, unname(theme$line_colors[2]))

  # Plot with left as band
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    left_as_band = TRUE,
    theme = theme
  )

  pb <- ggplot_build(p)
  # band
  colour_band_1 <- unique(pb$data[[1]]$fill)
  expect_equal(colour_band_1, unname(theme$band_fill_color[1]))
  colour_band_2 <- unique(pb$data[[2]]$fill)
  expect_equal(colour_band_2, unname(theme$band_fill_color[2]))
  # line
  colour_line_1 <- unique(pb$data[[3]]$colour)
  expect_equal(colour_line_1, unname(theme$line_colors[1]))
  colour_line_2 <- unique(pb$data[[4]]$colour)
  expect_equal(colour_line_2, unname(theme$line_colors[2]))

  # Plot with left as bar
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = JohnsonJohnson, JohnsonJohnson2 = JohnsonJohnson),
    tsr = list(AirPassengers = AirPassengers, AirPassengers2 = AirPassengers * 2),
    left_as_bar = TRUE,
    theme = theme
  )

  pb <- ggplot_build(p)
  # bar
  colour_bar <- unique(pb$data[[1]]$fill)
  expect_equal(colour_bar, unname(theme$bar_fill_color[1:2]))
  # line
  colour_line_1 <- unique(pb$data[[2]]$colour)
  expect_equal(colour_line_1, unname(theme$line_colors[1]))
  colour_line_2 <- unique(pb$data[[3]]$colour)
  expect_equal(colour_line_2, unname(theme$line_colors[2]))
})

test_that("tsggplot modify the legend", {
  # Modify the legend title
  theme <- init_tsggplot_theme(
    legend.title = element_text(size = 20, face = "bold"),
    legend.title.position = "top",
    legend.justification = "left"
  )
  p <- tsggplot(list(AirPassengers = AirPassengers),
    tsr = list(JohnsonJohnson = JohnsonJohnson),
    labs = list(color = "Legend Title"),
    theme = theme
  )

  expect_equal(p$theme$legend.title$size, 20)
  expect_equal(p$theme$legend.title$face, "bold")
  expect_null(p$theme$legend.title$colour)
  expect_null(p$theme$legend.title$family)
  expect_null(p$theme$legend.title$hjust)
  expect_null(p$theme$legend.title$vjust)
  expect_null(p$theme$legend.title$angle)
  expect_null(p$theme$legend.title$lineheight)
  expect_null(p$theme$legend.title$debug)
  expect_false(p$theme$legend.title$inherit.blank)
})

test_that("tsggplot hide legend", {
  theme <- init_tsggplot_theme(legend.position = "none")
  p <- tsggplot(list(AirPassengers = AirPassengers),
    tsr = list(JohnsonJohnson = JohnsonJohnson),
    theme = theme
  )

  expect_equal(p$theme$legend.position, "none")

  # alternative way
  p <- tsggplot(
    AirPassengers = AirPassengers,
    auto_legend = FALSE
  )
  expect_equal(p$theme$legend.position, "none")
})

test_that("tsggplot wrong theme passed", {
  # fails gracefully with tsplot theme
  theme <- init_tsplot_theme()
  expect_error(tsggplot(AirPassengers, theme = theme), "Invalid theme")

  # works with ggplot theme
  theme <- init_tsggplot_theme()
  expect_no_error(tsggplot(AirPassengers, theme = theme))
})

test_that("tsggplot with highlight window", {
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_color = "red",
    highlight_window_alpha = 0.2
  )
  if (capabilities("cairo") && getOption("bitmapType") != "cairo") {
    expect_warning(tsggplot(
      list(AirPassengers = AirPassengers),
      theme = theme
    ), "Transparency requested but current device is not cairo.")
  }

  # Suppress cairo warning
  suppressWarnings(
    p <- tsggplot(list(AirPassengers = AirPassengers),
      theme = theme
    )
  )

  # find the geom_rect layer
  ix <- which(sapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRect")
  ))

  rect <- p$layers[[ix]]
  expect_false(rect$inherit.aes)
  expect_equal(rect$aes_params$fill, "red")
  expect_true(is.na(rect$aes_params$colour))

  # Highlight window with start and end date
  theme <- init_tsggplot_theme(
    highlight_window = TRUE,
    highlight_window_start = c(1959, 1),
    highlight_window_end = c(1971, 1)
  )
  # Suppress cairo warning
  suppressWarnings(
    p <- tsggplot(list(AirPassengers = AirPassengers),
      theme = theme
    )
  )
  # find the geom_rect layer
  ix <- which(sapply(
    p$layers,
    function(l) inherits(l$geom, "GeomRect")
  ))

  rect <- p$layers[[ix]]
  expect_false(rect$inherit.aes)
  expect_equal(rect$aes_params$fill, theme$highlight_window_color)
  expect_true(is.na(rect$aes_params$colour))
})

test_that("tsggplot, with series starting not at start of year", {
  theme <- init_tsggplot_theme()
  p <- tsggplot(list(JohnsonJohnson = window(JohnsonJohnson, start = c(1960, 3))),
    theme = theme
  )

  expect_equal(range(p$layers[[1]]$data$time), c(1960.625, 1980.875))

  # check axis x labels
  pb <- ggplot_build(p)
  txts <- pb$layout$panel_params[[1]]$x$get_labels()
  expect_equal(range(txts), c(1960, 1980))
})

test_that("tsggplot, confidence intervals", {
  theme <- init_tsggplot_theme(
    ci_alpha = 0.2,
    ci_colors = c("red", "blue"),
    ci_legend_label = "%ci_value%% ci for %series% TEST"
  )

  # Define confidence intervals
  ci <- list(
    "KOF Barometer" = list(
      "80" = list(
        lb = KOF$baro_lo_80,
        ub = KOF$baro_hi_80
      ),
      "95" = list(
        lb = KOF$baro_lo_95,
        ub = KOF$baro_hi_95
      )
    )
  )

  p <- tsggplot(list("KOF Barometer" = KOF$baro_point_fc),
    ci = ci,
    theme = theme
  )

  expected_ci_colors <- namedColor2Hex(theme$ci_colors, theme$ci_alpha)

  bd <- ggplot_build(p)$data
  ci_colors <- unique(c(bd[[1]]$fill, bd[[2]]$fill))

  expect_equal(
    ci_colors,
    expected_ci_colors
  )

  fill_scale <- p$scales$get_scales("fill")

  expect_equal(
    fill_scale$labels,
    c(
      "80% ci for KOF Barometer TEST",
      "95% ci for KOF Barometer TEST"
    )
  )
})
