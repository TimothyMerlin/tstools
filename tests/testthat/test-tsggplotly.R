test_that("tsggplotly", {
  tsl <- list(AirPassengers = AirPassengers)
  tsg <- list(JohnsonJohnson = JohnsonJohnson)
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  theme <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold", color = "red"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15)
  )
  expect_no_warning(p <- tsggplot(tsl, tsr = tsg, labs = labs, theme = theme))

  fig <- tsggplotly(p)
  expect_s3_class(fig, "plotly")

  built <- plotly::plotly_build(fig)
  traces <- setNames(built$x$data, sapply(built$x$data, function(d) if (is.null(d$name)) "" else d$name))

  # the custom "value: ..." hover text (not ggplotly's raw default) is
  # present, and matches the true (un-rescaled) series values -- for the
  # secondary-axis series in particular, the trace's y data is rescaled
  # into the left axis's numeric range, so the hover text has to be
  # sourced from the original values instead
  expect_equal(traces[["AirPassengers"]]$hoverinfo, "text")
  expect_equal(
    traces[["AirPassengers"]]$text,
    paste("value:", as.numeric(AirPassengers)),
    ignore_attr = TRUE
  )
  expect_equal(traces[["JohnsonJohnson"]]$hoverinfo, "text")
  expect_equal(
    traces[["JohnsonJohnson"]]$text,
    paste("value:", as.numeric(JohnsonJohnson)),
    ignore_attr = TRUE
  )

  # traces stay one point per observation (text varying per point must not
  # fragment the line into disconnected single-point segments)
  expect_equal(length(traces[["AirPassengers"]]$x), length(AirPassengers))
  expect_equal(length(traces[["JohnsonJohnson"]]$x), length(JohnsonJohnson))

  # a real, non-guessed secondary axis, built from the true right-axis
  # range/ticks attached to the plot as tsggplot_meta
  meta <- attr(p, "tsggplot_meta")
  expect_equal(built$x$layout$yaxis2$title, labs$y_right)
  expect_equal(built$x$layout$yaxis2$range, meta$right_y$y_range)
  expect_equal(built$x$layout$yaxis2$tickvals, meta$right_y$y_ticks)
  expect_equal(built$x$layout$yaxis2$overlaying, "y")

  expect_equal(built$x$layout$font$family, "Verdana")
  expect_equal(built$x$layout$legend$orientation, "h")
})

test_that("tsggplotly", {
  theme <- init_tsggplot_theme(
    axis.line.y = element_line(colour = NA),
    axis.line.x = element_line(colour = NA),
    axis.text.x = element_text(hjust = 0, size = 10),
    axis.text.y.left = element_text(size = 10),
    axis.ticks.x.bottom = element_blank(),
    axis.minor.ticks.x.bottom = aes(colour = NA),
    legend.justification = "left",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15),
    grids_x_show = TRUE,
    axis_x_label_dt = 1,
    text = element_text(family = "sans")
  )
  labs <- list(
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  p <- tsggplot(list("Prognose Frühjahr" = diff(log(AirPassengers)) * 100),
    tsr = list("Niveau, rechte Skala" = AirPassengers),
    left_as_bar = TRUE,
    theme = theme,
    labs = labs
  )
  t <- list(
    family = "Courier New, monospace",
    size = 14,
    color = "blue"
  )

  fig <- tsggplotly(p)
  fig <- plotly::layout(fig, font = t)
  fig <- plotly::layout(
    fig,
    font = t,
    xaxis = list(titlefont = t, tickfont = t),
    yaxis = list(titlefont = t, tickfont = t),
    legend = list(font = t),
    title = list(font = t)
  )

  expect_s3_class(fig, "plotly")

  built <- plotly::plotly_build(fig)
  expect_equal(built$x$layout$font$family, t$family)
  expect_equal(built$x$layout$xaxis$titlefont$family, t$family)
  expect_equal(built$x$layout$yaxis$titlefont$family, t$family)
  expect_equal(built$x$layout$legend$font$family, t$family)

  # left_as_bar + tsr (secondary axis) still produces a real, non-guessed
  # yaxis2 and doesn't error out
  meta <- attr(p, "tsggplot_meta")
  expect_false(is.null(meta$right_y))
  expect_equal(built$x$layout$yaxis2$range, meta$right_y$y_range)
})

test_that("tsggplotly x_tick_mode (#15)", {
  # 40 yearly labels won't all fit at the assumed default width -- this is
  # the same overlap guide_axis(check.overlap = TRUE) hides visually for
  # the static plot, which ggplotly() can't see (it reads the scale's
  # break/label data, not the rendered grob)
  long_ts <- ts(runif(40 * 12), start = c(1950, 1), frequency = 12)
  p <- tsggplot(list(A = long_ts))

  expect_equal(eval(formals(tsggplotly)$x_tick_mode), c("thin", "auto"))

  fig_thin <- tsggplotly(p)
  xa_thin <- fig_thin$x$layout$xaxis
  expect_equal(xa_thin$tickmode, "array")
  # tick marks/gridlines stay at every year...
  expect_equal(length(xa_thin$tickvals), 40)
  # ...but most of the labels text got blanked out to avoid overlap
  expect_true(sum(xa_thin$ticktext == "") > 20)
  expect_true(any(xa_thin$ticktext != ""))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  xa_auto <- fig_auto$x$layout$xaxis
  expect_equal(xa_auto$tickmode, "auto")
  expect_null(xa_auto$tickvals)
  expect_null(xa_auto$ticktext)

  # a short series with few yearly labels shouldn't lose any of them
  short_ts <- ts(runif(5 * 12), start = c(2010, 1), frequency = 12)
  p_short <- tsggplot(list(A = short_ts))
  fig_short <- tsggplotly(p_short)
  xa_short <- fig_short$x$layout$xaxis
  expect_true(all(xa_short$ticktext != ""))
})
