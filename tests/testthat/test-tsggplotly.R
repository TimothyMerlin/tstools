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
    plot.tag = element_text(size = 15),
    # tsggplotly() can't convert the split (ggnewscale-based) legend, so
    # charts headed there need the merged legend (see tsggplotly()'s guard).
    legend_all_left = TRUE
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

  # Plotly.js doesn't actually render an axis with no trace bound to it
  # (ticks/line/title silently disappear), even though it's fully defined
  # in the layout -- an invisible point on yaxis2 keeps it visible
  yaxis2_traces <- Filter(function(d) identical(d$yaxis, "y2"), built$x$data)
  expect_equal(length(yaxis2_traces), 1)
  expect_equal(yaxis2_traces[[1]]$marker$opacity, 0)

  # the interactive plot's font follows the theme passed to tsggplot()
  # (mapped to a real CSS font stack -- "sans" alone isn't valid CSS and
  # browsers silently fall back to a serif font for it)
  expect_equal(built$x$layout$font$family, "Arial, Helvetica, sans-serif")
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

  # "auto" mode gives Plotly a real date-typed x-axis (instead of tsggplot's
  # plain decimal-year numbers) so its own zoom/tick logic can format ticks
  # as dates -- "thin" mode is untouched, still plain numeric.
  expect_equal(xa_auto$type, "date")
  expect_equal(xa_thin$type, "linear")
  built_auto_data <- plotly::plotly_build(fig_auto)$x$data[[1]]
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", built_auto_data$x)))
  expect_equal(as.integer(format(as.Date(built_auto_data$x[1]), "%Y")), 1950)

  # a short series with few yearly labels shouldn't lose any of them
  short_ts <- ts(runif(5 * 12), start = c(2010, 1), frequency = 12)
  p_short <- tsggplot(list(A = short_ts))
  fig_short <- tsggplotly(p_short)
  xa_short <- fig_short$x$layout$xaxis
  expect_true(all(xa_short$ticktext != ""))

  # tickvals/ticktext sit at mid_pts (label positions, centered within each
  # year), not the true year-start positions -- so blanked-or-not, tick
  # marks are drawn separately as shapes at the real year starts
  # (meta$global_x$yearly_tick_pos), matching the static plot's minor
  # ticks. Built explicitly (plotly::layout()'s own merge mishandles a
  # plain list like "shapes", so tsggplotly() assigns it directly).
  meta <- attr(p, "tsggplot_meta")
  built_thin <- plotly::plotly_build(fig_thin)
  shape_x <- sort(vapply(built_thin$x$layout$shapes, function(s) s$x0, numeric(1)))
  expect_equal(shape_x, sort(as.numeric(meta$global_x$yearly_tick_pos)))

  # "auto" mode hands ticks to Plotly entirely -- no manual shapes needed
  built_auto <- plotly::plotly_build(fig_auto)
  expect_equal(length(built_auto$x$layout$shapes), 0)
})

test_that("tsggplotly x_tick_mode = 'auto' reconstructs exact dates for daily/weekly series", {
  idx <- seq(as.Date("2020-01-01"), by = "day", length.out = 10)
  daily_xts <- xts::xts(seq_along(idx), order.by = idx)
  p <- tsggplot(list(A = daily_xts))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  built <- plotly::plotly_build(fig_auto)
  expect_equal(built$x$layout$xaxis$type, "date")
  # days-since-epoch encoding inverts exactly, no line_to_middle drift
  expect_equal(as.Date(built$x$data[[1]]$x), idx)
})

test_that("tsggplotly follows the theme's background and legend position", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  # defaults: transparent backgrounds, legend visible
  p_default <- tsggplot(list(A = long_ts))
  built_default <- plotly::plotly_build(tsggplotly(p_default))
  expect_equal(built_default$x$layout$paper_bgcolor, "rgba(0,0,0,0)")
  expect_equal(built_default$x$layout$plot_bgcolor, "rgba(0,0,0,0)")
  expect_true(built_default$x$layout$showlegend)

  # custom plot/panel background colours carry over
  p_bg <- tsggplot(list(A = long_ts),
    theme = init_tsggplot_theme(
      plot.background = ggplot2::element_rect(fill = "grey90"),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  )
  built_bg <- plotly::plotly_build(tsggplotly(p_bg))
  expect_equal(built_bg$x$layout$paper_bgcolor, "grey90")
  expect_equal(built_bg$x$layout$plot_bgcolor, "white")

  # legend.position = "none" actually hides the legend (ggplotly() alone
  # does not act on this)
  p_none <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(legend.position = "none"))
  built_none <- plotly::plotly_build(tsggplotly(p_none))
  expect_false(built_none$x$layout$showlegend)

  # legend.position = "right" is left to ggplotly()'s own derivation
  # instead of forcing a hardcoded bottom-right position
  p_right <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(legend.position = "right"))
  built_right <- plotly::plotly_build(tsggplotly(p_right))
  expect_null(built_right$x$layout$legend$orientation)
})

test_that("tsggplotly x_tick_mode = 'auto' reconstructs real datetimes for hourly series", {
  hourly_idx <- seq(as.POSIXct("2023-01-01", tz = "UTC"), by = "hour", length.out = 5 * 24)
  hourly_xts <- xts::xts(seq_along(hourly_idx), order.by = hourly_idx)
  p <- tsggplot(list(A = hourly_xts), theme = init_tsggplot_theme(fill_year_with_nas = FALSE))

  fig_auto <- tsggplotly(p, x_tick_mode = "auto")
  built <- plotly::plotly_build(fig_auto)
  expect_equal(built$x$layout$xaxis$type, "date")
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", built$x$data[[1]]$x)))
  expect_equal(format(as.Date(built$x$data[[1]]$x[1])), "2023-01-01")

  # "thin" mode isn't affected -- doesn't error and still produces the
  # correct (already date-formatted) tick labels
  fig_thin <- tsggplotly(p)
  built_thin <- plotly::plotly_build(fig_thin)
  expect_false(anyNA(built_thin$x$layout$xaxis$ticktext))
})

test_that("tsggplotly maps R's generic font family aliases to real CSS font stacks", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  for (alias in c("sans", "serif", "mono")) {
    p <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = alias)))
    built <- plotly::plotly_build(tsggplotly(p))
    expect_match(built$x$layout$font$family, "^[A-Za-z ]+(, [A-Za-z ]+)*, (sans-serif|serif|monospace)$")
    expect_false(identical(built$x$layout$font$family, alias))
  }

  # an already-real font name passes through unchanged
  p_custom <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Georgia")))
  built_custom <- plotly::plotly_build(tsggplotly(p_custom))
  expect_equal(built_custom$x$layout$font$family, "Georgia")
})

test_that("tsggplotly fixes font aliases on individual elements too, not just the global default", {
  # plotly::ggplotly() sets its own explicit "sans" family on axis titles/
  # tick labels (inherited from the static theme), which otherwise
  # overrides the global layout$font default we set -- a real CSS font
  # stack has to reach these individually too.
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"))
  built <- plotly::plotly_build(tsggplotly(p))

  not_sans <- function(family) !identical(family, "sans") && grepl("sans-serif", family)
  expect_true(not_sans(built$x$layout$yaxis$title$font$family))
  expect_true(not_sans(built$x$layout$yaxis$tickfont$family))
  expect_true(not_sans(built$x$layout$xaxis$tickfont$family))
})
