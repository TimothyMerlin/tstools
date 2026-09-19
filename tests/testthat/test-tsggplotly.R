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
  expect_equal(built$x$layout$yaxis2$title$text, labs$y_right)
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

test_that("tsggplotly warns about a theme font with no CSS fallback stack", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)

  # a common, essentially-always-installed font: no warning
  p_safe <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Georgia")))
  expect_no_warning(tsggplotly(p_safe))

  # one of the R/ggplot2 aliases this function already maps to a full CSS
  # stack: no warning either
  p_alias <- tsggplot(list(A = long_ts), theme = init_tsggplot_theme(text = ggplot2::element_text(family = "sans")))
  expect_no_warning(tsggplotly(p_alias))

  # a font the user already gave a fallback stack for: no warning
  p_stack <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Comic Sans MS, cursive"))
  )
  expect_no_warning(tsggplotly(p_stack))

  # an obscure/decorative font with no fallback: warn, since the viewer's
  # browser will otherwise just silently substitute its own default instead
  # of the theme's actual intended font if it isn't installed
  p_risky <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(text = ggplot2::element_text(family = "Comic Sans MS"))
  )
  expect_warning(tsggplotly(p_risky), "Comic Sans MS")
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

test_that("tsggplotly gives left, right and secondary axes matching, theme-derived line styling", {
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    axis.line.x = ggplot2::element_line(colour = "#123456", linewidth = 2),
    axis.line.y.left = ggplot2::element_line(colour = "#123456", linewidth = 2),
    axis.line.y.right = ggplot2::element_line(colour = "#123456", linewidth = 2)
  )
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p))

  for (ax in list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2)) {
    expect_true(ax$showline)
    expect_equal(ax$linecolor, "#123456")
  }
  # all three got the same linewidth conversion, not just whichever one
  # ggplotly() happened to compute correctly on its own
  expect_equal(built$x$layout$xaxis$linewidth, built$x$layout$yaxis$linewidth)
  expect_equal(built$x$layout$yaxis$linewidth, built$x$layout$yaxis2$linewidth)

  # element_blank() hides the line instead of leaving some stale default
  p_blank <- tsggplot(
    list(A = long_ts),
    theme = init_tsggplot_theme(axis.line.y.left = ggplot2::element_blank())
  )
  built_blank <- plotly::plotly_build(tsggplotly(p_blank))
  expect_false(built_blank$x$layout$yaxis$showline)
})

test_that("tsggplotly gives left, right and secondary axes matching, theme-derived tick label fonts", {
  # ggplotly() derives tick label font size/colour by measuring rendered
  # grobs, which is unreliable across axes -- confirmed by comparing against
  # a plain vanilla ggplot2 plot, where left/right came out identical: for
  # this exact same (evenly-themed, size = 10 both sides) plot, it measured
  # the x-axis tick labels as ~50% bigger than the y-axis ones. Read the
  # theme directly instead, like the axis line/colour case above.
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    axis.text.x = ggplot2::element_text(size = 10),
    axis.text.y.left = ggplot2::element_text(size = 10),
    axis.text.y.right = ggplot2::element_text(size = 10)
  )
  p <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), labs = list(y_right = "right"), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p))

  sizes <- vapply(
    list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2),
    function(ax) ax$tickfont$size,
    numeric(1)
  )
  expect_equal(sizes[1], sizes[2])
  expect_equal(sizes[2], sizes[3])

  colors <- vapply(
    list(built$x$layout$xaxis, built$x$layout$yaxis, built$x$layout$yaxis2),
    function(ax) ax$tickfont$color,
    character(1)
  )
  expect_equal(colors[1], colors[2])
  expect_equal(colors[2], colors[3])

  # axis.text colour ("grey30"-ish by ggplot2 default) isn't set anywhere in
  # tsggplot's own theme, so this only comes out right if the *inherited*
  # default is resolved (e.g. via ggplot2::complete_theme()), not just the
  # theme's own explicitly-set elements.
  expect_equal(colors[1], "rgba(77,77,77,1)")

  # right-axis title text still carries a font (not silently dropped when
  # building it defensively around the "no right label" case)
  expect_false(is.null(built$x$layout$yaxis2$title$font))

  # a tsr plot with no right-axis label doesn't crash plotly_build() (this
  # used to error: "attempt to set an attribute on NULL", from embedding a
  # bare NULL as a nested list(text = NULL, font = NULL) value)
  p_no_label <- tsggplot(list(A = long_ts), tsr = list(B = long_ts + 1), theme = theme)
  expect_no_error(plotly::plotly_build(tsggplotly(p_no_label)))
})

test_that("tsggplotly reflects custom data-line and gridline styling from the theme", {
  # these aren't something tsggplotly() touches itself (ggplotly() converts
  # the geoms/panel grid natively), but worth locking in given the axis
  # line case above shows theme styling silently not making it through is
  # a real failure mode, not just a hypothetical one
  long_ts <- ts(runif(30), start = c(2000, 1), frequency = 1)
  theme <- init_tsggplot_theme(
    line_colors = c("#FF00FF"),
    linewidth = 4,
    panel.grid.major.y = ggplot2::element_line(colour = "#00FF00", linewidth = 3)
  )
  p <- tsggplot(list(A = long_ts), theme = theme)
  built <- plotly::plotly_build(tsggplotly(p))

  expect_equal(built$x$data[[1]]$line$color, "rgba(255,0,255,1)")
  expect_true(built$x$data[[1]]$line$width > 10) # default is ~2-4
  expect_equal(built$x$layout$yaxis$gridcolor, "rgba(0,255,0,1)")
  expect_true(built$x$layout$yaxis$gridwidth > 2) # default is < 1
})

test_that("tsggplotly converts stacked, grouped and sum_as_line bar charts", {
  tsb1 <- ts(runif(20, -30, 20), start = c(2010, 1), frequency = 4)
  tsb2 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)
  tsb3 <- ts(runif(20, 0, 50), start = c(2010, 1), frequency = 4)

  p_stacked <- tsggplot(tsb1, tsb2, tsb3, left_as_bar = TRUE)
  expect_no_error(fig_stacked <- tsggplotly(p_stacked))
  built_stacked <- plotly::plotly_build(fig_stacked)
  bar_traces <- Filter(function(d) identical(d$type, "bar"), built_stacked$x$data)
  expect_equal(length(bar_traces), 3)
  expect_true(all(vapply(bar_traces, function(d) length(d$x) == length(tsb1), logical(1))))

  p_grouped <- tsggplot(tsb1, tsb2, tsb3, left_as_bar = TRUE, group_bar_chart = TRUE)
  expect_no_error(fig_grouped <- tsggplotly(p_grouped))
  built_grouped <- plotly::plotly_build(fig_grouped)
  bar_traces_grouped <- Filter(function(d) identical(d$type, "bar"), built_grouped$x$data)
  # dodged, not stacked: each series' bars sit at their own offset x
  # positions rather than all three sharing the same x
  x_by_trace <- lapply(bar_traces_grouped, function(d) d$x[1])
  expect_equal(length(unique(x_by_trace)), 3)

  p_sum <- tsggplot(list(tsb1, tsb2, tsb3), left_as_bar = TRUE, theme = init_tsggplot_theme(sum_as_line = TRUE))
  expect_no_error(fig_sum <- tsggplotly(p_sum))
  built_sum <- plotly::plotly_build(fig_sum)
  expect_equal(length(Filter(function(d) identical(d$type, "bar"), built_sum$x$data)), 3)
  expect_true(length(Filter(function(d) identical(d$type, "scatter"), built_sum$x$data)) >= 1)
})
