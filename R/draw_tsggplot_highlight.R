#' @importFrom ggplot2 aes geom_rect .data
draw_tsggplot_highlight <- function(p, global_x, theme, output_format) {
  hlw_start <- theme$highlight_window_start
  if (!any(is.na(hlw_start))) {
    if (!is.list(hlw_start)) {
      hlw_start <- list(hlw_start)
    }
    xl <- sapply(hlw_start, compute_decimal_time, theme$highlight_window_freq)
  } else {
    xl <- global_x$x_range[2] - 2
  }

  hlw_end <- theme$highlight_window_end
  if (!any(is.na(hlw_end))) {
    if (!is.list(hlw_end)) {
      hlw_end <- list(hlw_end)
    }
    xr <- sapply(hlw_end, compute_decimal_time, theme$highlight_window_freq) + 1 / theme$highlight_window_freq
    # highlight window can maximally extend to the end of the x-axis
    if (xr > global_x$x_range[2]) xr <- global_x$x_range[2]
  } else {
    xr <- global_x$x_range[2]
  }

  n_start <- length(xl)
  n_end <- length(xr)

  if (n_start != n_end) {
    warning(sprintf("%s highlight start points than end points specified! Dropping excess ones.", ifelse(n_start > n_end, "More", "Fewer")))
  }

  rect_df <- data.frame(
    xmin = xl,
    xmax = xr,
    ymin = left_y$y_range[1],
    ymax = left_y$y_range[2]
  )

  ## inside your plotting helper
  if (theme$highlight_window_alpha < 1 && capabilities("cairo") &&
    getOption("bitmapType") != "cairo" && output_format == "plot") {
    warning(
      "Transparency requested but current device is not cairo.\n",
      "Hightlight window may not correctly display.\n",
      'Use options(bitmapType = "cairo") to enable cairo support.'
    )
  } else if (!capabilities("cairo")) {
    warning(
      "Transparency will not render correctly.\n",
      "Hightlight window may not correctly display.\n",
      "Consider using a Cairo device."
    )
  }

  p <- p + geom_rect(
    data = rect_df,
    aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax,
      alpha = theme$highlight_window_alpha,
    ),
    fill = theme$highlight_window_color,
    color = NA,
    inherit.aes = FALSE,
    show.legend = FALSE
  )
}
