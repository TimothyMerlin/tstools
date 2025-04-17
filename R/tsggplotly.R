#' Convert ggplot2 time series object to plotly object
#'
#' @import plotly
#'
#' @export
tsggplotly <- function(p, ...) {
  p <- ggplotly(p)
  p <- layout(p,
    xaxis = list(ticks = ""),
    font = list(family = "Verdana"),
    title = list(font = list(family = "Verdana")),
    legend = list(
      font = list(family = "Verdana"),
      orientation = "h",
      x = 0.95,
      y = -0.05,
      xanchor = "right",
      title = list(text = "")
    )
  )

  # rename legend items
  for (i in seq_along(p$x$data)) {
    name <- p$x$data[[i]]$name

    if (!is.null(name)) {
      # Remove leading/trailing parentheses
      name <- gsub("^\\(|\\)$", "", name)

      # Remove trailing ", 1" or ",\\s*\\d+"
      name <- sub(",\\s*\\d+$", "", name)

      p$x$data[[i]]$name <- name
    }
  }

  # p <- plotly::layout(
  #  p,
  #  font = theme$font,
  #  barmode = "group",
  #  xaxis = list(
  #    showgrid = TRUE,
  #    tickvals = x_axis_ticks$year,
  #    tickfont = theme$xaxis$tickfont,
  #    tickmode = "array",
  #    title = "",
  #    range = c(
  #      theme$xaxis$range$start - x_range_padding,
  #      theme$xaxis$range$end + x_range_padding
  #    )
  #  ),
  #  annotations = annotations,
  #  yaxis = list(
  #    title = theme$yaxis$y$title,
  #    tickfont = theme$yaxis$tickfont,
  #    side = "left",
  #    range = c(
  #      optimal_y_ticks$extremas$growth$min,
  #      optimal_y_ticks$extremas$growth$max
  #    ),
  #    tickvals = optimal_y_ticks$y1_tickvals,
  #    ticktext = round(optimal_y_ticks$y1_tickvals, 1)
  #  ),
  #  yaxis2 = list(
  #    title = theme$yaxis$y2$title,
  #    tickfont = theme$yaxis$tickfont,
  #    side = "right",
  #    range = c(
  #      optimal_y_ticks$extremas$level$min,
  #      optimal_y_ticks$extremas$level$max
  #    ),
  #    overlaying = "y",
  #    automargin = TRUE,
  #    tickvals = optimal_y_ticks$y2_tickvals,
  #    ticktext = optimal_y_ticks$y2_ticktext
  #  ),
  #  legend = list(
  #    font = theme$legend$font,
  #    bgcolor = "rgba(0,0,0,0)",
  #    orientation = "h", # horizontal orientation
  #    x = 0, # center the legend
  #    y = -0.35, # position below the x-axis
  #    xanchor = "left", # anchor at the center
  #    yanchor = "top", # anchor at the top (bottom of the plot)
  #    traceorder = "normal", # order as they appear in the traces
  #    tracegroupgap = 0, # gap between trace groups
  #    itemsizing = "constant", # all items same size
  #    itemwidth = 30, # width of each legend item
  #    itemclick = "toggleothers", # only one item active at a time
  #    valign = "top", # align vertically at the top
  #    roworder = "top to bottom", # order of legend items
  #    ncol = 3 # number of columns
  #  ),
  #  hovermode = "closest"
  # )

  p
}
