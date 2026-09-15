#' Convert ggplot2 time series object to plotly object
#'
#' @param p ggplot2 figure, as returned by \code{\link{tsggplot}}
#' @param ... additional arguments passed on to \code{\link[plotly]{ggplotly}}
#'
#' @importFrom plotly ggplotly layout
#'
#' @export
tsggplotly <- function(p, ...) {
  meta <- attr(p, "tsggplot_meta")

  # Collect each series' custom hover text ("value: ...") from the line/point
  # layer data before conversion. It's a plain data column, not an aes()
  # mapping (mapping it would make ggplot2 warn "Ignoring unknown
  # aesthetics" on every ordinary, non-plotly tsggplot() call), so ggplotly()
  # has no way to pick it up on its own -- it's injected into the matching
  # trace by name below instead.
  text_by_series <- list()
  for (l in p$layers) {
    d <- l$data
    if (is.data.frame(d) && !is.null(d$text) && !is.null(d$series)) {
      for (s in levels(d$series)) {
        text_by_series[[s]] <- as.character(d$text[as.character(d$series) == s])
      }
    }
  }

  p <- plotly::ggplotly(p, ...)

  for (i in seq_along(p$x$data)) {
    nm <- p$x$data[[i]]$name
    txt <- text_by_series[[nm]]
    if (!is.null(txt) && length(txt) == length(p$x$data[[i]]$x)) {
      p$x$data[[i]]$text <- txt
      p$x$data[[i]]$hoverinfo <- "text"
    }
  }

  layout_args <- list(
    p = p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    xaxis = list(ticks = ""),
    font = list(family = "Verdana"),
    title = list(font = list(family = "Verdana")),
    hoverlabel = list(font = list(family = "Verdana")),
    legend = list(
      font = list(family = "Verdana"),
      orientation = "h",
      x = 0.95,
      y = -0.05,
      xanchor = "right",
      title = list(text = "")
    )
  )

  # The right-axis series are already rescaled into the left axis's numeric
  # range (the same trick ggplot2's sec_axis() relies on for a static plot),
  # so the traces don't need to move to a second y-axis -- overlaying a
  # cosmetic yaxis2 with the *true* right-axis range/ticks over the same
  # panel area reproduces the same dual-axis look plotly-side.
  if (!is.null(meta) && !is.null(meta$right_y)) {
    layout_args$yaxis2 <- list(
      title = meta$y_right_label,
      overlaying = "y",
      side = "right",
      range = meta$right_y$y_range,
      tickvals = meta$right_y$y_ticks,
      automargin = TRUE
    )
  }

  p <- do.call(plotly::layout, layout_args)

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

  p
}
