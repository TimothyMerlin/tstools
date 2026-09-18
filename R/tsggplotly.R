#' Estimate which x-axis tick labels would overlap and blank them out
#'
#' \code{tsggplot()}'s \code{guide_axis(check.overlap = TRUE)} (see #13) only
#' thins labels visually at grid draw time -- it never touches the ggplot
#' object's scale data, so \code{plotly::ggplotly()} (which reads that data,
#' not the rendered grob) always bakes every tick label into an explicit
#' \code{tickvals}/\code{ticktext} array, which Plotly then renders
#' unconditionally regardless of overlap (see #15). This replicates the same
#' greedy left-to-right overlap check ggplot2 itself uses, based on actual
#' text metrics for the assumed render width, rather than a fixed skip-every-
#' Nth heuristic.
#'
#' @param tickvals numeric tick positions, in the same units as \code{x_range}
#' @param ticktext character tick labels, one per \code{tickvals}
#' @param x_range numeric length-2 vector, the axis range \code{tickvals} sits in
#' @param width_in numeric, assumed plot width in inches used only to decide
#'   which labels are far enough apart to both show -- Plotly widgets are
#'   still fully resizable/zoomable afterwards
#' @param fontsize_pt numeric tick label font size in points
#' @param pad_in numeric minimum gap left between adjacent kept labels, in inches
#' @importFrom grDevices dev.cur dev.off dev.set pdf
#' @importFrom grid convertWidth grobWidth textGrob gpar
#' @noRd
thin_plotly_x_labels <- function(tickvals, ticktext, x_range, width_in = 7,
                                 fontsize_pt = 11, pad_in = 0.08) {
  if (length(tickvals) < 2) {
    return(ticktext)
  }

  ord <- order(tickvals)
  vals <- tickvals[ord]
  txt <- ticktext[ord]

  pos_in <- (vals - x_range[1]) / diff(x_range) * width_in

  cur_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    if (cur_dev > 1) grDevices::dev.set(cur_dev)
  })

  label_w_in <- vapply(txt, function(l) {
    if (!nzchar(l)) {
      return(0)
    }
    grid::convertWidth(
      grid::grobWidth(grid::textGrob(l, gp = grid::gpar(fontsize = fontsize_pt))),
      "in",
      valueOnly = TRUE
    )
  }, numeric(1))

  keep <- logical(length(txt))
  last_right <- -Inf
  for (i in seq_along(txt)) {
    left <- pos_in[i] - label_w_in[i] / 2
    if (i == 1 || left >= last_right + pad_in) {
      keep[i] <- TRUE
      last_right <- pos_in[i] + label_w_in[i] / 2
    }
  }
  txt[!keep] <- ""

  out <- character(length(txt))
  out[ord] <- txt
  out
}

#' Reconstruct real dates/datetimes from tsggplot()'s numeric x-axis positions
#'
#' \code{tsggplot()} plots x as a plain number for every frequency: days-
#' since-epoch for daily/weekly, seconds-since-epoch for hourly (the same
#' numbers \code{scale_x_date()}/\code{scale_x_datetime()} use internally),
#' decimal-year (\code{year + fraction_of_year}) for everything else (see
#' \code{getNumericTimeIndex()} in utils.R). \code{plotly::ggplotly()} always
#' flattens this to a plain "linear" axis regardless -- even the daily/
#' weekly/hourly cases lose their Date/POSIXct typing on conversion -- so
#' Plotly's auto zoom/tick logic sees bare numbers like 2020.5 with no
#' notion of dates. This inverts whichever encoding back to a real
#' date/datetime string so the x-axis can be declared \code{type = "date"}
#' instead, letting Plotly's own date-aware tick formatter take over when
#' zooming.
#'
#' Exact for the daily/weekly (days-since-epoch) and hourly (seconds-since-
#' epoch) cases. For the decimal-year case, this is approximate to within
#' about a day (ts's \code{k/frequency} fractions and this day-of-year
#' fraction don't perfectly agree) and includes whatever line_to_middle
#' half-period shift was already baked into the plotted position -- both
#' irrelevant for picking sensible zoomed tick labels, which is all this is
#' used for.
#'
#' @param x numeric, tsggplot()'s plotted x positions
#' @param dominant_freq character, \code{meta$global_x$dominant_freq} --
#'   which numeric encoding \code{x} is in (see above)
#' @noRd
tsggplotly_numeric_x_to_date <- function(x, dominant_freq) {
  if (isTRUE(dominant_freq %in% c("daily", "weekly"))) {
    return(as.Date(floor(x), origin = "1970-01-01"))
  }
  if (isTRUE(dominant_freq == "hourly")) {
    return(format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S"))
  }
  year <- floor(x)
  frac <- x - year
  days_in_year <- ifelse(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0), 366, 365)
  as.Date(sprintf("%d-01-01", year)) + round(frac * days_in_year)
}

#' Recursively remap R/ggplot2 font family aliases to real CSS font stacks
#'
#' \code{plotly::ggplotly()} sets an explicit \code{family} on many
#' individual elements (axis titles, tick labels, ...) inherited straight
#' from the static plot's theme -- \code{"sans"}/\code{"serif"}/\code{"mono"}
#' aren't valid CSS, so those would otherwise keep falling back to whatever
#' the browser's own default is, even after setting a real font stack on the
#' top-level \code{layout$font}, which only covers elements that don't
#' already have their own explicit \code{family}.
#'
#' @param x a (possibly deeply nested) list, e.g. \code{p$x$layout}
#' @param aliases named character vector mapping alias -> real CSS font stack
#' @noRd
fix_font_family_aliases <- function(x, aliases) {
  if (!is.list(x)) {
    return(x)
  }
  if (is.character(x$family) && length(x$family) == 1 && x$family %in% names(aliases)) {
    x$family <- aliases[[x$family]]
  }
  for (i in seq_along(x)) {
    x[[i]] <- fix_font_family_aliases(x[[i]], aliases)
  }
  x
}

#' Convert ggplot2 time series object to plotly object
#'
#' @param p ggplot2 figure, as returned by \code{\link{tsggplot}}. If it has
#'   a \code{tsr} (right-axis) series with the default split left/right
#'   legend (which can't be converted to plotly), the merged-legend
#'   equivalent \code{tsggplot()} built alongside it is used instead.
#' @param ... additional arguments passed on to \code{\link[plotly]{ggplotly}}
#' @param x_tick_mode character, how to avoid overlapping x-axis tick labels
#'   in the interactive plot (see #13, #15). \code{"thin"} (the default)
#'   estimates which yearly labels would overlap at the given/assumed width
#'   and blanks them out, the same way the static \code{tsggplot()} plot
#'   does, while still drawing every tick mark. \code{"auto"} instead hands
#'   tick placement entirely to Plotly (\code{tickmode = "auto"}) on a real
#'   date-typed x-axis, so Plotly recomputes "nice", properly date-formatted
#'   tick positions dynamically as the plot is resized or zoomed (e.g.
#'   month/day labels once zoomed in, instead of decimal years like
#'   2020.5), at the cost of no longer aligning ticks to exact year starts.
#'
#' @importFrom ggplot2 calc_element
#' @importFrom plotly ggplotly layout
#'
#' @export
tsggplotly <- function(p, ..., x_tick_mode = c("thin", "auto")) {
  x_tick_mode <- match.arg(x_tick_mode)
  dots <- list(...)
  meta <- attr(p, "tsggplot_meta")

  # tsggplotly() can't convert the ggnewscale-based split legend (the
  # split-off geom silently loses its data in plotly::ggplotly()) -- use
  # the merged-legend equivalent tsggplot() already built for this instead,
  # transparently.
  if (isTRUE(meta$split_legend) && !is.null(meta$merged_legend_fallback)) {
    p <- meta$merged_legend_fallback
    meta <- attr(p, "tsggplot_meta")
  }

  # ggplot2/R's generic font family aliases ("sans", "serif", "mono") are
  # graphics-device names, not valid CSS -- a browser doesn't recognize
  # "sans" and silently falls back to its own default (often a serif font),
  # rather than matching the static plot's actual sans-serif look.
  css_family_aliases <- c(
    sans = "Arial, Helvetica, sans-serif",
    serif = "Times New Roman, Times, serif",
    mono = "Courier New, Courier, monospace"
  )
  to_css_family <- function(x) {
    if (is.character(x) && x %in% names(css_family_aliases)) css_family_aliases[[x]] else x
  }

  text_family <- to_css_family(p$theme$text$family)
  if (is.null(text_family) || !nzchar(text_family)) {
    text_family <- css_family_aliases[["sans"]]
  }

  # Resolve a fill colour from a theme element (e.g. panel.background),
  # falling back to fully transparent when the theme leaves it unset --
  # matching this function's previous hardcoded default for plots that
  # don't customize it, while following the theme for ones that do.
  resolve_fill <- function(fill) {
    if (is.null(fill) || is.na(fill) || !nzchar(fill)) "rgba(0,0,0,0)" else fill
  }
  plot_bg <- resolve_fill(ggplot2::calc_element("plot.background", p$theme)$fill)
  panel_bg <- resolve_fill(ggplot2::calc_element("panel.background", p$theme)$fill)

  # ggplotly() sets its own axis line style per axis, inconsistently (e.g.
  # the left y-axis came out black instead of the theme's actual grey, and
  # the overlaid yaxis2 gets no line at all since it starts out undefined)
  # -- read the theme directly instead for all three axes. Captured here,
  # before p gets reassigned to the converted plotly object below -- a
  # closure referencing p$theme directly would instead see that later,
  # theme-less value once actually called.
  static_theme <- p$theme
  resolve_axis_line <- function(theme_key) {
    el <- ggplot2::calc_element(theme_key, static_theme)
    if (is.null(el) || inherits(el, "element_blank")) {
      return(list(showline = FALSE))
    }
    list(
      showline = TRUE,
      linecolor = el$colour,
      # ggplot2 linewidth -> plotly's pixel-based line width
      linewidth = (if (is.null(el$linewidth)) 0.5 else as.numeric(el$linewidth)) * 96 / 72.27
    )
  }

  legend_position <- p$theme$legend.position

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
  p$x$layout <- fix_font_family_aliases(p$x$layout, css_family_aliases)
  p$x$data <- fix_font_family_aliases(p$x$data, css_family_aliases)

  xa <- p$x$layout$xaxis
  if (identical(xa$tickmode, "array") && length(xa$tickvals) > 1) {
    if (x_tick_mode == "auto") {
      p$x$layout$xaxis$tickmode <- "auto"
      p$x$layout$xaxis$tickvals <- NULL
      p$x$layout$xaxis$ticktext <- NULL
      p$x$layout$xaxis$categoryorder <- NULL
      p$x$layout$xaxis$categoryarray <- NULL

      # Give Plotly real dates instead of tsggplot()'s plain numeric x (see
      # tsggplotly_numeric_x_to_date()), so its own zoom-aware tick
      # formatter can show month/day labels once zoomed in, rather than
      # decimal years like 2020.5 at every zoom level.
      dominant_freq <- meta$global_x$dominant_freq
      if (!identical(xa$type, "date")) {
        p$x$layout$xaxis$type <- "date"
        if (!is.null(xa$range)) {
          p$x$layout$xaxis$range <- as.character(tsggplotly_numeric_x_to_date(xa$range, dominant_freq))
        }
        for (i in seq_along(p$x$data)) {
          trace_x <- p$x$data[[i]]$x
          if (is.numeric(trace_x)) {
            p$x$data[[i]]$x <- as.character(tsggplotly_numeric_x_to_date(trace_x, dominant_freq))
          }
        }
      }
    } else {
      width_px <- if (is.null(dots$width)) 700 else dots$width
      fontsize_pt <- if (is.null(xa$tickfont$size)) 11 else xa$tickfont$size
      p$x$layout$xaxis$ticktext <- thin_plotly_x_labels(
        xa$tickvals, xa$ticktext, xa$range,
        width_in = width_px / 96,
        fontsize_pt = fontsize_pt
      )

      # tickvals/ticktext sit at mid_pts (label positions, centered within
      # each year -- see the Global X-Axis section of tsggplot.R) rather
      # than the true year-start positions, which only exist as the
      # static plot's separate minor-tick axis (guide_axis(minor.ticks =
      # TRUE)) -- something ggplotly() has no concept of and drops
      # entirely. Since this plot draws no gridlines/ticks of its own, and
      # thin_plotly_x_labels() blanks most labels for longer series, a mark
      # is drawn at each real year-start position (meta$global_x$
      # yearly_tick_pos) as a manual shape instead, matching what the
      # static plot's minor ticks show regardless of label thinning.
      yearly_x <- meta$global_x$yearly_tick_pos
      if (!is.null(yearly_x)) {
        yearly_x <- as.numeric(yearly_x)
        yearly_x <- yearly_x[yearly_x >= xa$range[1] & yearly_x <= xa$range[2]]
        tick_shapes <- lapply(yearly_x, function(xv) {
          list(
            type = "line",
            xref = "x", yref = "y domain",
            x0 = xv, x1 = xv, y0 = 0, y1 = 0.015,
            line = list(color = xa$tickcolor, width = xa$tickwidth)
          )
        })
        # plotly::layout()'s later merge (see layout_args below) only
        # queues changes into p$x$layoutAttrs, applied at build/print time
        # via a per-field merge that mishandles a plain (unnamed) list like
        # this one -- direct assignment here, like the ticktext mutation
        # above, actually sticks.
        p$x$layout$shapes <- tick_shapes
      }
    }
  }

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
    paper_bgcolor = plot_bg,
    plot_bgcolor = panel_bg,
    font = list(family = text_family),
    title = list(font = list(family = text_family)),
    hoverlabel = list(font = list(family = text_family)),
    # ggplotly() already derives orientation/x/y/xanchor for "bottom",
    # "top", "left" and "right" straight from theme$legend.position, so
    # those are left alone here; only "none" needs a manual assist, since
    # ggplotly() doesn't act on it (the legend and its trace entries stay
    # visible otherwise).
    showlegend = !identical(legend_position, "none"),
    legend = list(
      font = list(family = text_family),
      title = list(text = "")
    )
  )

  p <- do.call(plotly::layout, layout_args)

  # Axis line style (showline/linecolor/linewidth) is set by direct
  # assignment, not via the layout_args/plotly::layout() call above --
  # plotly::layout() only queues changes into p$x$layoutAttrs, and its
  # later merge doesn't reliably override values ggplotly() already set on
  # xaxis/yaxis during conversion (e.g. it left the left y-axis black
  # instead of the theme's actual colour).
  p$x$layout$xaxis <- modifyList(p$x$layout$xaxis, c(list(ticks = ""), resolve_axis_line("axis.line.x")))
  p$x$layout$yaxis <- modifyList(p$x$layout$yaxis, resolve_axis_line("axis.line.y.left"))

  # The right-axis series are already rescaled into the left axis's numeric
  # range (the same trick ggplot2's sec_axis() relies on for a static plot),
  # so the traces don't need to move to a second y-axis -- overlaying a
  # cosmetic yaxis2 with the *true* right-axis range/ticks over the same
  # panel area reproduces the same dual-axis look plotly-side.
  if (!is.null(meta) && !is.null(meta$right_y)) {
    p$x$layout$yaxis2 <- modifyList(
      if (is.null(p$x$layout$yaxis2)) list() else p$x$layout$yaxis2,
      c(
        list(
          title = meta$y_right_label,
          overlaying = "y",
          side = "right",
          range = meta$right_y$y_range,
          tickvals = meta$right_y$y_ticks,
          automargin = TRUE,
          # Plotly draws a reference line at 0 on every axis by default; the
          # static plot doesn't, and it's not meaningful here since 0 on this
          # axis has no special significance beyond being part of the range.
          zeroline = FALSE
        ),
        resolve_axis_line("axis.line.y.right")
      )
    )
  }

  if (!is.null(meta) && !is.null(meta$right_y)) {
    # Plotly.js won't actually render an axis with no trace bound to it --
    # ticks, axis line and title all silently disappear -- even though it's
    # fully defined in the layout above. Add one invisible point on yaxis2
    # so the overlaid secondary axis actually shows up.
    p <- plotly::add_trace(p,
      x = p$x$layout$xaxis$range[1], y = mean(meta$right_y$y_range),
      yaxis = "y2", type = "scatter", mode = "markers",
      marker = list(opacity = 0), showlegend = FALSE,
      hoverinfo = "skip", inherit = FALSE
    )
  }

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
