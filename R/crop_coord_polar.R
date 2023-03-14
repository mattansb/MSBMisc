#' Crop `coord_polar`
#'
#'
#' @param plot A `ggplot`
#' @param start,end The angular values (in radians) marking where the plot
#'   starts and ends.
#' @param padding How much padding to add around the crop?
#' @param fix_aspect.ratio Should the aspect ratio be fixed?
#'
#' @details This function uses
#'   [`{ggtrace}`](https://yjunechoe.github.io/ggtrace/) by June Choe, adapted
#'   from [this twitter thread](https://twitter.com/yjunechoe/status/1506672410208051201).
#'
#' @examplesIf require("ggplot2") && require("ggtrace")
#'
#' library(ggplot2)
#'
#' polar_plot <- ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   expand_limits(y = c(0, 60)) +
#'   coord_polar(start = 0, theta = "y")
#'
#' crop_coord_polar(polar_plot, end = pi)
#' crop_coord_polar(polar_plot, end = pi / 2)
#' crop_coord_polar(polar_plot, start = 3 * pi / 2, end = pi / 2)
#'
#'
#'
#' # Also works with facets!
#' d <- data.frame(
#'   x = seq(1, 7, length = 6 * 5),
#'   y = rnorm(6 * 5),
#'   g = rep(letters[1:6], each = 5)
#' )
#'
#' polar_plot_facet <- ggplot(d, aes(x, y)) +
#'   geom_point(aes(color = x), size = 2) +
#'   facet_wrap(~g) +
#'   scale_x_continuous(breaks = seq(0, 6), minor_breaks = NULL) +
#'   coord_polar()
#'
#' crop_coord_polar(polar_plot_facet, start = pi)
#'
#' # Use multiple values - one for each facet:
#' start <- seq(0, 5) * 2 * pi / 6
#' end <- start + start[2]
#'
#' crop_coord_polar(polar_plot_facet,
#'   start = start, end = end
#' )
#'
#' @export
crop_coord_polar <- function(plot, start = 0, end = 2 * pi,
                             padding = 0.02,
                             fix_aspect.ratio = TRUE) {
  .check_namespace("ggplot2", "ggtrace")

  stopifnot(
    inherits(plot, "gg"),
    length(start) == length(end),
    all(start >= 0),
    all(start <= 2 * pi),
    all(end >= 0),
    all(end <= 2 * pi),
    padding >= 0
  )

  if (!inherits(plot$coordinates, "CoordPolar")) {
    warning("Plot does not seem to use polar coordinates.")
  }

  trbl <- .get_trbl(start, end, padding)

  if (isTRUE(fix_aspect.ratio)) {
    aspect.ratio <-
      (trbl[["b."]] - trbl[["t."]]) /
        (trbl[["l."]] - trbl[["r."]])

    if (!all(aspect.ratio[1] == aspect.ratio)) aspect.ratio <- 1

    plot <- plot +
      ggplot2::theme(aspect.ratio = aspect.ratio[1])
  }


  expr <- substitute(
    {
      b <- b.
      t <- t.
      r <- r.
      l <- l.

      n_panels <- length(panels)
      if (length(b) != n_panels) b <- rep(b, length.out = n_panels)
      if (length(t) != n_panels) t <- rep(t, length.out = n_panels)
      if (length(r) != n_panels) r <- rep(r, length.out = n_panels)
      if (length(l) != n_panels) l <- rep(l, length.out = n_panels)

      for (p in seq_len(n_panels)) {
        panels[[p]] <- editGrob(panels[[p]],
          vp = viewport(
            yscale = c(b[p], t[p]),
            xscale = c(l[p], r[p])
          )
        )
      }
    },
    env = trbl
  )

  trace_plot <- ggtrace::with_ggtrace(
    x = plot,
    method = ggplot2::Layout$render,
    trace_steps = 5L,
    trace_expr = expr,
    out = "g"
  )
  trace_plot
}


# Utils -----

#' @keywords internal
.theta_to_xy <- function(theta) {
  theta <- (2 * pi - theta) + pi / 2
  cbind(cos(theta), sin(theta))
}

#' @keywords internal
.is_between_angle <- function(min, theta, max) {
  arc_long <- max - min
  if (arc_long < 0) arc_long <- arc_long + 360

  arc_short1 <- theta - min
  arc_short2 <- max - theta
  if (arc_short1 < 0) arc_short1 <- arc_short1 + 360
  if (arc_short2 < 0) arc_short2 <- arc_short2 + 360

  arc_long >= arc_short1 && arc_long >= arc_short2
}

#' @keywords internal
.get_trbl <- function(start, end, padding) {
  center.xy <- c(0, 0) + 0.5
  start.xy <- .theta_to_xy(start) / 2 + 0.5
  end.xy <- .theta_to_xy(end) / 2 + 0.5

  t. <- r. <- rep(1, nrow(start.xy))
  b. <- l. <- rep(0, nrow(start.xy))

  for (k in seq_len(nrow(start.xy))) {
    # t
    if (!.is_between_angle(start[k], 0, end[k])) {
      t.[k] <- pmax(center.xy[2], start.xy[k, 2], end.xy[k, 2]) + padding
    }

    # r
    if (!.is_between_angle(start[k], pi / 2, end[k])) {
      r.[k] <- pmax(center.xy[1], start.xy[k, 1], end.xy[k, 1]) + padding
    }

    # b
    if (!.is_between_angle(start[k], pi, end[k])) {
      b.[k] <- pmin(center.xy[2], start.xy[k, 2], end.xy[k, 2]) - padding
    }

    # l
    if (!.is_between_angle(start[k], 3 * pi / 2, end[k])) {
      l.[k] <- pmin(center.xy[1], start.xy[k, 1], end.xy[k, 1]) - padding
    }
  }

  data.frame(t., r., b., l.)
}
