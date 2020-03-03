#' Create a worm plot
#'
#' @param x A numerical vector
#' @param distribution Name of a distribution, matching the \code{d\*}, \code{p\*} and \code{q\*} function names.
#' @param return a \code{ggplot} or a data frame?
#' @param ... Args assed to \code{d\*}, \code{p\*} and \code{q\*} functions.
#'
#' @example examples/examples.qq_worm_plot.R
#'
#' @details
#' From https://twitter.com/mattansb/status/1199936633413476358
#'
#' @export
qq_worm_plot <-
  function(x,
           distribution = "norm",
           return = c("plot", "data"),
           ...) {
    .check_namespace("ggplot2")

    d <- match.fun(paste0("d", distribution))
    p <- match.fun(paste0("p", distribution))
    q <- match.fun(paste0("q", distribution))
    return <- match.arg(return)
    dparams <- list(...)

    worm_ci_UL <- function(nx) {
      1.96 * sqrt(p(nx, ...) * (1 - p(nx, ...)) / length(nx)) / d(nx, ...)
    }

    worm_ci_LL <- function(nx) {
      -1.96 * sqrt(p(nx, ...) * (1 - p(nx, ...)) / length(nx)) / d(nx, ...)
    }

    if (return == "plot") {
      ggplot2::ggplot(mapping = ggplot2::aes(sample = x)) +
        # Dots
        ggplot2::stat_qq(ggplot2::aes(y = stat(sample - theoretical)),
                         distribution = q,
                         dparams = dparams) +
        # Funnel
        ggplot2::stat_qq(
          ggplot2::aes(y = stat(worm_ci_UL(theoretical))),
          distribution = q,
          dparams = dparams,
          geom = "line"
        ) +
        ggplot2::stat_qq(
          ggplot2::aes(y = stat(worm_ci_LL(theoretical))),
          distribution = q,
          dparams = dparams,
          geom = "line"
        ) +
        ggplot2::labs(x = paste0("theoretical ", distribution, " quantiles")) +
        NULL
    } else {
      theoretical <- q(ppoints(x), ...)[order(order(x))]
      diff <- x - theoretical
      UCL <- worm_ci_UL(theoretical)
      LCL <- worm_ci_LL(theoretical)
      data.frame(theoretical, sample_deviation = diff, UCL, LCL)
    }
  }
