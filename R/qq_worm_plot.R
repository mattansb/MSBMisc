#' Create a worm plot
#'
#' @param x A numerical vector
#' @param distribution Name of a distribution, matching the `d*`, `p*` and `q*` function names.
#' @param return a `ggplot` or a data frame?
#' @param ... Args assed to `d*`, `p*` and `q*` functions.
#'
#' @examples
#' x <- rnorm(100)
#' qq_worm_plot(x)
#' qq_worm_plot(x, return = "data")
#'
#' x <- rbeta(100, shape1 = 2, shape2 = 3)
#' qq_worm_plot(x, distribution = "beta", shape1 = 2, shape2 = 3)
#'
#' \dontrun{
#'   x <- rexp(100)
#'   qq_worm_plot(x, distribution = "exp")
#'
#'   x <- rpois(100, lambda = 15)
#'   qq_worm_plot(x, distribution = "pois", lambda = 15)
#'
#'   x <- rt(100, df = 3)
#'   qq_worm_plot(x, distribution = "t", df = 3)
#'
#'   x <- runif(100)
#'   qq_worm_plot(x, distribution = "unif")
#' }
#'
#' @details
#' [Some related tweets](https://twitter.com/mattansb/status/1199936633413476358).
#'
#' @export
qq_worm_plot <-
  function(x,
           distribution = "norm",
           return = c("plot", "data"),
           ...) {

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
      .check_namespace("ggplot2")

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
