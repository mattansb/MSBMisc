#' Create a worm plot
#'
#' This function is a wrapper around `qqplotr::stat_pp_*(detrend = TRUE)`.
#'
#' @param x A numerical vector
#' @param distribution Name of a distribution, matching the `d*`, `p*` and `q*` function names.
#' @param ... Args passed to `d*`, `p*` and `q*` functions.
#'
#' @examplesIf require("ggplot2") && require("qqplotr")
#' x <- rnorm(100)
#' qq_worm_plot(x)
#'
#' x <- rbeta(100, shape1 = 2, shape2 = 3)
#' qq_worm_plot(x, distribution = "beta", shape1 = 2, shape2 = 3)
#'
#' x <- rt(100, df = 3)
#' qq_worm_plot(x, distribution = "t", df = 3)
#'
#' # x <- rexp(100)
#' # qq_worm_plot(x, distribution = "exp")
#'
#' # x <- rpois(100, lambda = 15)
#' # qq_worm_plot(x, distribution = "pois", lambda = 15)
#'
#' # x <- runif(100)
#' # qq_worm_plot(x, distribution = "unif")
#'
#' @details
#' [Some related tweets](https://twitter.com/mattansb/status/1199936633413476358).
#'
#' @export
qq_worm_plot <-
  function(x,
           distribution = "norm",
           ...) {
    .check_namespace("ggplot2", "qqplotr")

    dparams <- list(...)
    if (!is.null(dparams$return)) {
      warning("This functions no longer returns data.")
      dparams$return <- NULL
    }

    ggplot2::ggplot(mapping = ggplot2::aes(sample = x)) +
      qqplotr::stat_qq_band(detrend = TRUE, distribution = distribution, dparams = dparams) +
      qqplotr::stat_qq_line(detrend = TRUE, distribution = distribution, dparams = dparams) +
      qqplotr::stat_qq_point(detrend = TRUE, distribution = distribution, dparams = dparams)
  }
