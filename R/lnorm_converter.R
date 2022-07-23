#' Functions to convert parameters of a log-normal distribution to meaningfull
#' values on the response scale.
#'
#' @inheritParams stats::Lognormal
#' @param ... Not used
#'
#'
#' @examples
#' x <- rlnorm(1e4, meanlog = 1.5, sdlog = 1.2)
#'
#' m <- lm(log(x) ~ 1)
#'
#' meanlog <- coef(m)
#' sdlog <- sigma(m)
#'
#' lnorm_mean(meanlog, sdlog)
#' mean(x)
#'
#' lnorm_median(meanlog, sdlog)
#' median(x)
#'
#' lnorm_sd(meanlog, sdlog)
#' sd(x)
#'
#'
#' @export
lnorm_mean <- function(meanlog, sdlog, ...) {
  .same.length(meanlog, sdlog)
  Vlog <- sdlog ^ 2
  exp(meanlog + Vlog / 2) |>
    unname()
}

#' @export
#' @rdname lnorm_mean
lnorm_median <- function(meanlog, ...) {
  exp(meanlog) |>
    unname()
}

#' @export
#' @rdname lnorm_mean
lnorm_var <- function(meanlog, sdlog, ...) {
  .same.length(meanlog, sdlog)
  Vlog <- sdlog ^ 2
  (exp(Vlog) - 1) * exp(2 * meanlog + Vlog) |>
    unname()
}

#' @export
#' @rdname lnorm_mean
lnorm_sd <- function(meanlog, sdlog, ...) {
  sqrt(lnorm_var(meanlog, sdlog, ...))
}



# Utils -------------------------------------------------------------------

#' @keywords internal
.same.length <- function(x, y) {
  stopifnot(
    "Vector inputs must be of the same length." =
      length(x) == length(y) ||
      length(x) == 1L ||
      length(y) == 1L
  )
  invisible(NULL)
}

