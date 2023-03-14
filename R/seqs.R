#' Sequence Generation Based on the Values of a Vector
#'
#' @param x A numeric vector
#' @param length.out desired length of the sequence. If no other arguments are
#'   valued, defaults to 20.
#' @param padding Padding factor for the range.
#' @inheritParams base::seq
#' @inheritParams base::mean
#' @inheritParams stats::quantile
#'
#' @examples
#' set.seed(1)
#' x <- rt(100, df = 3)
#' seq_range(x, length.out = 5)
#' seq_IQR(x, length.out = 5)
#' seq_quantile(x, c(.05, .95), length.out = 5)
#'
#' mean_sd(x)
#'
#' @examplesIf require("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars, aes(cyl, mpg)) +
#'   stat_summary(aes(color = "Mean (SD)"),
#'     fun.data = mean_sd,
#'     fun.args = list(out = "data.frame")
#'   ) +
#'   stat_summary(aes(color = "Median (MAD)"),
#'     fun.data = median_mad,
#'     fun.args = list(out = "data.frame"),
#'     position = position_nudge(x = 0.5)
#'   )
#'
#' @export
seq_range <- function(x, length.out = NULL, by = NULL, along.with = NULL,
                      na.rm = TRUE, padding = 0.05) {
  match.call()

  stopifnot("'padding' must be a positive scalar." = padding >= 0 && length(padding) == 1L)
  range <- range(x, na.rm = na.rm)
  e <- diff(range) * padding
  range <- range + c(-1, 1) * e

  .seq(range, length.out = length.out, by = by, along.with = along.with)
}


#' @export
#' @rdname seq_range
seq_quantile <- function(x, probs, length.out = NULL, by = NULL, along.with = NULL,
                         na.rm = TRUE) {
  match.call()
  stopifnot("'probs' must be of length 2." = length(probs) == 2L)
  range <- stats::quantile(x, probs = probs, na.rm = na.rm)

  .seq(range, length.out = length.out, by = by, along.with = along.with)
}


#' @export
#' @rdname seq_range
seq_IQR <- function(x, length.out = NULL, by = NULL, along.with = NULL,
                    na.rm = TRUE) {
  cl <- match.call()
  cl[[1]] <- quote(seq_quantile)
  cl$probs <- c(.25, .75)
  eval(cl)
}


#' @keywords internal
.seq <- function(range, length.out = NULL, by = NULL, along.with = NULL) {
  if (is.null(length.out) && is.null(by) && is.null(along.with)) length.out <- 20
  cl <- quote(seq(range[1], range[2]))
  cl$length.out <- length.out
  cl$by <- by
  cl$along.with <- along.with
  eval(cl)
}


#' @export
#' @rdname seq_range
#' @param out If `"data.frame"` can be used as a summary function in `ggplot2`.
mean_sd <- function(x, na.rm = TRUE, out = c("vector", "data.frame")) {
  match.call()
  out <- match.arg(out)

  x <- mean(x, na.rm = na.rm) + c(-1, 0, 1) * stats::sd(x, na.rm = na.rm)
  names(x) <- c("-SD", "Mean", "+SD")
  if (out == "data.frame") {
    x <- as.data.frame(as.list(x))
    names(x) <- c("ymin", "y", "max")
  }
  x
}

#' @export
#' @rdname seq_range
median_mad <- function(x, na.rm = TRUE, out = c("vector", "data.frame")) {
  match.call()
  out <- match.arg(out)

  x <- stats::median(x, na.rm = na.rm) + c(-1, 0, 1) * stats::mad(x, na.rm = na.rm)
  names(x) <- c("-MAD", "Median", "+MAD")
  if (out == "data.frame") {
    x <- as.data.frame(as.list(x))
    names(x) <- c("ymin", "y", "max")
  }
  x
}
