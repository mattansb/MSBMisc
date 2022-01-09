#' Sequence Generation Based on the Values of a Vector
#'
#' @param x A numeric vector
#' @param length.out desired length of the sequence. If no other arguments are
#'   valued, defaults to 20.
#' @inheritParams base::seq
#' @inheritParams base::mean
#'
#' @examples
#' set.seed(1)
#' x <- rt(100, df = 3)
#' seq_range(x, length.out = 5)
#' seq_IQR(x, length.out = 5)
#'
#' mean_sd(x)
#'
#' @examplesIf require("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars, aes(cyl, mpg)) +
#'   stat_summary(fun.data = mean_sd, fun.args = list(out = "data.frame"))
#'
#' @export
seq_range <- function(x, length.out = NULL, by = NULL, along.with = NULL, na.rm = TRUE) {
  match.call()
  range <- range(x, na.rm = na.rm)

  .seq(range, length.out = length.out, by = by, along.with = along.with)
}


#' @export
#' @rdname seq_range
seq_IQR <- function(x, length.out = NULL, by = NULL, along.with = NULL, na.rm = TRUE) {
  match.call()
  range <- stats::quantile(x, c(0.25, 0.75), na.rm = na.rm)

  .seq(range, length.out = length.out, by = by, along.with = along.with)
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
