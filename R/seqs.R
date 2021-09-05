#' Sequence Generation Baed on Range
#'
#' @param x A numeric vector
#' @inheritParams base::seq
#' @inheritParams base::mean
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' seq_range(x, length.out = 5)
#'
#' mean_sd(x)
#'
#' library(ggplot2)
#' ggplot(mtcars, aes(cyl, mpg)) +
#'   stat_summary(fun.data = mean_sd, fun.args = list(out = "data.frame"))
#'
#' @export
seq_range <- function(x, length.out = 20, na.rm = TRUE) {
  match.call()
  range <- range(x, na.rm = na.rm)
  seq(range[1], range[2], length.out = length.out)
}

#' @export
#' @rdname seq_range
#' @param out If `"data.frame"` can be used as a summary function in `ggplot2`.
mean_sd <- function(x, na.rm = TRUE, out = c("vector", "data.frame")) {
  match.call()
  out <- match.arg(out)

  x <- mean(x, na.rm = na.rm) + c(-1, 0, 1) * sd(x, na.rm = na.rm)
  names(x) <- c("-SD", "Mean", "+SD")
  if (out == "data.frame") {
    x <- as.data.frame(as.list(x))
    names(x) <- c("ymin", "y", "max")
  }
  x
}
