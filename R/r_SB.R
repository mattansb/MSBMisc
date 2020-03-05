#' Spearman-Brown Split half reliability
#'
#' @param x A correlation or numeric vector
#' @param y A numeric vector (ignored if `x` is not a vector)
#' @param var.equal Assume equal var of `x` and `y`? (ignored if `x` is not a vector)
#'
#' @example examples/examples.r_SB.R
#'
#' @export
r_SB <- function(x, y = NULL, var.equal = TRUE) {
  if (length(x) > 1) {
    r <- cov(cbind(x, y))
    if (var.equal) {
      r <- cov2cor(r)
    }

    if (r[1, 2] < 0) {
      stop("Correlation is negative!")
    }
    mean(r[!(!upper.tri(r) & !lower.tri(r))]) / mean(r)
  } else {
    if (x < 0) {
      stop("Correlation is negative!")
    }
    if (x > 1) {
      stop("Correlation cannot be greater than 1!")
    }
    (2 * x) / (1 + x)
  }
}
