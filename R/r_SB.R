#' Spearman-Brown Split half reliability
#'
#' @param x A correlation or numeric vector
#' @param y A numeric vector
#' @param var.equal Assume equal var of `x` and `y`? (ignored if `y` is not `NULL`)
#'
#' @examples
#' r_SB(1:30,-exp(1/1:30), var.equal = TRUE)
#'
#' r_SB(1:30,-exp(1/1:30), var.equal = FALSE)
#'
#' r_SB(0.57)
#'
#' @export
r_SB <- function(x, y = NULL, var.equal = TRUE) {
  if (!is.null(y)) {
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
