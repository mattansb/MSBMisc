#' Calculate reliability from E/CFA
#'
#' @param x E/CFA model (e.g., the result from `psych::fa()` or
#'   `lavaan::cfa()`).
#' @param keys optional, see ?psych::make.keys
#' @param threshold which values from the loadings should be used? Only used if
#'   `keys = NULL`.
#' @param labels optional factor labels
#' @param ... For `lavaan` objects, arguments passed to `semTools::compRelSEM()`
#'
#' @author [Brenton M. Wiernik](https://wiernik.org/)
#'
#' @examplesIf require("psych") && require("GPArotation")
#' data("Harman74.cor")
#' EFA <- psych::fa(Harman74.cor$cov, 4)
#'
#' fa_reliability(EFA)
#'
#' @examplesIf require("lavaan") && require("semTools")
#' HS.model <- " visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 "
#'
#' CFA <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#'
#' fa_reliability(CFA)
#'
#' @export
fa_reliability <- function(x, ...) {
  UseMethod("fa_reliability")
}

#' @export
#' @rdname fa_reliability
fa_reliability.fa <- function(x, keys = NULL, threshold = 0, labels = NULL, ...) {
  L <- unclass(x$loadings)
  r <- x$r

  if (is.null(keys)) keys <- sign(L) * (abs(L) > threshold)

  out <- data.frame(
    Factor = colnames(L),
    Omega = colSums(keys * L)^2 / diag(t(keys) %*% r %*% keys)
  )

  if (!is.null(labels)) {
    out$Factor <- labels
  } else {
    rownames(out) <- NULL
  }

  out
}

#' @export
#' @rdname fa_reliability
fa_reliability.lavaan <- function(x, ...) {
  .check_namespace("semTools")
  Omega <- semTools::compRelSEM(x, tau.eq = FALSE, ...)
  Factor <- names(Omega)

  data.frame(Factor, Omega = as.vector(Omega))
}
