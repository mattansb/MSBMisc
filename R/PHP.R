#' Function for Post-Hoc Power analysis
#'
#' Based on [Lenth (2007) _Post Hoc Power: Tables and Commentary_](https://stat.uiowa.edu/sites/stat.uiowa.edu/files/techrep/tr378.pdf).
#' `php.guf()` give's Lenth's _grand unified formula for post hoc power_.
#'
#' @param tval,zval,Fval,chisqval Observed test statistic
#' @param p Observed p-value (used if test statistic not supplied)
#' @param df,df1,df2 Test statistics' degrees of freedom
#' @param alpha Confidence level of the test.
#' @param one.tailed Is the p-value from a one-tailed test?
#'
#' @examples
#'
#' lm(hp ~ am, mtcars) |> summary()
#'
#' php.t(p = 0.18, df = 30)
#'
#' php.guf(p = 0.18)
#'
#'
#'
#' # Table 1 - one tailed
#' expand.grid(
#'   p = c(.001, .01, .05, .1, .25, .5, .75),
#'   df = c(1, 2, 5, 10, 20, 50, 200, 1000, Inf)
#' ) |>
#'   transform(PHP = php.t(p = p, df = df, one.tailed = TRUE)) |>
#'   stats::reshape(direction = "wide",
#'                  idvar = "df", timevar = "p")
#'
#'
#' # Table 1 - two tailed
#' expand.grid(
#'   p = c(.001, .01, .05, .1, .25, .5, .75),
#'   df = c(1, 2, 5, 10, 20, 50, 200, 1000, Inf)
#' ) |>
#'   transform(PHP = php.t(p = p, df = df)) |>
#'   stats::reshape(direction = "wide",
#'                  idvar = "df", timevar = "p")
#'
#'
#' # Table 2
#' expand.grid(
#'   p = c(.001, .01, .05, .1, .25, .5, .75),
#'   df2 = c(1, 2, 5, 10, 20, 50, 200, 1000, Inf),
#'   df1 = c(2, 3, 4, 10)
#' ) |>
#'   transform(PHP = php.F(p = p, df1 = df1, df2 = df2)) |>
#'   stats::reshape(direction = "wide",
#'                  idvar = c("df1", "df2"), timevar = "p")
#'
#'
#' @export
php.t <- function(tval, p, df = Inf, alpha = 0.05, one.tailed = FALSE) {
  if (missing(tval)) {
    if (one.tailed) {
      tval <- stats::qt(p, df = df, ncp = 0, lower.tail = FALSE)
    } else {
      tval <- stats::qt(p / 2, df = df, ncp = 0, lower.tail = FALSE)
    }
  }

  if (one.tailed) {
    crit <- stats::qt(alpha, df = df, ncp = 0, lower.tail = FALSE)
    ncp <- tval
  } else {
    crit <- stats::qt(alpha / 2, df = df, ncp = 0, lower.tail = FALSE)
    ncp <- abs(tval)
  }

  php <- stats::pt(crit, df = df, ncp = ncp, lower.tail = FALSE)
  if (!one.tailed) {
    php <- php + stats::pt(-crit, df = df, ncp = ncp, lower.tail = TRUE)
  }
  php
}

#' @export
#' @rdname php.t
php.F <- function(Fval, p, df1, df2 = Inf, alpha = 0.05) {
  if (missing(Fval)) {
    Fval <- stats::qf(p, df1, df2, ncp = 0, lower.tail = FALSE)
  }
  crit <- stats::qf(alpha, df1, df2, ncp = 0, lower.tail = FALSE)

  ncp <- Fval * df1

  stats::pf(crit, df1, df2, ncp = ncp, lower.tail = FALSE)
}

#' @export
#' @rdname php.t
php.z <- function(zval, p, alpha = 0.05, one.tailed = FALSE) {
  php.t(zval, p, alpha = alpha, one.tailed = one.tailed)
}

#' @export
#' @rdname php.t
php.chisq <- function(chisqval, p, df, alpha = 0.05) {
  php.F(chisqval / df, p, df, alpha = alpha)
}

#' @export
#' @rdname php.t
php.guf <- function(p, alpha = 0.05) {
  as.numeric(p < alpha)
}
