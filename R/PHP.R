#' Function for Post-Hoc Power analysis
#'
#' Based on [Lenth (2007) _Post Hoc Power: Tables and Commentary_](https://stat.uiowa.edu/sites/stat.uiowa.edu/files/techrep/tr378.pdf).
#' `php.guf()` give's Lenth's _grand unified formula for post hoc power_.
#'
#' @param tval,zval,Fval,chisqval Observed test statistic
#' @param p Observed p-value (used if test statistic not supplied)
#' @param df,df1,df2 Test statistics' degrees of freedom
#' @param alpha Confidence level of the test.
#'
#'
#'
#' @export
php.t <- function(tval, p, df = Inf, alpha = 0.05) {
  if (missing(tval)) {
    tval <- qt(p / 2, df = df, ncp = 0)
  }
  crit <- qt(1 - alpha / 2, df = df, ncp = 0)

  ncp <- abs(tval)

  pt(crit, df = df, ncp = ncp, lower.tail = FALSE) +
    pt(-crit, df = df, ncp = ncp, lower.tail = TRUE)
}

#' @export
#' @rdname php.t
php.F <- function(Fval, p, df1, df2 = Inf, alpha = 0.05) {
  if (missing(Fval)) {
    Fval <- qf(p, df1, df2, ncp = 0, lower.tail = FALSE)
  }
  crit <- qf(1 - alpha, df1, df2, ncp = 0, lower.tail = TRUE)

  ncp <- Fval * df1

  pf(crit, df1, df2, ncp = ncp, lower.tail = FALSE)
}

#' @export
#' @rdname php.t
php.z <- function(zval, p, alpha = 0.05) {
  php.t(zval, p, alpha = alpha)
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








# Fp_to_pwr(0.1, 2, 50, random = T)
