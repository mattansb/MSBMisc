#' \eqn{R^2} Overkill
#'
#' Way too many ways to calculate \eqn{R^2}.
#'
#' @param pred The predicted values by some model; typically the result of a
#'   call to [predict()].
#' @param obs The true observed values.
#' @param type Which of the 8 \enq{R^2} to use. See details.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#'
#' @details
#' The types of \eqn{R^2}:
#' - \eqn{R^2_1 = 1 - \sum (y-\hat{y})^2 / \sum (y-\bar{y})^2}
#' - \eqn{R^2_2 = \sum (\hat{y}-\bar{y})^2 / \sum (y-\bar{y})^2}
#' - \eqn{R^2_3 = \sum (\hat{y}-\bar{\hat{y}})^2 / \sum (y-\bar{y})^2}
#' - \eqn{R^2_4 = 1 - \sum (e-\bar{e})^2 / \sum (y-\bar{y})^2}
#' - \eqn{R^2_5 = } squared multiple correlation coefficient between the regressand and the regressors
#' - \eqn{R^2_6 = r_{y,\hat{y}}^2}
#' - \eqn{R^2_7 = 1 - \sum (y-\hat{y})^2 / \sum y^2}
#' - \eqn{R^2_8 = \sum \hat{y}^2 / \sum y^2}
#'
#' @references Kvålseth, T. O. (1985). Cautionary note about R 2. The American Statistician, 39(4), 279-285.
#'
#' @examples
#' X <-  c(1, 2, 3, 4, 5, 6)
#' Y <- c(15, 37, 52, 59, 83, 92)
#'
#' m1 <- lm(Y ~ X)
#' m2 <- lm(Y ~ 0 + X)
#' m3 <- lm(log(Y) ~ X)
#'
#' # Table 2
#' data.frame(
#'   mod1 = sapply(1:8, R2, pred = predict(m1), obs = Y),
#'   mod2 = sapply(1:8, R2, pred = predict(m2), obs = Y),
#'   mod3 = sapply(1:8, R2, pred = exp(predict(m3)), obs = Y)
#' )
#'
#' @export
R2 <- function(pred, obs, type = 1, na.rm = TRUE) {
  if (na.rm) {
    good <- complete.cases(pred, obs)
    pred <- pred[good]
    obs <- obs[good]
  }

  f <- switch(as.character(type),
              "MSE" = ,
              "1" = .r2_1,
              "2" = .r2_2,
              "3" = .r2_3,
              "4" = .r2_4,
              "5" = .r2_5,
              "corr" = ,
              "6" = .r2_6,
              "7" = .r2_7,
              "8" = .r2_8)

  f(pred, obs)
}

#' @keywords internal
.r2_1 <- function(pred, obs) {
  1 - .SS(obs, pred) / .SS(obs, mean(obs))
}

#' @keywords internal
.r2_2 <- function(pred, obs) {
  .SS(pred, mean(obs)) / .SS(obs, mean(obs))
}

#' @keywords internal
.r2_3 <- function(pred, obs) {
  .SS(pred, mean(pred)) / .SS(obs, mean(obs))
}

#' @keywords internal
.r2_4 <- function(pred, obs) {
  e <- obs - pred
  1 - .SS(e, mean(e)) / .SS(obs, mean(obs))
}

#' @keywords internal
.r2_5 <- function(pred, obs) {
  NA
}

#' @keywords internal
.r2_6 <- function(pred, obs) {
  cor(pred, obs) ^ 2
}

#' @keywords internal
.r2_7 <- function(pred, obs) {
  1 - .SS(obs, pred) / .SS(obs, 0)
}

#' @keywords internal
.r2_8 <- function(pred, obs) {
  .SS(pred, 0) / .SS(obs, 0)
}

#' @keywords internal
.SS <- function(x, y) {
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)
  sum((x - y) ^ 2)
}
