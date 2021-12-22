#' Information criteria for log-normal models
#'
#' Log-likelihood (and by extension *AIC* and *BIC*) for log-normal models fit
#' with `stats::lm(log(y) ~ ...)` are computed with `stats::dnorm(log(y), ...)`
#' instead of with `stats::dlnorm(y, ...)`, which makes comparing different
#' families difficult. This function is aimed at rectifying this. See examples.
#'
#' @param object A fitted model object. The model must meet all of the
#'   following (will throw an error if not met):
#'   1. A Gaussian likelihood with an identity link function.
#'   2. The LHS of the model's formula must use the `log()` function.
#'   3. No weights (not yet supported).
#' @param REML Only `FALSE` supported.
#' @inheritParams stats::AIC
#'
#' @note `REML` is not (yet) supported. Make sure you are comparing correct
#'   LL/AIC/BIC values.
#'
#' @examples
#'
#' data("mtcars")
#' mtcars$mpg <- floor(mtcars$mpg)
#'
#' model_lnorm <- lm(log(mpg) ~ factor(cyl), mtcars)
#' model_norm <- lm(mpg ~ factor(cyl), mtcars)
#' model_pois <- glm(mpg ~ factor(cyl), mtcars, family = poisson())
#'
#' # No, that first one is wrong...
#' (aics <- AIC(model_lnorm, model_norm, model_pois))
#'
#' aics[1, "AIC"] <- AIC_lnorm(model_lnorm)
#' aics # better!
#'
#'
#' @examplesIf require("lme4")
#' # Should support any model really... =====================
#' model_lnorm <- lme4::lmer(log(mpg) ~ factor(cyl) + (1 | gear), mtcars, REML = FALSE)
#' model_norm <- lme4::lmer(mpg ~ factor(cyl) + (1 | gear), mtcars, REML = FALSE)
#' model_pois <- lme4::glmer(mpg ~ factor(cyl) + (1 | gear), mtcars, family = poisson())
#'
#' # No, that first one is wrong...
#' (aics <- AIC(model_lnorm, model_norm, model_pois))
#'
#' aics[1, "AIC"] <- AIC_lnorm(model_lnorm)
#' aics # better!
#'
#'
#' @export
logLik_lnorm <- function(object, REML = FALSE) {
  .check_namespace("insight")

  stopifnot(
    "Model is not log-normal" = .is_lnorm(object)
  )

  if (REML) warning("logLik with `REML = TRUE` not supported. Setting `REML = FALSE`")

  ll <- stats::logLik(object)

  ll[1] <- stats::dlnorm(
    x = insight::get_response(object),
    meanlog = stats::fitted(object),
    sdlog = stats::sigma(object),
    log = TRUE
  ) |> sum()

  ll
}


#' @rdname logLik_lnorm
#' @export
AIC_lnorm <- function(object, k = 2, REML = FALSE) {
  logLik_lnorm(object) |>
    stats::AIC(k = k)
}


#' @rdname logLik_lnorm
#' @export
BIC_lnorm <- function(object, REML = FALSE) {
  logLik_lnorm(object, REML = REML) |>
    stats::BIC()
}


# Utils -------------------------------------------------------------------



#' @keywords internal
.is_lnorm <- function(object, check_weights = TRUE) {
  .check_namespace("insight")

  if (check_weights && !is.null(insight::get_weights(object))) {
    stop("Oops, model has weights... Mattan was too lazy to solve this", call. = FALSE)
  }

  fam <- insight::get_family(object)
  form <- insight::find_formula(object)[[1]]

  fam$family == "gaussian" &&
    fam$link == "identity" &&
    length(LHS <- form[2]) == 1L &&
    length(LHS <- as.character(LHS[[1]])) >= 2L &&
    LHS[1] == "log"
}








