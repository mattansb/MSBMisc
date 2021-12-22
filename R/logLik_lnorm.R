#' Information criteria for log-normal models
#'
#' Log-likelihood (and by extension *AIC* and *BIC*) for log-normal models fit
#' with `stats::lm(log(y) ~ ...)` are computed with `dnorm(log(y), ...)` instead
#' of with `dlnorm(y, ...)`, which makes comparing different families difficult.
#' This function is aimed at rectifying this. See examples.
#'
#' @inheritParams stats::AIC
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
#' model_lnorm <- lme4::lmer(log(mpg) ~ factor(cyl) + (1 | gear), mtcars)
#' model_norm <- lme4::lmer(mpg ~ factor(cyl) + (1 | gear), mtcars)
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
logLik_lnorm <- function(object) {
  .check_namespace("insight")

  stopifnot(
    "Model is not log-normal" = .is_lnorm(object)
  )

  ll <- stats::logLik(object)
  df <- attr(ll, "df")
  nobs <- attr(ll, "nobs")

  y <- insight::get_response(object)
  ll[1] <- stats::dlnorm(y, meanlog = fitted(object), sdlog = sigma(object),
                         log = TRUE) |> sum()

  ll
}


#' @rdname logLik_lnorm
#' @export
AIC_lnorm <- function(object, ..., k = 2) {
  if (length(list(...))) {
    cl <- match.call()
    cl[[1]] <- quote(stats::AIC)
    out <- eval(cl)
    out[["AIC"]] <- sapply(list(object, ...), AIC_lnorm, k = k)
  } else {
    ll <- logLik_lnorm(object)
    df <- attr(ll, "df")
    out <- k * df - 2 * ll
  }
  out
}


#' @rdname logLik_lnorm
#' @export
BIC_lnorm <- function(object, ...) {
  if (length(list(...))) {
    cl <- match.call()
    cl[[1]] <- quote(stats::BIC)
    out <- eval(cl)
    out[["BIC"]] <- sapply(list(object, ...), BIC_lnorm)
  } else {
    ll <- logLik_lnorm(object)
    df <- attr(ll, "df")
    nobs <- attr(ll, "nobs")
    out <- df * log(nobs) - 2 * ll
  }
  out
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
    length(LHS <- as.character(LHS[[1]])) == 2L &&
    LHS[1] == "log"
}








