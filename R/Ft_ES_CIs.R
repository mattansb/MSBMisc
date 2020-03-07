#' Convert F and t values to Noncentrality-Based Confidence Intervals
#'
#' These methods searches for a the best `ncp` (non-central parameters) for either
#' the t distribution (for r and Cohen's d effect
#' sizes) of the F distribution (for the PVE effect sizes) for the desired tail-probabilities,
#' and then convert these `ncp`s to the corresponding effect sizes using `effectsize`'s
#' conversion functions.
#'
#' @param f,t The t or the F statistics.
#' @param df,df_error Degrees of freedom of numerator or of the error estimate (i.e., the residuals).
#' @param conf.level Confidence level.
#' @param type See [the corresponding functions in effectsize][effectsize::F_to_eta2].
#'
#' @details
#' All effect sizes are converted using simple conversion formulas. See formulas for
#' [partial variance explained (PVE)][effectsize::F_to_eta2] and for [d and r][effectsize::t_to_d].
#'
#' CI for PVE are based on Steiger (2004, eq. 16), and for Cohen's d are based on Cumming and Finch (2001, eq. 17-18).
#' Read [here](https://tinyurl.com/srqd3ys) for more.
#'
#' @references
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and calculation of confidence intervals that are based on central and noncentral distributions. Educational and Psychological Measurement, 61(4), 532-574.
#'
#' @example examples/examples.Ft_ES_CIs.R
#'
#' @export
F_to_PVE_CI <- function(f,
                        df,
                        df_error,
                        conf.level = 0.9,
                        type = c("eta2", "epsilon2", "omega2")) {
  .check_namespace("effectsize")
  type <- match.arg(type)

  fs <- .get_ncp_F(f, df, df_error, conf.level = conf.level)

  .es <- switch (
    type,
    eta2 = effectsize::F_to_eta2,
    epsilon2 = effectsize::F_to_epsilon2,
    omega2 = effectsize::F_to_omega2
  )

  setNames(.es(c(fs[1], f, fs[2]), df, df_error),
           c("Lower", "ES", "Upper"))
}

#' @rdname F_to_PVE_CI
#' @export
t_to_PVE_CI <- function(t,
                        df_error,
                        conf.level = 0.9,
                        type = c("eta2", "epsilon2", "omega2")) {
  F_to_PVE_CI(t ^ 2, 1, df_error, conf.level = conf.level, type = type)
}

#' @rdname F_to_PVE_CI
#' @export
t_to_r_CI <- function(t, df_error, conf.level = 0.95) {
  .check_namespace("effectsize")
  ts <- .get_ncp_t(t, df_error, conf.level = conf.level)

  setNames(effectsize::t_to_r(c(ts[1], t, ts[2]), df_error),
           c("Lower", "ES", "Upper"))
}

#' @rdname F_to_PVE_CI
#' @export
F_to_r_CI <- function(f, df, df_error, conf.level = 0.95) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to r.")
  }
  t_to_r_CI(sqrt(f), df_error, conf.level)
}

#' @rdname F_to_PVE_CI
#' @export
t_to_d_CI <- function(t,
                      df_error,
                      conf.level = 0.95,
                      pooled = FALSE) {
  .check_namespace("effectsize")
  ts <- .get_ncp_t(t, df_error, conf.level = conf.level)

  setNames(effectsize::t_to_d(c(ts[1], t, ts[2]), df_error, pooled = pooled),
           c("Lower", "ES", "Upper"))
}

#' @rdname F_to_PVE_CI
#' @export
F_to_d_CI <- function(f,
                      df,
                      df_error,
                      conf.level = 0.95,
                      pooled = FALSE) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to r.")
  }
  t_to_d_CI(sqrt(f), df_error, conf.level = conf.level, pooled = pooled)
}


# ci_cramers_V <- function(chisq, N, a, b, conf.level = 0.9){
#   if (isTRUE(all.equal(chisq,0))) {
#     return(c(0, 1)) # unestimatable
#   }
#
#   alpha <- 1 - conf.level
#   probs <- c(alpha / 2, 1 - alpha / 2)
#
#   df <- prod(c(a, b) - 1)
#
#   ncp <- suppressWarnings(
#     c(
#       optim(chisq, function(x) abs(pchisq(q = chisq, df, ncp = x) - probs[1]))$par,
#       optim(chisq, function(x) abs(pchisq(q = chisq, df, ncp = x) - probs[2]))$par
#     )
#   )
#
#   # Convert to pes
#   x <- effectsize::chisq_to_cramers_v(sort(ncp), N, a, b)
#   if (x[2] < effectsize::chisq_to_cramers_v(chisq, N, a, b)) {
#     # this is for dealing with rare cases with very small effect sizes
#     # where the CI gives nonsencical upper bounds
#     # (This is an issue in effectsize:::.ci_partial_eta_squared)
#     x[2] <- 1
#   }
#   x
# }


# Utils -------------------------------------------------------------------

#' @keywords internal
.get_ncp_t <- function(t, df_error, conf.level = 0.95) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (isTRUE(all.equal(t, 0))) {
    t_ncp <- qt(probs, df_error)
    return(t_ncp)
  }

  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(t, 2),
    fn = function(x) {
      p <- pt(q = t, df = df_error, ncp = x)

      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  t_ncp <- unname(sort(ncp$par))
  if (isTRUE(all.equal(t_ncp[1], 0))) {
    t_ncp[1] <- qt(probs[1], df_error)
  }

  if (isTRUE(all.equal(t_ncp[2], 0))) {
    t_ncp[2] <- qt(probs[2], df_error)
  }
  return(t_ncp)
}


#' @keywords internal
.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (isTRUE(all.equal(f, 0))) {
    return(c(0, Inf)) # unestimatable
  }

  lambda <- f * df
  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(lambda, 2),
    fn = function(x) {
      p <- pf(q = f, df, df_error, ncp = x)

      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  f_ncp <- sort(ncp$par) / df
  return(f_ncp)
}
