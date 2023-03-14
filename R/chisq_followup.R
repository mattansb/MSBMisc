#' Follow-up for contingency table test
#'
#' @param Xsq Result from `chisq.test()`
#' @param population_in_row Comparisons by row? (If not, by column.)
#' @param adjust Method for correcting p-values. See [`stats::p.adjust`].
#' @param effect_size Type of effect size to use.
#' @inheritParams effectsize::chisq_to_phi
#' @param res_type Type of residuals to use.
#' @param ... Passed to `chisq.test()`.
#'
#' @examples
#' M <- as.table(rbind(
#'   c(762, 327, 468),
#'   c(484, 239, 477)
#' ))
#' dimnames(M) <- list(
#'   gender = c("F", "M"),
#'   party = c("Democrat", "Independent", "Republican")
#' )
#' M
#'
#' res <- chisq.test(M)
#' chisq_pairwise(res)
#' chisq_pairwise(res, population_in_row = FALSE)
#'
#' chisq_residual(res)
#'
#' @export
chisq_pairwise <- function(Xsq,
                           population_in_row = TRUE,
                           adjust = stats::p.adjust.methods,
                           effect_size = c("V", "phi"),
                           ci = 0.95,
                           ...) {
  adjust <- match.arg(adjust)
  effect_size <- match.arg(effect_size)

  tbl <- Xsq$observed

  if (!population_in_row) tbl <- t(tbl)

  popsNames <- rownames(tbl)

  if (have_effectsize <- .check_namespace("effectsize", quietly = TRUE)) {
    esf <- switch(effect_size,
      V = effectsize::chisq_to_cramers_v,
      phi = effectsize::chisq_to_phi
    )
  }

  pairs <- utils::combn(1:nrow(tbl), 2)
  res <- lapply(seq_len(ncol(pairs)), function(i) {
    pair <- pairs[, i]
    temp_res <- stats::chisq.test(tbl[pair, ], ...)

    .tmp <- data.frame(
      comparison = paste(popsNames[pair], collapse = " vs. "),
      Chi.sq = temp_res$statistic,
      df = temp_res$parameter
    )

    if (have_effectsize) {
      ES <- effectsize::effectsize(temp_res, type = effect_size, ci = ci)
      .tmp[[effect_size]] <- ES[[1]]
      .tmp[[paste0(effect_size, ".CI_low")]] <- ES$CI_low
      .tmp[[paste0(effect_size, ".CI_high")]] <- ES$CI_high
    }
    .tmp$p.raw <- temp_res$p.value
    .tmp
  })

  res <- do.call(rbind, res)
  res[[paste0("p.", adjust)]] <-
    stats::p.adjust(res$p.raw, method = adjust)
  rownames(res) <- NULL
  return(res)
}


#' @rdname chisq_pairwise
#' @export
chisq_residual <- function(Xsq,
                           adjust = stats::p.adjust.methods,
                           res_type = c("pearson", "standardized"),
                           ci = 0.95) {
  adjust <- match.arg(adjust)
  res_type <- match.arg(res_type)


  resids <- switch(res_type,
    pearson = Xsq$residuals,
    standardized = Xsq$stdres
  )

  Obs <- Xsq$observed

  tbl <- expand.grid(dimnames(Obs)) |>
    cbind(
      z.value = c(resids),
      n.obs = c(Obs)
    )

  # Effect size
  if (.check_namespace("effectsize", quietly = TRUE)) {
    ES <- effectsize::z_to_d(tbl$z.value, tbl$n.obs, paired = TRUE, ci = ci)
    tbl$d <- ES$d
    tbl$d.CI_low <- ES$CI_low
    tbl$d.CI_high <- ES$CI_high
  }

  # p values
  tbl$p.raw <- 2 * stats::pnorm(abs(tbl$z.value), lower.tail = FALSE)
  tbl[paste0("p.", adjust)] <- stats::p.adjust(tbl$p.raw, method = adjust)

  return(tbl)
}
