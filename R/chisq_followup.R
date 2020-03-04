#' Follow-up for contingency table test
#'
#' See also \code{mcnemar.test()}
#'
#' @param Xsq Result from \code{chisq.test}
#' @param population_in_row Comparisons by row? (If not, by column.)
#' @param adjust Method for correcting p-values. See \code{\link[stats]{p.adjust}}.
#' @param ... Passed to \code{chisq.test}.
#'
#' @example examples/examples.chisq_followup.R
#'
#' @export
chisq.pairwise <- function(Xsq,
                           population_in_row = TRUE,
                           adjust = p.adjust.methods,
                           ...) {
  adjust <- match.arg(adjust)
  tbl <- Xsq$observed

  if (!population_in_row)
    tbl <- t(tbl)
  popsNames <- rownames(tbl)


  pairs <- combn(1:nrow(tbl), 2)
  res <- lapply(seq_len(ncol(pairs)), function(i) {
    pair <- pairs[, i]
    temp_res <- chisq.test(tbl[pair,], ...)

    data.frame(
      comparison = paste(popsNames[pair], collapse = " vs. "),
      Chi.sq = temp_res$statistic,
      df = temp_res$parameter,
      p.raw = temp_res$p.value
    )
  })

  res <- do.call(rbind, res)
  res[[paste0("p.", adjust)]] <-
    p.adjust(res$p.raw, method = adjust)
  rownames(res) <- NULL
  return(res)
}


#' @rdname chisq.pairwise
#' @export
chisq.residual <- function(Xsq,
                           adjust = p.adjust.methods) {
  adjust <- match.arg(adjust)

  tbl <- data.frame(Xsq$residuals)
  tbl$z.value <- tbl$Freq
  tbl$Freq <- NULL
  tbl$p.raw <- 2 * pnorm(abs(tbl$z.value), lower.tail = FALSE)
  tbl[paste0("p.", adjust)] <- p.adjust(tbl$p.raw, method = adjust)

  return(tbl)
}
