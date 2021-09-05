#' Follow-up for contingency table test
#'
#' @param Xsq Result from `chisq.test()`
#' @param population_in_row Comparisons by row? (If not, by column.)
#' @param adjust Method for correcting p-values. See [`stats::p.adjust`].
#' @param ... Passed to `chisq.test()`.
#'
#' @examples
#' M <- as.table(rbind(c(762, 327, 468),
#'                     c(484, 239, 477)))
#' dimnames(M) <- list(
#'   gender = c("F", "M"),
#'   party = c("Democrat", "Independent", "Republican")
#' )
#' M
#'
#' res <- chisq.test(M)
#' chisq_pairwise(res)
#' chisq_pairwise(res, population_in_row = FALSE)
#' chisq_residual(res)
#'
#' @export
chisq_pairwise <- function(Xsq,
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


#' @rdname chisq_pairwise
#' @export
chisq_residual <- function(Xsq,
                           adjust = p.adjust.methods) {
  adjust <- match.arg(adjust)

  tbl <- data.frame(Xsq$residuals)
  tbl$z.value <- tbl$Freq
  tbl$Freq <- NULL
  tbl$p.raw <- 2 * pnorm(abs(tbl$z.value), lower.tail = FALSE)
  tbl[paste0("p.", adjust)] <- p.adjust(tbl$p.raw, method = adjust)

  return(tbl)
}
