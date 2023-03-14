#' Compare the frequencies of levels of a factor
#'
#' Using [`stats::mcnemar.test`] for comparing dependent proportions.
#' \cr\cr
#' This is function is dubious. Best not to use it.
#'
#' @param f A factor vector.
#' @param adjust Method for correcting p-values. See [`stats::p.adjust`].
#' @param correct a logical indicating whether to apply continuity correction when computing the test statistic.
#'
#' @examples
#' f <- c(
#'   rep("A", 12),
#'   rep("B", 45),
#'   rep("C", 42),
#'   rep("D", 20)
#' )
#'
#' compare_freqs(f)
#'
#' @export
compare_freqs <- function(f, adjust = stats::p.adjust.methods, correct = TRUE) {
  warning("This is function is dubious. Best not to use it.", call. = FALSE, immediate. = TRUE)
  adjust <- match.arg(adjust)

  levels <- unique(f)
  pairs <- utils::combn(seq_len(length(levels)), 2)

  res <- lapply(seq_len(ncol(pairs)), function(i) {
    k1 <- pairs[1, i]
    k2 <- pairs[2, i]
    level1 <- levels[k1]
    level2 <- levels[k2]

    f1 <- f == level1
    f2 <- f == level2

    tab <- table(f1, f2)

    temp_res <- stats::mcnemar.test(tab, correct = correct)

    data.frame(
      comparison = paste(level1, " vs. ", level2),
      prop1 = mean(f1),
      prop2 = mean(f2),
      Chi.sq = temp_res$statistic,
      df = temp_res$parameter,
      p.raw = temp_res$p.value
    )
  })

  res <- do.call(rbind, res)
  res[[paste0("p.", adjust)]] <- stats::p.adjust(res$p.raw, method = adjust)
  rownames(res) <- NULL
  return(res)
}
