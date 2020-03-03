\dontrun{
  M <- as.table(rbind(c(762, 327, 468),
                      c(484, 239, 477)))
  dimnames(M) <- list(
    gender = c("F", "M"),
    party = c("Democrat", "Independent", "Republican")
  )
  M

  res <- chisq.test(M)
  chisq.pairwise(res)
  chisq.pairwise(res, population_in_row = FALSE)
  chisq.residual(res)
}
