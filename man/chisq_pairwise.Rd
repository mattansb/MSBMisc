% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq_followup.R
\name{chisq_pairwise}
\alias{chisq_pairwise}
\alias{chisq_residual}
\title{Follow-up for contingency table test}
\usage{
chisq_pairwise(Xsq, population_in_row = TRUE, adjust = p.adjust.methods, ...)

chisq_residual(Xsq, adjust = p.adjust.methods)
}
\arguments{
\item{Xsq}{Result from \code{chisq.test()}}

\item{population_in_row}{Comparisons by row? (If not, by column.)}

\item{adjust}{Method for correcting p-values. See \code{\link[stats:p.adjust]{stats::p.adjust}}.}

\item{...}{Passed to \code{chisq.test()}.}
}
\description{
Follow-up for contingency table test
}
\examples{
M <- as.table(rbind(c(762, 327, 468),
                    c(484, 239, 477)))
dimnames(M) <- list(
  gender = c("F", "M"),
  party = c("Democrat", "Independent", "Republican")
)
M

res <- chisq.test(M)
chisq_pairwise(res)
chisq_pairwise(res, population_in_row = FALSE)
chisq_residual(res)
}
