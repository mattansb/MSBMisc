% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afex_to_BF.R
\name{afex_to_BF}
\alias{afex_to_BF}
\title{Refit afex models to produce Bayes Factors}
\usage{
afex_to_BF(fit, method = c("JZS", "BIC"), inclusion = TRUE, ...)
}
\arguments{
\item{fit}{An object of class afex_aov.}

\item{method}{Should BFs be computed with the BayesFactor package, or by comparing BICs.}

\item{inclusion}{Return inclusions BFs (for matched models) instead of the actual models' BFs.}

\item{...}{passed to \code{\link[BayesFactor:generalTestBF]{BayesFactor::generalTestBF()}}}
}
\description{
Refit afex models to produce Bayes Factors
}
\examples{
\dontrun{
  library(afex)

  data(obk.long, package = "afex")

  fit <- aov_ez(
    "id",
    "value",
    obk.long,
    within = c("phase", "hour"),
    covariate = "age",
    factorize = FALSE
  )
  fit
  afex_to_BF(fit)
}
}
