% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contrast_weights.R
\name{contrast_weights}
\alias{contrast_weights}
\alias{cw}
\title{Build Contrast Weights}
\usage{
contrast_weights(..., .name = "custom", .adjust = NULL)

cw(..., .name = "custom", .adjust = NULL)
}
\arguments{
\item{...}{Can be:
\itemize{
\item Unnamed scalars.
\item (Possibly named) vectors if equal length
}}

\item{.name}{The label as it will appear in the results in \code{emmeans}.}

\item{.adjust}{Gives the default adjustment method for multiplicity (used in
\code{emmeans}).}
}
\value{
Depending on input, either a vector or a data frame of scaled weights.
}
\description{
To be used ideally with \link[emmeans:contrast]{emmeans::contrast}. Each contrasts is tested (sum to
0?) and scaled so that all positive weights sum to 1 (and all negative
weights to -1).
}
\examples{
data(mtcars)

mod <- lm(mpg ~ factor(cyl) * am, mtcars)


my_contrasts <- data.frame(
  "squares" = c(-1, 2, -1),
  "4 vs 6" = c(-30, 30, 0),
  check.names = FALSE
)

(my_contrasts2 <- cw(my_contrasts))
my_contrasts3 <- cw(my_contrasts, .adjust = "fdr")

\dontshow{if (require("emmeans")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(emmeans)
(emms <- emmeans(mod, ~ cyl + am))

contrast(emms, method = my_contrasts, by = "am")
contrast(emms, method = my_contrasts2, by = "am") # estimate is affected!
contrast(emms, method = my_contrasts3, by = "am") # p value is affected

# Also in interaction contrasts
contrast(emms, interaction = list(cyl = my_contrasts2, am = "pairwise"))
\dontshow{\}) # examplesIf}
}
