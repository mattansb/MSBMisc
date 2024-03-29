% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_library.R
\name{print_library}
\alias{print_library}
\alias{print_require}
\alias{print_library_md}
\title{Print Loading/Attaching of Packages}
\usage{
print_library(..., .character.only = FALSE, .version = TRUE, .load = TRUE)

print_require(..., .character.only = FALSE, .version = TRUE, .load = TRUE)

print_library_md(..., .character.only = FALSE, .version = TRUE, .load = TRUE)
}
\arguments{
\item{...}{Names of packages.}

\item{.character.only}{Is \code{...} from characters?}

\item{.version}{Print library version?}

\item{.load}{Load package, or just print?}
}
\description{
Useful in RMarkdown.
}
\examples{
print_library(afex, tidyverse, emmeans, MASS,
  .load = FALSE
)

}
