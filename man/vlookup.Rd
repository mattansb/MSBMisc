% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vlookup.R
\name{vlookup}
\alias{vlookup}
\title{vlookup}
\usage{
vlookup(this, data, key, value, add = FALSE)
}
\arguments{
\item{this}{A vector of values}

\item{data}{A data frame to search in}

\item{key}{Where should \code{this} be looked up?}

\item{value}{Name of the column from which values should be returned.}

\item{add}{Should \code{this} and the resulting values be returned as a data frame? (Else a vector)}
}
\description{
vlookup
}
\examples{
df <- data.frame(
  a = letters[c(1, 1:9)],
  b = 51:60
)

vlookup(c("a", "e", "c"), df, key = "a", value = "b")
vlookup(c("a", "e", "c"), df, key = "a", value = "b", add = TRUE)

}
