#' vlookup
#'
#' @param this A vector of values
#' @param data A data frame to search in
#' @param key Where should `this` be looked up?
#' @param value Name of the column from which values should be returned.
#' @param add Should `this` and the resulting values be returned as a data frame? (Else a vector)
#'
#' @example examples/examples.vlookup.R
#'
#' @export
vlookup <- function(this, data, key, value, add = FALSE) {
  if (length(this) > 1L) {
    res <-
      sapply(
        this,
        vlookup,
        data = data,
        key = key,
        value = value,
        add = FALSE
      )
  } else {
    m <- which(this == data[[key]])
    if (all(is.na(m))) {
      return(NA)
    }
    res <- data[[value]][m]
    if (length(res) > 1L) {
      warning("Found more than 1 match. Returning the first.", call. = FALSE)
      res <- res[1]
    }
  }

  if (add) {
    res <- data.frame(this, res)
    colnames(res) <- c(key, value)
  }

  return(res)
}
