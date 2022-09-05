#' Vectorised Extract and Replace from Matrix
#'
#' @param x A matrix
#' @param ... vectors of indices (numeric or char)
#' @param value vector of values to replace
#'
#' @examples
#'
#' (M <- matrix(1:12, 3, 4))
#'
#' index_matrix(M, 1:2, 3:4) # get values at (i = 1, j = 3) and (2, 4)
#'
#' # same as:
#' c(M[1, 3], M[2, 4])
#'
#' # replace values
#' index_matrix(M, 1:2, 3:4) <- NA
#'
#' M
#'
#' @export
index_matrix <- function(x, ...) {
  UseMethod("index_matrix")
}

#' @export
#' @rdname index_matrix
index_matrix.matrix <- function(x, ...) {
  .test_matrix_idx(list(...))

  mapply(\(...) x[...], ..., USE.NAMES = FALSE)
}

#' @export
#' @rdname index_matrix
"index_matrix<-" <- function(x, ..., value) {
  UseMethod("index_matrix<-")
}

#' @export
#' @rdname index_matrix
"index_matrix<-.matrix" <- function(x, ..., value) {
  ind_and_value <- data.frame(..., value)
  nc <- ncol(ind_and_value)

  .test_matrix_idx(as.list(ind_and_value[,-nc]))

  for (r in seq_len(nrow(ind_and_value))) {
    args <- c(list(x),
              as.data.frame(
                as.list(ind_and_value[r,-nc])
              ),
              ind_and_value[r,nc])
    names(args)[seq_len(nc-1)] <- ""

    x <- do.call("[<-", args)
  }

  x
}

#' @keywords internal
.test_matrix_idx <- function(idx) {
  stopifnot(
    "Indices must be numeric" = all(sapply(idx, is.numeric) | sapply(idx, is.character)),
    "Indices must be positive" = idx |>
      Filter(is.numeric, x = _) |>
      lapply(sign) |>
      lapply("==", 1) |>
      unlist() |>
      all())
}
