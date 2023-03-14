#' Bind matrices diagonally
#'
#' @param ... Matrices.
#' @param .fill Value to fill the off-"diagonal" values. If `NULL`, the value is
#'   the default value of the inputs' mode.
#'
#'
#' @examples
#'
#' M1 <- matrix(1:8, 2, 4)
#' M2 <- matrix(9:14, 2, 3)
#' dbind(M1, M2)
#' dbind(M1, M2, .fill = NA)
#'
#'
#' M1 <- matrix(letters[1:4], 2, 2)
#' M2 <- matrix(LETTERS[5:10], 3, 2)
#' dbind(M1, M2)
#' dbind(M1, M2, .fill = "Banana")
#'
#'
#' M1 <- matrix(TRUE, 2, 3)
#' M2 <- matrix(NA, 3, 4)
#' dbind(M1, M2)
#'
#' @export
dbind <- function(..., .fill = NULL) {
  mats <- list(...) |> lapply(as.matrix)

  stopifnot(
    "All inputs must be matrices." = all(sapply(mats, is.matrix)),
    "All inputs must be of same mode." = length(unique(sapply(mats, mode))) == 1L
  )

  n.row <- sapply(mats, nrow)
  n.col <- sapply(mats, ncol)

  end.rows <- cumsum(n.row)
  end.cols <- cumsum(n.col)

  start.rows <- end.rows - n.row + 1
  start.cols <- end.cols - n.col + 1

  if (is.null(.fill)) .fill <- vector(mode = mode(mats[[1]]), length = 1)
  out <- matrix(.fill,
    nrow = utils::tail(end.rows, 1), ncol = utils::tail(end.cols, 1),
    dimnames = list(1:sum(n.row), 1:sum(n.col))
  )
  for (m in seq_along(mats)) {
    out[
      start.rows[m]:end.rows[m],
      start.cols[m]:end.cols[m]
    ] <- mats[[m]]

    if (!is.null(colnames(mats[[m]]))) {
      colnames(out)[start.cols[m]:end.cols[m]] <- colnames(mats[[m]])
    }
    if (!is.null(rownames(mats[[m]]))) {
      rownames(out)[start.rows[m]:end.rows[m]] <- rownames(mats[[m]])
    }
  }
  out
}
