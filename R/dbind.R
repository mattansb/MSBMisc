#' Bind matrices diagonally
#'
#' @param ... Matrices.
#' @param .fill Value to fill the off-"diagonal" values.
#'
#'
#' @examples

#' M1 <- matrix(1:9, 3, 3)
#' M2 <- matrix(10:15, 2, 2)
#'
#' dbind(M1, M2)
#' dbind(M1, M2, .fill = NA)
#'
#'
#'
#' M1 <- matrix(letters[1:9], 3, 3)
#' M2 <- matrix(LETTERS[10:15], 2, 2)
#'
#' dbind(M1, M2, .fill = "Banana")
#' dbind(M1, M2, .fill = NA)
#'
#' @export
dbind <- function(..., .fill = 0) {
  mats <- list(...) |> lapply(as.matrix)

  stopifnot("All inputs must be matrices." = all(sapply(mats, is.matrix)))

  n.row <- sapply(mats, nrow)
  n.col <- sapply(mats, ncol)

  end.rows <- cumsum(n.row)
  end.cols <- cumsum(n.col)

  start.rows <- end.rows - n.row + 1
  start.cols <- end.cols - n.col + 1

  out <- matrix(.fill, nrow = tail(end.rows, 1), ncol = tail(end.cols, 1),
                dimnames = list(1:sum(n.row), 1:sum(n.col)))
  for (m in seq_along(mats)) {
    out[start.rows[m]:end.rows[m],
        start.cols[m]:end.cols[m]] <- mats[[m]]

    if (!is.null(colnames(mats[[m]])))
      colnames(out)[start.cols[m]:end.cols[m]] <- colnames(mats[[m]])
    if (!is.null(rownames(mats[[m]])))
      rownames(out)[start.rows[m]:end.rows[m]] <- rownames(mats[[m]])
  }
  out
}
