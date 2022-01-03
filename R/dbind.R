#' Bind matrices diagonally
#'
#' @param ... Numerical matrices.
#' @param .fill Value to fill the off-"diagonal" values.
#'
#' @export
dbind <- function(..., .fill = 0) {
  mats <- list(...) |> lapply(as.matrix)

  stopifnot("All inputs must be matrices." = all(sapply(mats, is.matrix)),
            "All inputs must be numeric." = all(sapply(mats, is.numeric)))

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
