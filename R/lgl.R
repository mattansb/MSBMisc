#' It's just logical
#'
#' @param x,... Values to be tested.
#'
#' @examples
#' x <- list(1, TRUE, list(TRUE), FALSE, "Hello world!")
#'
#' is.TRUE(x)
#'
#' is.FALSE(x)
#'
#' allTRUE(TRUE, FALSE, stop("NOOOO"))
#'
#' anyTRUE(TRUE, FALSE, stop("NOOOO"))
#'
#' @export
is.TRUE <- Vectorize(isTRUE, "x", SIMPLIFY = TRUE)

#' @export
#' @rdname is.TRUE
is.FALSE <- Vectorize(isFALSE, "x", SIMPLIFY = TRUE)


#' @export
#' @rdname is.TRUE
allTRUE <- function(...) {
  cl <- match.call()
  i <- 2
  verdict <- TRUE
  while (verdict && i <= length(cl)) {
    verdict <-
      verdict &&
        eval(cl[[i]], envir = parent.frame())
    i <- i + 1
  }
  return(verdict)
}

#' @export
#' @rdname is.TRUE
anyTRUE <- function(...) {
  cl <- match.call()
  i <- 2
  verdict <- FALSE
  while (!verdict && i <= length(cl)) {
    verdict <-
      verdict ||
        eval(cl[[i]], envir = parent.frame())
    i <- i + 1
  }
  return(verdict)
}
