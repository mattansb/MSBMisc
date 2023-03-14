#' Compare multiple vectors
#'
#' @param x,y,... Vectors, typically numerical, to be compared.
#'
#' @return A logical vector. For the operator, a `last_y` attribute stores the
#'   last RHS values from the comparisons (strip away with `as.vector()`). See
#'   examples.
#'
#' @examples
#' x <- c(1, 3, 1, 1, 2)
#' y <- c(2, 2, 1, 1, 1)
#' z <- c(3, 1, 1, 2, 1)
#'
#'
#' lt(x, y, z) # >
#' eq(x, y, z) # ==
#' neq(x, y, z) # !=
#' leq(x, y, z) # <=
#' geq(x, y, z) # >=
#'
#' gt(x, y, z) # <
#'
#' # same as
#' x %>>% y %>>% z
#'
#' # same as
#' x > y & y > z
#'
#' # Operators can be mixed!
#'
#' x %>>% y %==% z
#'
#' # Or broken
#' (l1 <- x %>>% y)
#'
#' (l2 <- l1 %==% z)
#'
#' # same as
#' x > y & y == z
#'
#' as.vector(l2)
#'
#' @export
gt <- function(...) {
  .vect_lgl_comps(..., type = ">")
}

#' @export
#' @rdname gt
lt <- function(...) {
  .vect_lgl_comps(..., type = "<")
}

#' @export
#' @rdname gt
eq <- function(...) {
  .vect_lgl_comps(..., type = "==")
}

#' @export
#' @rdname gt
neq <- function(...) {
  .vect_lgl_comps(..., type = "!=")
}

#' @export
#' @rdname gt
leq <- function(...) {
  .vect_lgl_comps(..., type = "<=")
}

#' @export
#' @rdname gt
geq <- function(...) {
  .vect_lgl_comps(..., type = ">=")
}




# Opers -------------------------

#' @export
#' @rdname gt
"%>>%" <- function(x, y) {
  .op_lgl_comps(x, y, ">")
}

#' @export
#' @rdname gt
"%<<%" <- function(x, y) {
  .op_lgl_comps(x, y, "<")
}

#' @export
#' @rdname gt
"%==%" <- function(x, y) {
  .op_lgl_comps(x, y, "==")
}

#' @export
#' @rdname gt
"%!=%" <- function(x, y) {
  .op_lgl_comps(x, y, "!=")
}

#' @export
#' @rdname gt
"%<=%" <- function(x, y) {
  .op_lgl_comps(x, y, "<=")
}

#' @export
#' @rdname gt
"%>=%" <- function(x, y) {
  .op_lgl_comps(x, y, ">=")
}

# Internals -------------------------


#' @keywords internal
.vect_lgl_comps <- function(..., type) {
  x <- list(...)
  Ls <- lengths(x)
  stopifnot(length(unique(Ls)) == 1L || (1 %in% Ls && length(unique(Ls)) == 2L))

  f <- match.fun(type)

  out <- rep(TRUE, length = max(Ls))
  for (i in seq_len(length(x) - 1)) {
    out <- out & f(x[[i]], x[[i + 1]])
  }
  out
}

.op_lgl_comps <- function(x, y, type) {
  RHS0 <- attr(x, "last_y")

  f <- match.fun(type)

  if (is.null(RHS0)) {
    out <- f(x, y)
  } else {
    out <- x & f(RHS0, y)
  }

  attr(out, "last_y ") <- y
  out
}


# Examples -----------------------
