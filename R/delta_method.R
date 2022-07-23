#' Transform means a (co)variances using the *delta* method
#'
#' @param ... Unquoted transformations. See example.
#' @param .means A named vector of means.
#' @param .V A covariance matrix.
#'
#' @return A list with `means` and `V` of the transformed variables. (row/col)
#'   names are of the `g` transformations. See examples.
#'
#' @note Most of this function is mostly copied from `msm::deltamethod()`.
#'
#' @examples
#'
#' M <- sapply(mtcars, mean)
#' V <- cov(mtcars)
#'
#' delta_method(
#'   (mpg^2)/hp, log1p(am),
#'   .means = M, .V = V,
#'   return = "cor"
#' )
#'
#' # Sobel Test ----
#'
#' data("mtcars")
#' mod.y <- lm(mpg ~ hp + cyl, mtcars[1:5,])
#' mod.m <- lm(hp ~ cyl, mtcars[1:5,])
#'
#' bhat <- c(coef(mod.y), coef(mod.m))[c(2,5)]
#' Vhat <- dbind(vcov(mod.y), vcov(mod.m))[c(2,5), c(2,5)]
#'
#' res <- delta_method(
#'   hp * cyl,
#'   .means = bhat, .V = Vhat,
#'   return = c("means", "stddev")
#' )
#'
#' res$means / res$stddev
#'
#' # Compare:
#' (bhat[1] * bhat[2]) /
#'   sqrt(bhat[1]^2 * Vhat[2,2] + bhat[2]^2 * Vhat[1,1])
#'
#' # Special character will give you a bad time...
#' m <- lm(mpg ~ factor(cyl), mtcars[1:5,])
#'
#' bhat <- coef(m)
#' names(bhat) <- c("cyl4", "cyl6", "cyl8")
#' V <- vcov(m)
#'
#' delta_method(cyl4, cyl4 + cyl6, cyl4 + cyl8,
#'              .means = bhat,
#'              .V = V)
#'
#' @export
delta_method <- function(..., .means, .V, return = c("means", "cov", "stddev", "cor")) {

  if (all(diag(.V) == 1))
    warning("'V' should be a (co) variance matrix, but the diag is all 1s.")

  cl <- match.call()
  cl$.means <- cl$.V <- cl$return <- NULL
  g <- character(length(cl) - 1)
  for (gi in seq_along(g)) {
    g[gi] <- as.character(cl[gi + 1])
  }

  if (!is.null(names(cl)[-1])) {
    names(g) <- names(cl)[-1]
    blank_names <- names(g) == ""
    if (any(blank_names)) {
      names(g)[blank_names] <- g[blank_names]
    }
  } else {
    names(g) <- g
  }


  nms1 <- paste0("\\b", names(.means), "\\b")
  nms2 <- paste0("x", seq_along(.means))

  meansout <- g |>
    lapply(\(x) parse(text = x)) |>
    sapply(eval, envir = as.list(.means))

  gl <- vector("list", length = length(g))
  for (gi in seq_along(g)) {
    for (ti in seq_along(nms1)) {
      g[gi] <- gsub(x = g[gi], pattern = nms1[ti], replacement = nms2[ti])
    }
    gl[[gi]] <- stats::reformulate(g[gi])
  }
  names(gl) <- names(g)

  Vout <- msm.deltamethod(gl, .means, .V, FALSE)
  dimnames(Vout) <- list(nm <- names(g), nm)
  # attr(Vout, "correlation") <- cov2cor(Vout)

  list(
    means = stats::setNames(meansout, nm = nm),
    cov = Vout,
    stddev = sqrt(diag(Vout)),
    cor = cov2cor(Vout)
  )[return]
}

#' @keywords internal
msm.deltamethod <- function (g, mean, cov, ses = TRUE) {
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g))
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n))
    stop(paste("Covariances should be a ", n, " by ", n,
               " matrix"))
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(stats::deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}


# # gives similar results to msms....
# car::deltaMethod(object = sapply(mtcars, mean),
#                  g. = c("(mpg^2)/hp", "log1p(am)"),
#                  vcov. = cov(mtcars))
