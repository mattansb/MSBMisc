#' Residualize data over a data grid
#'
#' Allows for the plotting of **partial regression plots**.
#'
#' @param grid A data grid. Data must be completely crossed (e.g., made with `expand.grig()`).
#' @param model The original model object.
#' @param ... Arguments passed to [residuals()].
#'
#' @example examples/examples.residualize_over_grid.R
#'
#' @export
residualize_over_grid <- function(grid, model, ...) {
  UseMethod("residualize_over_grid")

}

#' @export
#' @rdname residualize_over_grid
#' @param pred_name Additional column name in `grid` that contains the predicted
#'   outcome (on the response scale).
residualize_over_grid.data.frame <- function(grid, model, pred_name, ...) {
  stopifnot("{insight} is required for this function" = requireNamespace("insight"))

  mdata <- insight::get_predictors(model)
  fun_link <- insight::link_function(model)
  inv_fun <- insight::link_inverse(model)
  predicted <- grid[[pred_name]]


  is_fixed <- sapply(lapply(grid, unique), length) == 1

  grid <- grid[, !is_fixed, drop = FALSE]
  grid[[pred_name]] <- NULL
  stopifnot("grid must be a fully crossed grid" = .is_grid(grid))


  mdata <- mdata[, intersect(names(grid), names(mdata)), drop = FALSE]


  # for each var
  best_match <- NULL
  for (p in names(mdata)) {
    if (is.factor(mdata[[p]]) ||
        is.logical(mdata[[p]]) ||
        is.character(mdata[[p]])) {
      grid[[p]] <- as.character(grid[[p]])
      mdata[[p]] <- as.character(mdata[[p]])
    } else {
      grid[[p]] <- .validate_num(grid[[p]])
    }

    # if factor / logical / char in old data, find where it is equal
    # if numeric in old data, find where it is closest
    best_match <- .closest(mdata[[p]],
                           grid[[p]],
                           best_match = best_match)
  }

  idx <- apply(best_match, 2, which)

  points <- grid[idx, , drop = FALSE]
  points[[pred_name]] <- inv_fun(
    fun_link(predicted[idx]) + residuals(model, ...) # add errors
  )

  return(points)
}

#' @export
#' @rdname residualize_over_grid
#' @param protect_gge_names Return the data with the original columns names
#'   (`x`, `group`, `facet`, `panel`)?
residualize_over_grid.ggeffects <- function(grid, model, protect_gge_names = TRUE, ...) {
  names_gge <- c("x", "group", "facet","panel")
  names_orig <- attr(grid,"terms")

  grid <- as.data.frame(grid)
  grid <- grid[names(grid) %in% c(names_gge, "predicted")]
  names(grid)[names(grid) %in% names_gge] <- names_orig

  points <- residualize_over_grid(grid, model, pred_name = "predicted", ...)

  if (protect_gge_names) {
    for (i in seq_along(names_orig)) {
      names(points)[names(points) == names_orig[i]] <- names_gge[i]
    }
  }

  return(points)
}


#' @keywords internal
.is_grid <- function(df) {
  unq <- lapply(df, unique)

  if (prod(sapply(unq, length)) != nrow(df)) {
    return(FALSE)
  }

  target <- do.call(expand.grid, args = unq)
  target$..1 <- 1

  res <- merge(df, target, by = names(df), all = TRUE)

  return(sum(res$..1) == sum(target$..1))
}


#' @keywords internal
.closest <- function(x, target, best_match) {
  if (is.numeric(x)) {
    AD <- abs(outer(x, target, FUN = `-`))
    idx <- apply(AD, 1, function(x) x == min(x))
  } else {
    idx <- t(outer(x, target, FUN = `==`))
  }

  if (is.matrix(best_match)) {
    idx <- idx & best_match
  }

  return(idx)
}

#' @keywords internal
.validate_num <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  return(x)
}
