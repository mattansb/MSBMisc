#' Get raw data for plotting with model predictions
#'
#' @param grid A data grid with predictions
#' @param model The statistical model
#' @param residualize Should data be residualized?
#' @param collapse_by Name of grouping variable to collaple across. If `TRUE`
#'   name of grouping variable is automatically detected from the model.
#' @param ... Args passed from / to other functions.
#'
#' @examplesIf require("insight")
#'
#' data("mtcars")
#' mtcars <- mtcars |> transform(cyl = factor(cyl))
#' mod <- lm(mpg ~ hp + cyl, data = mtcars[1:10, ])
#'
#' nd <- expand.grid(hp = seq(50, 350, by = 50),
#'                   cyl = "4")
#'
#' nd$predicted_mpg <- predict(mod, newdata = nd)
#'
#' get_data_for_grid(nd, mod)
#'
#' get_data_for_grid(nd, mod, residualize = TRUE, pred_name = "predicted_mpg")
#'
#' @examplesIf require("insight") && require("ggeffects")
#'
#' library(ggplot2)
#' ggplot(nd, aes(hp, predicted_mpg)) +
#'   geom_line() +
#'   geom_point(aes(y = mpg, color = "Raw"),
#'              data = get_data_for_grid(nd, mod)) +
#'   geom_point(aes(color = "Residualized"),
#'              data = get_data_for_grid(nd, mod, residualize = TRUE, pred_name = "predicted_mpg")) +
#'   labs(title = "Partial residual plot",
#'        color = "Data")
#'
#' ## Support of data-gripd + prediction packages ------
#' # - ggeffects
#' # - emmeans
#' # - marginaleffects
#'
#' @examplesIf require("insight") && require("ggeffects")
#' pred_ggeffects <- ggeffects::ggpredict(mod, c("hp [50:350, by = 50]", "cyl [4]"))
#' get_data_for_grid(pred_ggeffects, residualize = TRUE)
#'
#' @examplesIf require("insight") && require("emmeans")
#' at <- list(hp = seq(50, 350, by = 50), cyl = 4)
#' pred_emmeans <- emmeans::emmeans(mod, ~ hp + cyl, at = at)
#' get_data_for_grid(pred_emmeans, mod, residualize = TRUE)
#'
#' @examplesIf require("insight") && require("marginaleffects")
#' pred_marginaleffects <- marginaleffects::predictions(mod, newdata = nd)
#' get_data_for_grid(pred_marginaleffects, residualize = TRUE)
#'
#'
#' @examplesIf require("insight") && require("marginaleffects") &&  require("lme4")
#' ## Collapes across group ------
#' fm1 <- lme4::lmer(angle ~ temperature + (1|recipe),
#'                   data = cake)
#'
#' pred_ggeffects <- ggeffects::ggpredict(fm1, c("temperature", "recipe"))
#' nd <- marginaleffects::datagrid(temperature = unique(cake$temperature),
#'                                 model = fm1)
#' pred_marginaleffects <- marginaleffects::predictions(fm1, newdata = nd)
#'
#' get_data_for_grid(pred_marginaleffects, collapse_by = TRUE)
#' get_data_for_grid(pred_marginaleffects, collapse_by = TRUE, residualize = TRUE)
#'
#'
#' @export
get_data_for_grid <- function(grid, model, residualize = FALSE, collapse_by = FALSE, ...) {
  if (getOption("get_data_for_grid.warn", TRUE)) {
    warning("'get_data_for_grid()' is experimental.", call. = FALSE)
    options(get_data_for_grid.warn = FALSE)
  }

  .check_namespace("insight")
  UseMethod("get_data_for_grid")
}


#' @export
#' @rdname get_data_for_grid
#' @param pred_name Name of column that has the predictions in the data grid
get_data_for_grid.data.frame <- function(grid, model, residualize = FALSE, collapse_by = FALSE,
                                         pred_name, ...) {


  data <- insight::get_data(model)

  if (isTRUE(residualize)) {
    data <- residualize_over_grid(data, grid, model, pred_name)
  }

  if (!isFALSE(collapse_by)) {
    data <- collapse_by_group(data, grid, model, collapse_by, pred_name)
  }


  data
}


#' @export
#' @rdname get_data_for_grid
#' @param protect_names Logical, if `TRUE`, preserves column names from the
#'   `ggeffects` object.
get_data_for_grid.ggeffects <- function(grid, model, residualize = FALSE, collapse_by = FALSE,
                                        protect_names = TRUE, ...) {
  new_d <- as.data.frame(grid)
  new_d <- new_d[colnames(new_d) %in% c("x", "group", "facet", "panel", "predicted")]

  colnames(new_d)[colnames(new_d) %in% c("x", "group", "facet","panel")] <- attr(grid, "terms")

  if (missing(model)) {
    model <- get(attr(grid, "model.name"), envir = parent.frame())
  }

  points <- get_data_for_grid.data.frame(
    new_d, model,
    residualize = residualize,
    collapse_by = collapse_by,
    pred_name = "predicted"
  )

  if (protect_names && !is.null(points)) {
    colnames_gge <- c("x", "group", "facet","panel")
    colnames_orig <- attr(grid,"terms")
    for (i in seq_along(colnames_orig)) {
      colnames(points)[colnames(points) == colnames_orig[i]] <- colnames_gge[i]
    }
  }

  points
}


#' @export
#' @rdname get_data_for_grid
get_data_for_grid.emmGrid <- function(grid, model, residualize = FALSE, collapse_by = FALSE,
                                      protect_names = TRUE, ...) {

  .check_namespace("emmeans")

  grid <- emmeans::regrid(grid, transform = "response")
  s <- as.data.frame(grid)
  pred_name <- grid@misc[["estName"]]

  get_data_for_grid.data.frame(
    s[c(grid@roles[["predictors"]], pred_name)], model,
     residualize = residualize,
     collapse_by = collapse_by,
     pred_name = pred_name
  )
}


#' @export
#' @rdname get_data_for_grid
get_data_for_grid.predictions <- function(grid, model, residualize = FALSE, collapse_by = FALSE,
                                          ...) {
  stopifnot("Type must be 'response'." = attr(grid, "type") == "response")

  if (missing(model)) {
    model <- attr(grid, "model")
  }

  get_data_for_grid.data.frame(
    grid[c(unlist(attr(grid, "variables")), "predicted")], model,
    residualize = residualize,
    collapse_by = collapse_by,
    pred_name = "predicted"
  )
}


# Resid and collapse ------------------------------------------------------


#' @keywords internal
residualize_over_grid <- function(data, grid, model, pred_name) {
  fun_link <- insight::link_function(model)
  inv_fun <- insight::link_inverse(model)

  predicted <- grid[[pred_name]]
  grid[[pred_name]] <- NULL

  is_fixed <- sapply(grid, function(x) length(unique(x))) == 1
  grid <- grid[,!is_fixed, drop = FALSE]
  other_columns <- data[setdiff(colnames(data), c(insight::find_response(model), colnames(grid)))]
  data <- data[intersect(colnames(data), colnames(grid))]

  if (!.is_grid(grid)) {
    stop("Grid for partial residuals must be a fully crossed grid.")
  }

  # for each var
  best_match <- NULL

  for (p in colnames(data)) {
    if (is.factor(data[[p]]) || is.logical(data[[p]]) || is.character(data[[p]])) {
      grid[[p]] <- as.character(grid[[p]])
      data[[p]] <- as.character(data[[p]])
    } else {
      grid[[p]] <- .validate_num(grid[[p]])
    }

    # if factor / logical / char in old data, find where it is equal
    # if numeric in old data, find where it is closest
    best_match <- .closest(data[[p]], grid[[p]], best_match = best_match)
  }

  idx <- apply(best_match, 2, which)
  idx <- sapply(idx, "[", 1)

  res <- stats::residuals(model, type = "working")

  points <- grid[idx, , drop = FALSE]
  points[[pred_name]] <- inv_fun(fun_link(predicted[idx]) + res) # add errors

  cbind(other_columns, points)
}


#' @keywords internal
collapse_by_group <- function(data, grid, model, collapse_by, pred_name) {
  if (!insight::is_mixed_model(model)) {
    stop("This function only works with mixed effects models.", call. = FALSE)
  }

  if (isTRUE(collapse_by)) {
    collapse_by <- insight::find_random(model, flatten = TRUE)
  }

  if (length(collapse_by) > 1) {
    collapse_by <- collapse_by[1]
    warning("More than one random grouping variable found.",
            "\n  Using `", collapse_by, "`.", call. = FALSE)
  }

  if (!collapse_by %in% colnames(data)) {
    stop("Could not find `", collapse_by, "` column.", call. = FALSE)
  }

  if ((resp_name <- insight::find_response(model)) != pred_name && resp_name %in% colnames(data)) {
    colnames(data)[colnames(data) == resp_name] <- pred_name
  }

  data <- data[,colnames(data) %in% c(colnames(grid), pred_name, collapse_by), drop = FALSE]

  agg_data <- stats::aggregate(data[[pred_name]],
                               by = data[colnames(data) != pred_name],
                               FUN = mean)

  colnames(agg_data)[ncol(agg_data)] <- pred_name

  agg_data
}



# Utils -------------------------------------------------------------------

#' @keywords internal
.is_grid <- function(df) {
  unq <- lapply(df, unique)

  if (prod(lengths(unq)) != nrow(df)) {
    return(FALSE)
  }

  df2 <- do.call(expand.grid, args = unq)
  df2[["..1"]] <- 1

  res <- merge(df,df2, by = colnames(df), all = TRUE)

  return(sum(res[["..1"]]) == sum(df2[["..1"]]))
}


#' @keywords internal
.closest <- function(x, target, best_match) {
  if (is.numeric(x)) {
    # AD <- outer(x, target, FUN = function(x, y) abs(x - y))
    AD <- abs(outer(x, target, FUN = `-`))
    idx <- apply(AD, 1, function(x) x == min(x))
  } else {
    idx <- t(outer(x, target, FUN = `==`))
  }

  if (is.matrix(best_match)) {
    idx <- idx & best_match
  }

  idx
}


#' @keywords internal
.validate_num <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(as.character(x))
  }
  x
}
