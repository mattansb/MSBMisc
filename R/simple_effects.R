#' Compute Simple Effects Omnibus Tests
#'
#' This is a wrapper for `emmeans::joint_tests()` that provides an easy way to
#' specify which simple effects we wish to test, and within what variable(s).
#'
#' @param model The model.
#' @param effect The name of the required simple effect. e.g., `"A"` for a
#'   simple effect of *A*, or `"A:B"` for a simple *A-by-B* interaction.
#' @param inside A vector of the name(s) of the variable(s) within whose levels
#'   the `effect` will be tested. Can also be the name of an interaction (e.g.,
#'   `"B:C"`). If not specified, will use all the terms not in `effect`.
#' @param ... Passed to `emmeans::joint_tests()`, e.g., `cov.reduce`, `at`, etc.
#'
#'
#' @examplesIf require("ggplot2") && require("insight") && require("stringr")
#' library(afex)
#'
#' data(obk.long, package = "afex")
#' A <- aov_car(value ~ treatment * gender + Error(id/(phase*hour)),
#'              data = obk.long)
#'
#' simple_effects(A, effect = "treatment")
#'
#' simple_effects(A, effect = "treatment:phase")
#'
#' simple_effects(A, effect = "phase", inside = "treatment")
#'
#' simple_effects(A, effect = "phase", inside = c("treatment", "gender"))
#' # simple_effects(A, effect = "phase", inside = "treatment:gender") # same
#'
#' simple_effects(A, effect = "phase", inside = c("treatment", "gender"),
#'                at = list(gender = "F"))
#'
#' simple_effects(A, effect = "phase:treatment", inside = "gender")
#'
#'
#' @export
simple_effects <- function(model, effect, inside, ...) {
  UseMethod("simple_effects")
}

#' @export
simple_effects.lm <- function(model, effect, inside, ...) {
  .check_namespace("insight", "emmeans", "stringr")

  stopifnot("Effect must me a char of length 1." = is.character(effect) && length(effect) == 1L)

  if (missing(effect)) {
    stop("'effect' must be specified.")
  }

  effects <- unique(unlist(stringr::str_split(effect, pattern = ":")))
  IVs <- insight::find_predictors(model)[[1]]

  if (missing(inside)) {
    inside <- setdiff(IVs, effects)
  } else {
    inside <- unique(unlist(stringr::str_split(inside, pattern = ":")))
  }

  jt <- emmeans::joint_tests(model, by = inside, ...)

  other_ivs <- setdiff(IVs, c(effects, inside))
  i <- outer(jt[["model term"]], effects, stringr::str_detect)
  i <- apply(i, 1, all)
  if (length(other_ivs)) {
    iX <- outer(jt[["model term"]], other_ivs, stringr::str_detect)
    iX <- apply(iX, 1, any)
    i <- i & !iX
  }

  jt <- jt[i, ]
  jt[["model term"]] <- NULL
  class(jt) <- c("summary_emm", class(jt))


  cl <- quote(stats::update(jt))
  cl$by <- if (length(inside) > 1L) utils::tail(inside, -1)
  cl$mesg <- paste0("Omnibus test for ",
                    paste0(rep("simple", length(inside)), collapse = "-"),
                    " effect of ",
                    effect,
                    ".\n")
  jt <- eval(cl)

  if (length(inside) == 1L) jt <- stats::update(jt, by = NULL)

  jt
}

#' @export
simple_effects.afex_aov <- simple_effects.lm
