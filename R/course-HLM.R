# Pseudo R-square -----------------------------------------------------------

#' Compute pseudo R-square for mixed models
#'
#' This function computes the pseudo R-square for mixed models by quantifying
#' the reduction on the variance-covariance between a full model and a reduced
#' model.
#'
#' @param mf The full model
#' @param mr The reduced model
#' @param mnull The empty model, where some variance of interest is unmodeled
#'   (an "unconditional" model)
#'
#' @details
#' Variance components are extracted from the 3 models, and where they match
#' (groups and terms must match _exactly_), we compute:
#'
#' \deqn{R^2 = \frac{V_{full} - V_{restricted}}{V_{null}}}
#'
#' This gives the increase in variance explained by the full model compared to
#' the reduced model. When the restricted model is the empty model, this is the
#' same as:
#'
#' \deqn{R^2 = 1 - \frac{V_{full}}{V_{restricted}}}
#'
#' ## Supported model classes
#' - `{stats}`: `lm`, `glm`
#' - `{lme4}`: `lmerMod` and `glmerMod`
#' - `{rstanarm}`: `stanreg`
#' - `{brms}`: `brmsfit`
#' - `{glmmTMB}`: `glmmTMB`
#'
#' @return A tibble with the following columns:
#' - `grp`: The name of the grouping factor
#' - `var`: The name of the variance component
#' - `r2`: The pseudo R-square for that variance component
#'
#' @examplesIf require("lme4")
#'
#' mod0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
#' mod1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
#' mod2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
#'
#' r2_pseudo(mod1, mod0) # Residual variance explained by the fixed effect
#' r2_pseudo(mod2, mod1, mod0) # Residual variance explained by the random effects
#' r2_pseudo(mod2, mod0) # Residual variance explained by the mixed effects
#'
#' @family HLM4Psych
#' @export
r2_pseudo <- function(mf, mr, mnull = mr) {
  V_full <- V_table(mf)
  V_restricted <- V_table(mr)
  V_empty <- V_table(mnull)

  V_full |>
    dplyr::inner_join(V_restricted, by = c("grp", "var")) |>
    dplyr::inner_join(V_empty, by = c("grp", "var")) |>
    dplyr::mutate(
      r2 = (.data$vcov.y - .data$vcov.x) / .data$vcov,
    ) |>
    dplyr::select(dplyr::all_of(c("grp", "var", "r2")))
}

#' @export
#' @rdname r2_pseudo
#' @param model A fitted model object
V_table <- function(model) {
  UseMethod("V_table")
}

#' @export
V_table.lm <- function(model) {
  tibble::tibble(
    grp = "Residual",
    var = NA,
    vcov = stats::sigma(model)^2
  )
}

#' @export
V_table.glm <- V_table.lm

#' @export
V_table.lmerMod <- function(model) {
  tibble::as_tibble(lme4::VarCorr(model)) |>
    dplyr::filter(is.na(.data$var2)) |>
    dplyr::select(-dplyr::all_of(c("sdcor", "var2"))) |>
    dplyr::rename(dplyr::all_of(c(var = "var1")))
}

#' @export
V_table.glmerMod <- function(model) {
  res <- tibble::tibble(
    grp = "Residual",
    var = NA,
    vcov = stats::sigma(model)^2
  )

  V_table.lmerMod(model) |>
    dplyr::bind_rows(res)
}

#' @export
V_table.stanreg <- V_table.lmerMod

#' @export
V_table.brmsfit <- function(model) {
  V <- lme4::VarCorr(model)

  purrr::imap(V, \(v, grp) {
    x <- v[["sd"]][, "Estimate"]
    tibble::tibble(
      grp = ifelse(grp == "residual__", "Residual", grp),
      var = names(x),
      vcov = x^2
    )
  }) |>
    purrr::list_rbind()
}

#' @export
V_table.glmmTMB <- function(model) {
  V <- lme4::VarCorr(model)

  L <- purrr::imap(V[["cond"]], \(v, grp) {
    x <- diag(v)
    tibble::tibble(
      grp = grp,
      var = names(x),
      vcov = x
    )
  })

  L |>
    append(
      list(tibble::tibble(grp = "Residual", vcov = attr(V[["cond"]], "sc")))
    ) |>
    purrr::list_rbind()
}

#' @export
V_table.default <- function(model) {
  f <- ls(all.names = TRUE, pattern = "^V_table\\.", envir = .GlobalEnv)
  cls <- sub("^V_table\\.", "", f)
  stop(
    sprintf(
      "No method for objects of class '%s'\n\nSupported classes:\n%s",
      class(model)[1],
      paste0(cls, collapse = ", ")
    ),
    call. = FALSE
  )
}
