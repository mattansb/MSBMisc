#' Build Contrast Weights
#'
#' To be used ideally with [emmeans::contrast]. Each contrasts is tested (sum to
#' 0?) and scaled so that all positive weights sum to 1 (and all negative
#' weights to -1).
#'
#' @param ... Can be:
#'   - Unnamed scalars.
#'   - (Possibly named) vectors if equal length
#' @param .name  The label as it will appear in the results in `emmeans`.
#' @param .adjust Gives the default adjustment method for multiplicity (used in
#'   `emmeans`).
#'
#' @return
#' Depending on input, either a vector or a data frame of scaled weights.
#'
#' @examples
#' data(mtcars)
#'
#' mod <- lm(mpg ~ factor(cyl) * am, mtcars)
#'
#'
#' my_contrasts <- data.frame("squares" = c(-1, 2, -1),
#'                            "4 vs 6" = c(-30, 30, 0),
#'                            check.names = FALSE)
#'
#' (my_contrasts2 <- cw(my_contrasts))
#' my_contrasts3 <- cw(my_contrasts, .adjust = "fdr")
#'
#' @examplesIf require("emmeans")
#' library(emmeans)
#' (emms <- emmeans(mod, ~ cyl + am))
#'
#' contrast(emms, method = my_contrasts, by = "am")
#' contrast(emms, method = my_contrasts2, by = "am") # estimate is affected!
#' contrast(emms, method = my_contrasts3, by = "am") # p value is affected
#'
#' # Also in interaction contrasts
#' contrast(emms, interaction = list(cyl = my_contrasts2, am = "pairwise"))
#'
#' @export
contrast_weights <- function(..., .name = "custom", .adjust = NULL) {
  df <- data.frame(..., check.names = FALSE)
  # browser()
  if (nrow(df) == 1L) {
    x <- c(...)
    stopifnot(
      "Weights do not define a proper contrast!" =
        isTRUE(all.equal(sum(x), 0)) && any(x>0)
    )

    nf <- sum(x[x>0])
    return(x / nf)
  }

  df[] <- lapply(df, function(.c) do.call(contrast_weights, as.list(.c)))
  attr(df, "desc") <- .name
  attr(df, "adjust") <- .adjust
  df
}

#' @export
#' @rdname contrast_weights
cw <- contrast_weights
