#' Refit afex models to produce Bayes Factors
#'
#' @param fit An object of class afex_aov.
#' @param method Should BFs be computed with the BayesFactor package, or by comparing BICs.
#' @param inclusion Return inclusions BFs (for matched models) instead of the actual models' BFs.
#' @param ... passed to [`BayesFactor::generalTestBF()`]
#'
#' @example examples/examples.afex_to_BF.R
#'
#' @export
afex_to_BF <-
  function(fit,
           method = c("JZS", "BIC"),
           inclusion = TRUE,
           ...) {
    .check_namespace("BayesFactor", "bayestestR")

    method <- match.arg(method)

    data <- fit$data$long
    between <- attr(fit, "between")
    covariate <- names(between)[sapply(between, is.null)]
    between <- names(between)[!sapply(between, is.null)]
    within <- names(attr(fit, "within"))
    id <- attr(fit, "id")
    dv <- attr(fit, "dv")

    if (length(names(attr(fit, "within"))) > 0) {
      .check_namespace("lme4")
    }
    # prep data
    data[c(id, between, within)] <-
      lapply(data[c(id, between, within)], as.factor)
    data[c(dv, covariate)] <-
      lapply(data[c(dv, covariate)], as.numeric)

    # prep formula
    w <- b <- NULL

    if (length(within) > 0) {
      w <- paste0("(", paste0(within, collapse = "*"), ")")
    }

    if (length(between) > 0) {
      b <- paste0(between, collapse = "*")
    }

    if (length(covariate) > 0) {
      b <- paste0(c(b, covariate), collapse = "+")
    }

    if (!is.null(b)) {
      b <- paste0("(", b, ")")
    }

    IV <- paste0(c(w, b), collapse = "*")

    if (method == "JZS") {
      if (length(within) > 0) {
        f <- as.formula(paste0(dv, "~", paste0(c(IV, id), collapse = "+")))
        BF <- BayesFactor::generalTestBF(
          f,
          data = data,
          whichRandom = id,
          neverExclude = id,
          progress = TRUE,
          ...
        )
        BF <- BF[-length(BF)] / BF[length(BF)]
      } else {
        f <- as.formula(paste0(dv, "~", IV))
        BF <- BayesFactor::generalTestBF(f, data = data,
                            progress = TRUE, ...)
      }
      BF <- bayestestR::bayesfactor_models(BF)
    } else if (method == "BIC") {
      f <- as.formula(paste0(dv, "~", IV))
      fs <- BayesFactor::enumerateGeneralModels(f, whichModels = "withmain")

      if (length(within) > 0) {
        fs <- lapply(fs, function(x) {
          x <- as.character(x)[-c(1:2)]
          as.formula(paste0(dv, "~", x, "+(1|", id, ")"))
        })

        mods <- lapply(fs, function(x)
          lmer(x, data))
        h0 <- as.formula(paste0(dv, "~(1|", id, ")"))
        mods <- c(list(lmer(h0, data)), mods)
        BF <- do.call(bayestestR::bayesfactor_models, args = mods)
      } else {
        mods <- lapply(fs, function(x)
          lm(x, data))
        h0 <- as.formula(paste0(dv, "~1"))
        mods <- c(list(lm(h0, data)), mods)
        BF <- do.call(bayestestR::bayesfactor_models, args = mods)
      }
    }

    if (inclusion) {
      BF <- bayestestR::bayesfactor_inclusion(BF, match_models = TRUE)
    }

    return(BF)
  }
