\dontrun{
  library(afex)

  data(obk.long, package = "afex")

  fit <- aov_ez(
    "id",
    "value",
    obk.long,
    within = c("phase", "hour"),
    covariate = "age",
    factorize = FALSE
  )
  fit
  afex_to_BF(fit)
}
