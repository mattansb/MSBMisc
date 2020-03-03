\dontrun{
  # Give it a try
  library(afex)
  data(obk.long, package = "afex")
  fit <- aov_ez(
    "id",
    "value",
    obk.long,
    between = c("treatment", "gender"),
    within = c("phase", "hour"),
    covariate = "age",
    factorize = FALSE
  )
  fit
  afex_to_BF(fit)
}
