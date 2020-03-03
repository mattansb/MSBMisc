\dontrun{
  library(lavaan)
  library(semPlot)

  mediation_model <- "
    Sepal.Length ~ b * Sepal.Width + c * Petal.Length
     Sepal.Width ~ a * Petal.Length
  "
  fit <- sem(mediation_model, data = iris)

  semPaths(fit)


  # With user layout
  m <- node_layout_maker(fit, snap_to_grid = TRUE)

  semPaths(fit, layout = m)
}
