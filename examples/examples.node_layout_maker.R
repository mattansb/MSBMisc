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

\dontrun{
  library(tidygraph)
  library(ggraph)

  rstat_nodes <- data.frame(person = c("Hadley", "David", "Romain", "Julia"))
  rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
                            to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
  graph_data <- tbl_graph(nodes = rstat_nodes, edges = rstat_edges)

  m_layout <- node_layout_maker(graph_data)

  ggraph(graph_data, layout = m_layout) +
    geom_node_label(aes(label = person))
}
