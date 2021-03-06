#' Method for importing `lavaan` output to `tbl_graph`
#'
#' Method for importing `lavaan` output to `tbl_graph`
#'
#' @author Mattan S. Ben-Shachar
#'
#' @param object a lavaan object, returned from `lavaan`(`sem()`/`cfa()`/`lavaan()`).
#' @param standardize should the edge coeffciants be standerdized.
#' @param include_var should the variances be included?
#' @param ... args passed to [`lavaan::standardizedSolution()`] or [`lavaan::parameterEstimates()`]
#'
#' @return a [`tidygraph::tbl_graph()`] object that can further be processed with `tidygraph`, and plotted with `ggraph`.
#'
#' @example examples/examples.as_tbl_graph.lavaan.R
#'
#' @export
as_tbl_graph.lavaan <- function(object, standardize = TRUE, include_var = FALSE, ...) {
  .check_namespace("tidygraph", "lavaan")

  op_type <- c(
    "=~"  = "latent",
    "~"   = "regression",
    "~~"  = "covariance",
    "~1"  = "intercept",
    "|"   = "threshold",
    "~*~" = "scale",
    "<~"  = "latent (formative)"
  )

  rel_type <- c(
    "=~" = "regression",
    "~"  = "regression",
    "<~" = "regression",
    "~~" = "covariance"
  )


  if (standardize) {
    params <- lavaan::standardizedSolution(object, ...)
  } else {
    params <- lavaan::parameterEstimates(object, ...)
  }


  params <- cbind(params[c(3, 1, 2)],
                  params[-c(3, 1, 2)])

  params$relation_full <- op_type[params$op]
  params$relation_type <- rel_type[params$op]

  params$rhs2 <- params$rhs
  params$rhs <- ifelse(params$relation_full == "latent", params$lhs,  params$rhs)
  params$lhs <- ifelse(params$relation_full == "latent", params$rhs2, params$lhs)
  params$rhs2 <- NULL

  params <- params[!params$op %in% c("|", "~1", "~*~"), ]
  if (!include_var) params <- params[params$rhs != params$lhs, ]

  latent_nodes <- unique(params$rhs[params$relation_full == "latent"])

  tidygraph::mutate(
    tidygraph::activate(tidygraph::as_tbl_graph(params),
                        nodes),
    latent = .data$name %in% latent_nodes
  )
}
