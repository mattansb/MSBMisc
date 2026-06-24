#' Compute class-wise metrics for a metric set in a multiclass setting
#'
#' This function computes class-wise metrics for a metric set in a multiclass
#' setting by binarizing the outcome variable for each class and applying the
#' metric set to each binary version of the data.
#'
#' @param data A data frame containing the columns specified in `truth`, `estimate`, and `...`.
#' @param metric_set A metric set function (e.g., `yardstick::metric_set()`) that includes class-wise metrics.
#' @param .all A logical value indicating whether to include all metrics from the metric set (including those that do not use macro/micro estimators).
#' @inheritParams yardstick::accuracy
#' @inheritParams yardstick::roc_auc
#'
#' @return A tibble with the same columns to be expected from the metric set,
#' plus the following additional columns: `.class` indicating the class for which
#' the metric was computed. For these rows, `.estimator` is set to `"event"` to
#' indicate that these are event-level metrics.
#'
#' @family ML4Psych
#'
#' @export
metric_by_event <- function(
  data,
  metric_set,
  truth,
  estimate,
  ...,
  .all = FALSE
) {
  # Only for class_prob_metric_set
  stopifnot(inherits(
    metric_set,
    c("class_prob_metric_set", "prob_metric", "class_metric")
  ))

  # Make a call with the metric set
  cl <- match.call(expand.dots = FALSE)
  cl[[1]] <- as.name(as.character(cl$metric_set))
  cl[["..."]] <- NULL
  cl[["metric_set"]] <- NULL
  cl[["event_level"]] <- "first"

  # Find names and levels
  truth_name <- as.character(cl$truth)
  levels <- levels(data[[truth_name]])

  estimate_name <- as.character(cl$estimate)
  prob_names <- as.character(substitute(list(...)))[-1]
  has_est <- length(estimate_name) > 0L
  has_prob <- all(nzchar(prob_names))

  # A function to binarize a multiclass factor
  fct_binarize <- function(.f, lvl) {
    forcats::fct_collapse(.f, Yes = lvl, other_level = "No")
  }

  # For each class...
  out <- levels |>
    purrr::imap(function(lvl, i) {
      tmp_data <- data
      tmp_cl <- cl

      # binarize
      tmp_data[[truth_name]] <- fct_binarize(tmp_data[[truth_name]], lvl)
      if (has_est) {
        tmp_data[[estimate_name]] <- fct_binarize(
          tmp_data[[estimate_name]],
          lvl
        )
      }
      if (has_prob) {
        tmp_cl[[length(tmp_cl) + 1]] <- as.name(prob_names[i])
      }
      tmp_cl$data <- tmp_data

      # Get the metrics
      eval.parent(tmp_cl)
    }) |>
    purrr::set_names(nm = levels) |>
    # rbind with an `.class` column for each class
    dplyr::bind_rows(.id = ".class") |>
    dplyr::mutate(
      # New estimator name?
      .estimator = "event"
    )

  if (isFALSE(.all)) {
    # Get current default behavior
    if (has_prob) {
      cl[length(cl) + seq_along(prob_names)] <- lapply(prob_names, as.name)
    }
    out_raw <- eval.parent(cl) |>
      dplyr::filter_out(
        .data$.estimator %in% c("macro", "macro_weighted", "micro", "hand_till")
      )

    # keep only metrics that use macro/micro multiclass estimators
    out <- out |>
      dplyr::anti_join(out_raw, by = ".metric") |>
      # Add the (non-classwise) metrics
      dplyr::bind_rows(out_raw)
  }

  # Deal with grouped data frames
  if (inherits(data, "grouped_df")) {
    out <- out |>
      dplyr::relocate(dplyr::all_of(dplyr::group_vars(data)), .before = 1) |>
      dplyr::arrange(dplyr::pick(dplyr::all_of(dplyr::group_vars(data))))
  }

  out
}


#' Estimate the Average Jaccard Index for Cluster Stability
#'
#' This function estimates the stability of cluster assignments by calculating
#' the average Jaccard index: how well do cluster assignments of new data from a
#' pre-trained model align with cluster assignments of the same data from a new
#' model fitted on the same data (Hennig, 2007).
#'
#' @param object A fitted tidyclust model
#' @param new_data A dataset to predict on.
#' @param ... Not currently used.
#'
#' @details Note that this metric will re-fit the model on the new dataset
#' (`new_data`), and so might be computationally expensive.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @references Hennig, C. (2007). Cluster-wise assessment of cluster stability.
#'   _Computational Statistics & Data Analysis, 52_(1), 258-271.
#'
#' @family cluster metric
#'
#' @seealso [fpc::clusterboot()]
#'
#' @examplesIf require("tidymodels") && require("tidyclust")
#'
#' library(tidymodels)
#' library(tidyclust)
#'
#' penguins <- drop_na(penguins)
#'
#' kmeans_spec <- k_means(num_clusters = tune())
#'
#' res <- tune_cluster(
#'   kmeans_spec,
#'   ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'   resamples = bootstraps(penguins, times = 10),
#'   metrics = cluster_metric_set(jaccard_avg)
#' )
#'
#' # What's the largest k that has high recovery rate?
#' # (values above 0.75 are considered good, values below 0.5 are considered bad)
#' autoplot(res)
#'
#' @family ML4Psych
#'
#' @export
jaccard_avg <- function(object, ...) {
  UseMethod("jaccard_avg")
}


#' @export
#' @rdname jaccard_avg
jaccard_avg.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname jaccard_avg
jaccard_avg.cluster_fit <- function(object, new_data = NULL, ...) {
  spec <- object$spec

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- generics::fit(
    spec,
    ~.,
    data = new_data[, attr(object$preproc$terms, "term.labels"), drop = FALSE]
  )

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- stats::predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- stats::predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  jaccard_avg_impl(pred_fit, pred_new_fit)
}

#' @export
#' @rdname jaccard_avg
jaccard_avg.workflow <- function(object, new_data = NULL, ...) {
  if (!workflows::is_trained_workflow(object)) {
    stop("The workflow must be fitted before calculating cluster metrics.")
  }

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- generics::fit(object, data = new_data)

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- stats::predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- stats::predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  jaccard_avg_impl(pred_fit, pred_new_fit)
}

# Core vector helper for Jaccard index calculation
jaccard_avg_impl <- function(pred_clusters, pred_clusters_new) {
  # 1. Create a contingency table (Rows = test clusters, Cols = train clusters)
  # tab[i, j] gives the size of the intersection between test cluster i and train cluster j
  tab <- as.matrix(table(pred_clusters, pred_clusters_new))

  # 2. Extract cluster sizes
  row_sums <- rowSums(tab) # Sizes of reference (test) clusters
  col_sums <- colSums(tab) # Sizes of comparison (train) clusters

  # 3. Compute the Union matrix: |A| + |B| - |A,B|
  # outer() creates a matrix of all combinations of (|A| + |B|)
  union_matrix <- outer(row_sums, col_sums, "+") - tab

  # 4. Compute the Jaccard matrix: Intersection / Union
  jaccard_avg_matrix <- tab / union_matrix

  # 5. For each reference (test) cluster, find its maximum Jaccard alignment
  cluster_stabilities <- apply(jaccard_avg_matrix, 1, max)

  tibble::tibble(
    .metric = "jaccard_avg",
    .estimator = "standard",
    .estimate = mean(cluster_stabilities)
  )
}

#' Estimate the Prediction Strength of a Clustering Model
#'
#' This function estimates the stability of cluster assignments by calculating
#' the prediction strength: how well do cluster assignments of new data from a
#' pre-trained model co-incide with cluster assignments of the same data from a
#' new model fitted on the same data (Tibshirani & Walther, 2005).
#'
#' @param object A fitted tidyclust model
#' @param new_data A dataset to predict on.
#' @param ... Not currently used.
#'
#' @details Note that this metric will re-fit the model on the new dataset
#' (`new_data`), and so might be computationally expensive.
#'
#' @return A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.
#'
#' @references Tibshirani, R. and Walther, G. (2005) Cluster Validation by
#'   Prediction Strength, _Journal of Computational and Graphical Statistics, 14_(3), 511-528.
#'
#' @family cluster metric
#'
#' @examplesIf require("tidymodels") && require("tidyclust")
#'
#' library(tidymodels)
#' library(tidyclust)
#'
#' penguins <- drop_na(penguins)
#'
#' kmeans_spec <- k_means(num_clusters = tune())
#'
#' res <- tune_cluster(
#'   kmeans_spec,
#'   ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'   resamples = vfold_cv(penguins, v = 2, repeats = 10),
#'   metrics = cluster_metric_set(pred_strength)
#' )
#'
#' # What's the largest k that has a good prediction strength?
#' # (values above 0.8 are considered good)
#' autoplot(res)
#'
#' @family ML4Psych
#'
#' @export
pred_strength <- function(object, ...) {
  UseMethod("pred_strength")
}


#' @export
#' @rdname pred_strength
pred_strength.cluster_spec <- function(object, ...) {
  cli::cli_abort(
    c(
      "This function requires a fitted model.",
      "i" = "Please use {.fn fit} on your cluster specification."
    )
  )
}

#' @export
#' @rdname pred_strength
pred_strength.cluster_fit <- function(object, new_data = NULL, ...) {
  spec <- object$spec

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- generics::fit(
    spec,
    ~.,
    data = new_data[, attr(object$preproc$terms, "term.labels"), drop = FALSE]
  )

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- stats::predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- stats::predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  pred_strength_impl(pred_fit, pred_new_fit)
}

#' @export
#' @rdname pred_strength
pred_strength.workflow <- function(object, new_data = NULL, ...) {
  if (!workflows::is_trained_workflow(object)) {
    stop("The workflow must be fitted before calculating cluster metrics.")
  }

  if (is.null(new_data)) {
    stop(
      "Prediction strength requires a validation dataset. ",
      "Please provide a data frame to `new_data` to evaluate out-of-sample stability."
    )
  }

  # 1. Fit a fresh reference model to the new data using the same spec
  new_fit <- generics::fit(object, data = new_data)

  # 2. Extract independent test labels from the new fit
  pred_new_fit <- stats::predict(new_fit, new_data)[[".pred_cluster"]]

  # 3. Predict test labels using the original training centroids
  pred_fit <- stats::predict(object, new_data)[[".pred_cluster"]]

  # 4. Compute stability metric
  pred_strength_impl(pred_fit, pred_new_fit)
}


# Core vector helper for prediction strength calculation
pred_strength_impl <- function(pred_clusters, pred_clusters_new) {
  split_preds <- split(pred_clusters, pred_clusters_new)
  ps_by_cluster <- purrr::map_dbl(split_preds, function(preds) {
    n <- length(preds)
    if (n <= 1) {
      return(1.0)
    }
    counts <- as.vector(table(as.character(preds)))
    matching_pairs <- sum(counts * (counts - 1))
    matching_pairs / (n * (n - 1))
  })

  # Return structured tibble standard for tidyclust metrics
  tibble::tibble(
    .metric = "pred_strength",
    .estimator = "standard",
    .estimate = min(ps_by_cluster)
  )
}

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("tidyclust", quietly = TRUE)) {
    utils::assignInMyNamespace(
      "jaccard_avg",
      tidyclust::new_cluster_metric(jaccard_avg, direction = "maximize")
    )
    utils::assignInMyNamespace(
      "pred_strength",
      tidyclust::new_cluster_metric(pred_strength, direction = "maximize")
    )
  }
}
