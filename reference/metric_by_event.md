# Compute class-wise metrics for a metric set in a multiclass setting

This function computes class-wise metrics for a metric set in a
multiclass setting by binarizing the outcome variable for each class and
applying the metric set to each binary version of the data.

## Usage

``` r
metric_by_event(data, metric_set, truth, estimate, ..., .all = FALSE)
```

## Arguments

- data:

  A data frame containing the columns specified in `truth`, `estimate`,
  and `...`.

- metric_set:

  A metric set function (e.g.,
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html))
  that includes class-wise metrics.

- truth:

  The column identifier for the true class results (that is a `factor`).
  This should be an unquoted column name although this argument is
  passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names). For `_vec()` functions, a `factor`
  vector.

- estimate:

  The column identifier for the predicted class results (that is also
  `factor`). As with `truth` this can be specified different ways but
  the primary method is to use an unquoted variable name. For `_vec()`
  functions, a `factor` vector.

- ...:

  Not currently used.

- .all:

  A logical value indicating whether to include all metrics from the
  metric set (including those that do not use macro/micro estimators).

## Value

A tibble with the same columns to be expected from the metric set, plus
the following additional columns: `.class` indicating the class for
which the metric was computed. For these rows, `.estimator` is set to
`"event"` to indicate that these are event-level metrics.

## See also

Other ML4Psych: [`jaccard_avg()`](jaccard_avg.md),
[`pred_strength()`](pred_strength.md)
