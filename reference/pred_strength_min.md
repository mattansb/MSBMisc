# Estimate the Prediction Strength of a Clustering Model

This function estimates the stability of cluster assignments by
calculating the prediction strength: how well do cluster assignments of
new data from a pre-trained model co-incide with cluster assignments of
the same data from a new model fitted on the same data (Tibshirani &
Walther, 2005).

## Usage

``` r
pred_strength_min(object, ...)

# S3 method for class 'cluster_spec'
pred_strength_min(object, ...)

# S3 method for class 'cluster_fit'
pred_strength_min(object, new_data = NULL, ...)

# S3 method for class 'workflow'
pred_strength_min(object, new_data = NULL, ...)
```

## Arguments

- object:

  A fitted tidyclust model

- ...:

  Not currently used.

- new_data:

  A dataset to predict on.

## Value

A tibble with 3 columns; `.metric`, `.estimator`, and `.estimate`.

## Details

Note that this metric will re-fit the model on the new dataset
(`new_data`), and so might be computationally expensive.

## References

Tibshirani, R. and Walther, G. (2005) Cluster Validation by Prediction
Strength, *Journal of Computational and Graphical Statistics, 14*(3),
511-528.

## See also

Other cluster metric: [`jaccard_avg()`](jaccard_avg.md)

Other ML4Psych: [`jaccard_avg()`](jaccard_avg.md),
[`metric_by_event()`](metric_by_event.md)

## Examples

``` r

library(tidymodels)
library(tidyclust)

penguins <- drop_na(penguins)

kmeans_spec <- k_means(num_clusters = tune())

res <- tune_cluster(
  kmeans_spec,
  ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  resamples = vfold_cv(penguins, v = 2, repeats = 10),
  metrics = cluster_metric_set(pred_strength_min)
)

# What's the largest k that has a good prediction strength?
# (values above 0.8 are considered good)
autoplot(res)
```
