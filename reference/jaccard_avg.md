# Estimate the Average Jaccard Index for Cluster Stability

This function estimates the stability of cluster assignments by
calculating the average Jaccard index: how well do cluster assignments
of new data from a pre-trained model align with cluster assignments of
the same data from a new model fitted on the same data (Hennig, 2007).

## Usage

``` r
jaccard_avg(object, ...)

# S3 method for class 'cluster_spec'
jaccard_avg(object, ...)

# S3 method for class 'cluster_fit'
jaccard_avg(object, new_data = NULL, ...)

# S3 method for class 'workflow'
jaccard_avg(object, new_data = NULL, ...)
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

Hennig, C. (2007). Cluster-wise assessment of cluster stability.
*Computational Statistics & Data Analysis, 52*(1), 258-271.

## See also

`fpc::clusterboot()`

Other cluster metric: [`pred_strength()`](pred_strength.md)

Other ML4Psych: [`metric_by_event()`](metric_by_event.md),
[`pred_strength()`](pred_strength.md)

## Examples

``` r

library(tidymodels)
library(tidyclust)

penguins <- drop_na(penguins)

kmeans_spec <- k_means(num_clusters = tune())

res <- tune_cluster(
  kmeans_spec,
  ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  resamples = bootstraps(penguins, times = 10),
  metrics = cluster_metric_set(jaccard_avg)
)

# What's the largest k that has high recovery rate?
# (values above 0.75 are considered good, values below 0.5 are considered bad)
autoplot(res)
```
