# Some stats demos

Some stats demos

## Usage

``` r
stat_demo_apps(
  demo = c("paired ttest", "truncated correlation", "berksons paradox")
)
```

## Arguments

- demo:

  Which demo to show? (Partial matching supported)

## Details

### paired ttest

Shows a paired vs un-paired ttest, and how these differences are
affected by the correlation.

### truncated correlation

Demo of how truncation affects correlations and doesn't affect MSE.

### berksons paradox

Demo of how How sampling bias can affect estimated correlations and
effects. See related [Numberphile
video](https://www.youtube.com/watch?v=FUD8h9JpEVQ).
