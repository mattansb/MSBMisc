# Compare the frequencies of levels of a factor

Using [`stats::mcnemar.test`](https://rdrr.io/r/stats/mcnemar.test.html)
for comparing dependent proportions.  
  
This is function is dubious. Best not to use it.

## Usage

``` r
compare_freqs(f, adjust = stats::p.adjust.methods, correct = TRUE)
```

## Arguments

- f:

  A factor vector.

- adjust:

  Method for correcting p-values. See
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- correct:

  a logical indicating whether to apply continuity correction when
  computing the test statistic.

## Examples

``` r
f <- c(
  rep("A", 12),
  rep("B", 45),
  rep("C", 42),
  rep("D", 20)
)

compare_freqs(f)
#> Warning: This is function is dubious. Best not to use it.
#>   comparison     prop1     prop2      Chi.sq df        p.raw       p.holm
#> 1  A  vs.  B 0.1008403 0.3781513 17.96491228  1 2.250146e-05 0.0001350088
#> 2  A  vs.  C 0.1008403 0.3529412 15.57407407  1 7.933499e-05 0.0003966749
#> 3  A  vs.  D 0.1008403 0.1680672  1.53125000  1 2.159249e-01 0.4318498779
#> 4  B  vs.  C 0.3781513 0.3529412  0.04597701  1 8.302176e-01 0.8302175918
#> 5  B  vs.  D 0.3781513 0.1680672  8.86153846  1 2.912420e-03 0.0116496803
#> 6  C  vs.  D 0.3529412 0.1680672  7.11290323  1 7.653106e-03 0.0229593194
```
