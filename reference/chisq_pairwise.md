# Follow-up for contingency table test

Follow-up for contingency table test

## Usage

``` r
chisq_pairwise(
  Xsq,
  population_in_row = TRUE,
  adjust = stats::p.adjust.methods,
  effect_size = c("V", "phi"),
  ci = 0.95,
  ...
)

chisq_residual(
  Xsq,
  adjust = stats::p.adjust.methods,
  res_type = c("pearson", "standardized"),
  ci = 0.95
)
```

## Arguments

- Xsq:

  Result from [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)

- population_in_row:

  Comparisons by row? (If not, by column.)

- adjust:

  Method for correcting p-values. See
  [`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html).

- effect_size:

  Type of effect size to use.

- ci:

  Confidence Interval (CI) level

- ...:

  Passed to [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

- res_type:

  Type of residuals to use.

## Examples

``` r
M <- as.table(rbind(
  c(762, 327, 468),
  c(484, 239, 477)
))
dimnames(M) <- list(
  gender = c("F", "M"),
  party = c("Democrat", "Independent", "Republican")
)
M
#>       party
#> gender Democrat Independent Republican
#>      F      762         327        468
#>      M      484         239        477

res <- chisq.test(M)
chisq_pairwise(res)
#>   comparison   Chi.sq df       V   V.CI_low V.CI_high        p.raw       p.holm
#> 1    F vs. M 30.07015  2 0.10092 0.06570276         1 2.953589e-07 2.953589e-07
chisq_pairwise(res, population_in_row = FALSE)
#>                   comparison    Chi.sq df          V   V.CI_low V.CI_high
#> 1   Democrat vs. Independent  1.717892  1 0.02173696 0.00000000         1
#> 2    Democrat vs. Republican 29.059545  1 0.11413683 0.07810062         1
#> 3 Independent vs. Republican  9.335659  1 0.07574388 0.02750054         1
#>          p.raw       p.holm
#> 1 1.899646e-01 1.899646e-01
#> 2 7.018745e-08 2.105623e-07
#> 3 2.247373e-03 4.494746e-03

chisq_residual(res)
#>   gender       party    z.value n.obs           d     d.CI_low   d.CI_high
#> 1      F    Democrat  2.1988558   762  0.07965614  0.008654136  0.15065814
#> 2      M    Democrat -2.5046695   484 -0.11384861 -0.202937885 -0.02475934
#> 3      F Independent  0.4113702   327  0.02274882 -0.085637434  0.13113508
#> 4      M Independent -0.4685829   239 -0.03031011 -0.157089642  0.09646942
#> 5      F  Republican -2.8432397   468 -0.13142880 -0.222028170 -0.04082944
#> 6      M  Republican  3.2386734   477  0.14828867  0.058548084  0.23802926
#>         p.raw      p.holm
#> 1 0.027888180 0.083664539
#> 2 0.012256586 0.049026346
#> 3 0.680801124 1.000000000
#> 4 0.639367769 1.000000000
#> 5 0.004465747 0.022328734
#> 6 0.001200870 0.007205219
```
