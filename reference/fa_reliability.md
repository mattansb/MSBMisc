# Calculate reliability from E/CFA

Calculate reliability from E/CFA

## Usage

``` r
fa_reliability(x, ...)

# S3 method for class 'fa'
fa_reliability(x, keys = NULL, threshold = 0, labels = NULL, ...)

# S3 method for class 'lavaan'
fa_reliability(x, ...)
```

## Arguments

- x:

  E/CFA model (e.g., the result from
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)).

- ...:

  For `lavaan` objects, arguments passed to
  [`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html)

- keys:

  optional, see ?psych::make.keys

- threshold:

  which values from the loadings should be used? Only used if
  `keys = NULL`.

- labels:

  optional factor labels

## Author

[Brenton M. Wiernik](https://wiernik.org/)

## Examples

``` r
data("Harman74.cor")
EFA <- psych::fa(Harman74.cor$cov, 4)

fa_reliability(EFA)
#>   Factor     Omega
#> 1    MR1 0.3277063
#> 2    MR3 0.3644803
#> 3    MR4 0.6165837
#> 4    MR2 0.3654178
HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

CFA <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)

fa_reliability(CFA)
#> Error in data.frame(Factor, Omega = as.vector(Omega)): arguments imply differing number of rows: 3, 1
```
