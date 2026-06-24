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
#> 1    MR3 0.3277087
#> 2    MR2 0.3644751
#> 3    MR1 0.6165841
#> 4    MR4 0.3654222
HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

CFA <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)

fa_reliability(CFA)
#> Argument return.df= is deprecated, replaced by simplify= argument.
#>    Factor     Omega
#> 1  visual 0.6120052
#> 2 textual 0.8850608
#> 3   speed 0.6858417
```
