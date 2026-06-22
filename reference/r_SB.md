# Spearman-Brown Split half reliability

Spearman-Brown Split half reliability

## Usage

``` r
r_SB(x, y = NULL, var.equal = TRUE)
```

## Arguments

- x:

  A correlation or numeric vector

- y:

  A numeric vector

- var.equal:

  Assume equal var of `x` and `y`? (ignored if `y` is not `NULL`)

## Examples

``` r
r_SB(1:30, -exp(1 / 1:30), var.equal = TRUE)
#> [1] 0.7207338

r_SB(1:30, -exp(1 / 1:30), var.equal = FALSE)
#> [1] 0.07863447

r_SB(0.57)
#> [1] 0.7261146
```
