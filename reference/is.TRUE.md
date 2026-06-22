# It's just logical

It's just logical

## Usage

``` r
is.TRUE(x)

is.FALSE(x)

allTRUE(...)

anyTRUE(...)
```

## Arguments

- x, ...:

  Values to be tested.

## Examples

``` r
x <- list(1, TRUE, list(TRUE), FALSE, "Hello world!")

is.TRUE(x)
#> [1] FALSE  TRUE FALSE FALSE FALSE

is.FALSE(x)
#> [1] FALSE FALSE FALSE  TRUE FALSE

allTRUE(TRUE, FALSE, stop("NOOOO"))
#> [1] FALSE

anyTRUE(TRUE, FALSE, stop("NOOOO"))
#> [1] TRUE
```
