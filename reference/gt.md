# Compare multiple vectors

Compare multiple vectors

## Usage

``` r
gt(...)

lt(...)

eq(...)

neq(...)

leq(...)

geq(...)

x %>>% y

x %<<% y

x %==% y

x %!=% y

x %<=% y

x %>=% y
```

## Arguments

- x, y, ...:

  Vectors, typically numerical, to be compared.

## Value

A logical vector. For the operator, a `last_y` attribute stores the last
RHS values from the comparisons (strip away with
[`as.vector()`](https://rdrr.io/r/base/vector.html)). See examples.

## Examples

``` r
x <- c(1, 3, 1, 1, 2)
y <- c(2, 2, 1, 1, 1)
z <- c(3, 1, 1, 2, 1)


lt(x, y, z) # >
#> [1]  TRUE FALSE FALSE FALSE FALSE
eq(x, y, z) # ==
#> [1] FALSE FALSE  TRUE FALSE FALSE
neq(x, y, z) # !=
#> [1]  TRUE  TRUE FALSE FALSE FALSE
leq(x, y, z) # <=
#> [1]  TRUE FALSE  TRUE  TRUE FALSE
geq(x, y, z) # >=
#> [1] FALSE  TRUE  TRUE FALSE  TRUE

gt(x, y, z) # <
#> [1] FALSE  TRUE FALSE FALSE FALSE

# same as
x %>>% y %>>% z
#> [1] FALSE  TRUE FALSE FALSE FALSE
#> attr(,"last_y ")
#> [1] 3 1 1 2 1

# same as
x > y & y > z
#> [1] FALSE  TRUE FALSE FALSE FALSE

# Operators can be mixed!

x %>>% y %==% z
#> [1] FALSE FALSE FALSE FALSE  TRUE
#> attr(,"last_y ")
#> [1] 3 1 1 2 1

# Or broken
(l1 <- x %>>% y)
#> [1] FALSE  TRUE FALSE FALSE  TRUE
#> attr(,"last_y ")
#> [1] 2 2 1 1 1

(l2 <- l1 %==% z)
#> [1] FALSE FALSE FALSE FALSE  TRUE
#> attr(,"last_y ")
#> [1] 3 1 1 2 1

# same as
x > y & y == z
#> [1] FALSE FALSE FALSE FALSE  TRUE

as.vector(l2)
#> [1] FALSE FALSE FALSE FALSE  TRUE
```
