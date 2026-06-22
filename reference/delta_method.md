# Transform means a (co)variances using the *delta* method

Transform means a (co)variances using the *delta* method

## Usage

``` r
delta_method(..., .means, .V, return = c("means", "cov", "stddev", "cor"))
```

## Arguments

- ...:

  Unquoted transformations. See example.

- .means:

  A named vector of means.

- .V:

  A covariance matrix.

- return:

  What should be returned?

## Value

A list with one of (optionally named):

- `means` of the transformed variables.

- `cov` (co) variance matrix of the the transformed variables.

- `stddev` standard deviations of the transformed variables
  (`sqrt(diag(cov))`).

- `cor` correlation matrix of the transformed variables.
  (`cov2cor(cov)`)

## Note

Most of this function is mostly copied from `msm::deltamethod()`.

## Examples

``` r

M <- sapply(mtcars, mean)
V <- cov(mtcars)

delta_method(
  (mpg^2) / hp,
  log_am = log1p(am),
  .means = M, .V = V,
  return = "cor"
)
#> $cor
#>            (mpg^2)/hp    log_am
#> (mpg^2)/hp  1.0000000 0.4703339
#> log_am      0.4703339 1.0000000
#> 

# Sobel Test ----

data("mtcars")
mod.y <- lm(mpg ~ hp + cyl, mtcars[1:5, ])
mod.m <- lm(hp ~ cyl, mtcars[1:5, ])

bhat <- c(coef(mod.y), coef(mod.m))[c(2, 5)]
Vhat <- dbind(vcov(mod.y), vcov(mod.m))[c(2, 5), c(2, 5)]

res <- delta_method(
  hp * cyl,
  .means = bhat, .V = Vhat,
  return = c("means", "stddev")
)

res$means / res$stddev
#>  hp * cyl 
#> -1.641799 

# Compare:
(bhat[1] * bhat[2]) /
  sqrt(bhat[1]^2 * Vhat[2, 2] + bhat[2]^2 * Vhat[1, 1])
#>        hp 
#> -1.641799 

# Special character will give you a bad time...
m <- lm(mpg ~ factor(cyl), mtcars[1:5, ])

bhat <- coef(m)
names(bhat) <- c("cyl4", "cyl6", "cyl8")
V <- vcov(m)

delta_method(cyl4, cyl4 + cyl6, cyl4 + cyl8,
  .means = bhat,
  .V = V
)
#> $means
#>        cyl4 cyl4 + cyl6 cyl4 + cyl8 
#>    22.80000    21.13333    18.70000 
#> 
#> $cov
#>                   cyl4  cyl4 + cyl6  cyl4 + cyl8
#> cyl4        0.05333333 0.000000e+00 0.000000e+00
#> cyl4 + cyl6 0.00000000 1.777778e-02 6.938894e-18
#> cyl4 + cyl8 0.00000000 6.938894e-18 5.333333e-02
#> 
#> $stddev
#>        cyl4 cyl4 + cyl6 cyl4 + cyl8 
#>   0.2309401   0.1333333   0.2309401 
#> 
#> $cor
#>             cyl4  cyl4 + cyl6  cyl4 + cyl8
#> cyl4           1 0.000000e+00 0.000000e+00
#> cyl4 + cyl6    0 1.000000e+00 2.253472e-16
#> cyl4 + cyl8    0 2.253472e-16 1.000000e+00
#> 
```
