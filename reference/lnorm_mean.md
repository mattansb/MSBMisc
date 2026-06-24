# Functions to convert parameters of a log-normal distribution to meaningfull values on the response scale.

Functions to convert parameters of a log-normal distribution to
meaningfull values on the response scale.

## Usage

``` r
lnorm_mean(meanlog, sdlog, ...)

lnorm_median(meanlog, ...)

lnorm_var(meanlog, sdlog, ...)

lnorm_sd(meanlog, sdlog, ...)
```

## Arguments

- meanlog, sdlog:

  mean and standard deviation of the distribution on the log scale with
  default values of `0` and `1` respectively.

- ...:

  Not used

## Examples

``` r
x <- rlnorm(1e4, meanlog = 1.5, sdlog = 1.2)

m <- lm(log(x) ~ 1)

meanlog <- coef(m)
sdlog <- sigma(m)

lnorm_mean(meanlog, sdlog)
#> [1] 8.965027
mean(x)
#> [1] 9.049275

lnorm_median(meanlog, sdlog)
#> [1] 4.405787
median(x)
#> [1] 4.3756

lnorm_sd(meanlog, sdlog)
#> [1] 15.88742
sd(x)
#> [1] 18.52951
```
