# Information criteria for log-normal models

Log-likelihood (and by extension *AIC* and *BIC*) for log-normal models
fit with `stats::lm(log(y) ~ ...)` are computed with
`stats::dnorm(log(y), ...)` instead of with `stats::dlnorm(y, ...)`,
which makes comparing different families difficult. This function is
aimed at rectifying this. See examples.

## Usage

``` r
logLik_lnorm(object, REML = FALSE)

AIC_lnorm(object, k = 2, REML = FALSE)

BIC_lnorm(object, REML = FALSE)
```

## Arguments

- object:

  A fitted model object. The model must meet all of the following (will
  throw an error if not met):

  1.  A Gaussian likelihood with an identity link function.

  2.  The LHS of the model's formula must use the
      [`log()`](https://rdrr.io/r/base/Log.html) function.

  3.  No weights (not yet supported).

- REML:

  Only `FALSE` supported.

- k:

  numeric, the *penalty* per parameter to be used; the default `k = 2`
  is the classical AIC.

## Note

`REML` is not (yet) supported. Make sure you are comparing correct
LL/AIC/BIC values.

## Examples

``` r

data("mtcars")
mtcars$mpg <- floor(mtcars$mpg)

model_lnorm <- lm(log(mpg) ~ factor(cyl), mtcars)
model_norm <- lm(mpg ~ factor(cyl), mtcars)
model_pois <- glm(mpg ~ factor(cyl), mtcars, family = poisson())

# No, that first one is wrong...
(aics <- AIC(model_lnorm, model_norm, model_pois))
#>             df       AIC
#> model_lnorm  4 -19.67061
#> model_norm   4 170.87527
#> model_pois   3 173.59104

aics[1, "AIC"] <- AIC_lnorm(model_lnorm)
aics # better!
#>             df      AIC
#> model_lnorm  4 168.3652
#> model_norm   4 170.8753
#> model_pois   3 173.5910

# Should support any model really... =====================
model_lnorm <- lme4::lmer(log(mpg) ~ factor(cyl) + (1 | gear), mtcars, REML = FALSE)
#> boundary (singular) fit: see help('isSingular')
model_norm <- lme4::lmer(mpg ~ factor(cyl) + (1 | gear), mtcars, REML = FALSE)
#> boundary (singular) fit: see help('isSingular')
model_pois <- lme4::glmer(mpg ~ factor(cyl) + (1 | gear), mtcars, family = poisson())
#> boundary (singular) fit: see help('isSingular')

# No, that first one is wrong...
(aics <- AIC(model_lnorm, model_norm, model_pois))
#>             df       AIC
#> model_lnorm  5 -17.67061
#> model_norm   5 172.87527
#> model_pois   4 175.59104

aics[1, "AIC"] <- AIC_lnorm(model_lnorm)
aics # better!
#>             df      AIC
#> model_lnorm  5 170.2152
#> model_norm   5 172.8753
#> model_pois   4 175.5910
```
