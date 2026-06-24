# Compute pseudo R-square for mixed models

This function computes the pseudo R-square for mixed models by
quantifying the reduction on the variance-covariance between a full
model and a reduced model.

## Usage

``` r
r2_pseudo(mf, mr, mnull = mr)

V_table(model)
```

## Arguments

- mf:

  The full model

- mr:

  The reduced model

- mnull:

  The empty model, where some variance of interest is unmodeled (an
  "unconditional" model)

- model:

  A fitted model object

## Value

A tibble with the following columns:

- `grp`: The name of the grouping factor

- `var`: The name of the variance component

- `r2`: The pseudo R-square for that variance component

## Details

Variance components are extracted from the 3 models, and where they
match (groups and terms must match *exactly*), we compute:

\$\$R^2 = \frac{V\_{full} - V\_{restricted}}{V\_{null}}\$\$

This gives the increase in variance explained by the full model compared
to the reduced model. When the restricted model is the empty model, this
is the same as:

\$\$R^2 = 1 - \frac{V\_{full}}{V\_{restricted}}\$\$

### Supported model classes

- `{stats}`: `lm`, `glm`

- `{lme4}`: `lmerMod` and `glmerMod`

- `{rstanarm}`: `stanreg`

- `{brms}`: `brmsfit`

- `{glmmTMB}`: `glmmTMB`

## Examples

``` r

mod0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
mod1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
mod2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)

r2_pseudo(mod1, mod0) # Residual variance explained by the fixed effect
#> # A tibble: 2 × 3
#>   grp      var              r2
#>   <chr>    <chr>         <dbl>
#> 1 Subject  (Intercept) -0.0781
#> 2 Residual NA           0.510 
r2_pseudo(mod2, mod1, mod0) # Residual variance explained by the random effects
#> # A tibble: 2 × 3
#>   grp      var            r2
#>   <chr>    <chr>       <dbl>
#> 1 Subject  (Intercept) 0.599
#> 2 Residual NA          0.156
r2_pseudo(mod2, mod0) # Residual variance explained by the mixed effects
#> # A tibble: 2 × 3
#>   grp      var            r2
#>   <chr>    <chr>       <dbl>
#> 1 Subject  (Intercept) 0.521
#> 2 Residual NA          0.666
```
