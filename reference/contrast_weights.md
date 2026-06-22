# Build Contrast Weights

To be used ideally with
[emmeans::contrast](https://rvlenth.github.io/emmeans/reference/contrast.html).
Each contrasts is tested (sum to 0?) and scaled so that all positive
weights sum to 1 (and all negative weights to -1).

## Usage

``` r
contrast_weights(..., .name = "custom", .adjust = NULL)

cw(..., .name = "custom", .adjust = NULL)
```

## Arguments

- ...:

  Can be:

  - Unnamed scalars.

  - (Possibly named) vectors if equal length

- .name:

  The label as it will appear in the results in `emmeans`.

- .adjust:

  Gives the default adjustment method for multiplicity (used in
  `emmeans`).

## Value

Depending on input, either a vector or a data frame of scaled weights.

## Examples

``` r
data(mtcars)

mod <- lm(mpg ~ factor(cyl) * am, mtcars)


my_contrasts <- data.frame(
  "squares" = c(-1, 2, -1),
  "4 vs 6" = c(-30, 30, 0),
  check.names = FALSE
)

(my_contrasts2 <- cw(my_contrasts))
#>   squares 4 vs 6
#> 1    -0.5     -1
#> 2     1.0      1
#> 3    -0.5      0
my_contrasts3 <- cw(my_contrasts, .adjust = "fdr")

library(emmeans)
(emms <- emmeans(mod, ~ cyl + am))
#>  cyl am emmean    SE df lower.CL upper.CL
#>    4  0   22.9 1.750 26     19.3     26.5
#>    6  0   19.1 1.520 26     16.0     22.2
#>    8  0   15.1 0.875 26     13.3     16.8
#>    4  1   28.1 1.070 26     25.9     30.3
#>    6  1   20.6 1.750 26     17.0     24.2
#>    8  1   15.4 2.140 26     11.0     19.8
#> 
#> Confidence level used: 0.95 

contrast(emms, method = my_contrasts, by = "am")
#> am = 0:
#>  contrast estimate    SE df t.ratio p.value
#>  squares      0.30  3.61 26   0.083  0.9344
#>  4 vs 6    -113.25 69.50 26  -1.630  0.1152
#> 
#> am = 1:
#>  contrast estimate    SE df t.ratio p.value
#>  squares     -2.34  4.24 26  -0.552  0.5858
#>  4 vs 6    -225.25 61.60 26  -3.658  0.0011
#> 
contrast(emms, method = my_contrasts2, by = "am") # estimate is affected!
#> am = 0:
#>  contrast estimate   SE df t.ratio p.value
#>  squares      0.15 1.80 26   0.083  0.9344
#>  4 vs 6      -3.77 2.32 26  -1.630  0.1152
#> 
#> am = 1:
#>  contrast estimate   SE df t.ratio p.value
#>  squares     -1.17 2.12 26  -0.552  0.5858
#>  4 vs 6      -7.51 2.05 26  -3.658  0.0011
#> 
contrast(emms, method = my_contrasts3, by = "am") # p value is affected
#> am = 0:
#>  contrast estimate   SE df t.ratio p.value
#>  squares      0.15 1.80 26   0.083  0.9344
#>  4 vs 6      -3.77 2.32 26  -1.630  0.2303
#> 
#> am = 1:
#>  contrast estimate   SE df t.ratio p.value
#>  squares     -1.17 2.12 26  -0.552  0.5858
#>  4 vs 6      -7.51 2.05 26  -3.658  0.0023
#> 
#> P value adjustment: fdr method for 2 tests 

# Also in interaction contrasts
contrast(emms, interaction = list(cyl = my_contrasts2, am = "pairwise"))
#>  cyl_custom am_pairwise estimate   SE df t.ratio p.value
#>  squares    0 - 1           1.32 2.79 26   0.474  0.6393
#>  4 vs 6     0 - 1           3.73 3.09 26   1.206  0.2386
#> 
```
