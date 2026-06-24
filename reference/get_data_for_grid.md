# Get raw data for plotting with model predictions

Get raw data for plotting with model predictions

## Usage

``` r
get_data_for_grid(grid, model, residualize = FALSE, collapse_by = FALSE, ...)

# S3 method for class 'data.frame'
get_data_for_grid(
  grid,
  model,
  residualize = FALSE,
  collapse_by = FALSE,
  pred_name,
  ...
)

# S3 method for class 'ggeffects'
get_data_for_grid(
  grid,
  model,
  residualize = FALSE,
  collapse_by = FALSE,
  protect_names = TRUE,
  ...
)

# S3 method for class 'emmGrid'
get_data_for_grid(
  grid,
  model,
  residualize = FALSE,
  collapse_by = FALSE,
  protect_names = TRUE,
  ...
)

# S3 method for class 'predictions'
get_data_for_grid(grid, model, residualize = FALSE, collapse_by = FALSE, ...)
```

## Arguments

- grid:

  A data grid with predictions

- model:

  The statistical model

- residualize:

  Should data be residualized?

- collapse_by:

  Name of grouping variable to collaple across. If `TRUE` name of
  grouping variable is automatically detected from the model.

- ...:

  Args passed from / to other functions.

- pred_name:

  Name of column that has the predictions in the data grid

- protect_names:

  Logical, if `TRUE`, preserves column names from the `ggeffects`
  object.

## Examples

``` r

data("mtcars")
mtcars <- mtcars |> transform(cyl = factor(cyl))
mod <- lm(mpg ~ hp + cyl, data = mtcars[1:10, ])

nd <- expand.grid(
  hp = seq(50, 350, by = 50),
  cyl = "4"
)

nd$predicted_mpg <- predict(mod, newdata = nd)

get_data_for_grid(nd, mod)
#> Warning: 'get_data_for_grid()' is experimental.
#>                    mpg  hp cyl
#> Mazda RX4         21.0 110   6
#> Mazda RX4 Wag     21.0 110   6
#> Datsun 710        22.8  93   4
#> Hornet 4 Drive    21.4 110   6
#> Hornet Sportabout 18.7 175   8
#> Valiant           18.1 105   6
#> Duster 360        14.3 245   8
#> Merc 240D         24.4  62   4
#> Merc 230          22.8  95   4
#> Merc 280          19.2 123   6

get_data_for_grid(nd, mod, residualize = TRUE, pred_name = "predicted_mpg")
#>                   cyl  hp predicted_mpg
#> Mazda RX4           6 100      23.14580
#> Mazda RX4 Wag       6 100      23.14580
#> Datsun 710          4 100      22.39857
#> Hornet 4 Drive      6 100      23.54580
#> Hornet Sportabout   8 150      19.70311
#> Valiant             6 100      19.95907
#> Duster 360          8 250      13.58271
#> Merc 240D           4  50      25.08816
#> Merc 230            4 100      22.51327
#> Merc 280            6 100      22.09131

library(ggplot2)
#> 
#> Attaching package: ‘ggplot2’
#> The following objects are masked from ‘package:psych’:
#> 
#>     %+%, alpha
ggplot(nd, aes(hp, predicted_mpg)) +
  geom_line() +
  geom_point(aes(y = mpg, color = "Raw"),
    data = get_data_for_grid(nd, mod)
  ) +
  geom_point(aes(color = "Residualized"),
    data = get_data_for_grid(nd, mod, residualize = TRUE, pred_name = "predicted_mpg")
  ) +
  labs(
    title = "Partial residual plot",
    color = "Data"
  )


## Support of data-grid packages ------
# - ggeffects
# - emmeans
# - marginaleffects
pred_ggeffects <- ggeffects::ggpredict(mod, c("hp [50:350, by = 50]", "cyl [4]"))
get_data_for_grid(pred_ggeffects, residualize = TRUE)
#>                   group   x predicted
#> Mazda RX4             6 100  23.14580
#> Mazda RX4 Wag         6 100  23.14580
#> Datsun 710            4 100  22.39857
#> Hornet 4 Drive        6 100  23.54580
#> Hornet Sportabout     8 150  19.70311
#> Valiant               6 100  19.95907
#> Duster 360            8 250  13.58271
#> Merc 240D             4  50  25.08816
#> Merc 230              4 100  22.51327
#> Merc 280              6 100  22.09131
at <- list(hp = seq(50, 350, by = 50), cyl = "4")
pred_emmeans <- emmeans::emmeans(mod, ~ hp + cyl, at = at)
get_data_for_grid(pred_emmeans, mod, residualize = TRUE)
#>                   cyl  hp   emmean
#> Mazda RX4           6 100 23.14580
#> Mazda RX4 Wag       6 100 23.14580
#> Datsun 710          4 100 22.39857
#> Hornet 4 Drive      6 100 23.54580
#> Hornet Sportabout   8 150 19.70311
#> Valiant             6 100 19.95907
#> Duster 360          8 250 13.58271
#> Merc 240D           4  50 25.08816
#> Merc 230            4 100 22.51327
#> Merc 280            6 100 22.09131
# pred_marginaleffects <- marginaleffects::predictions(mod, newdata = nd)
# get_data_for_grid(pred_marginaleffects, residualize = TRUE)
## Collapes across group ------
data("cake", package = "lme4")
fm1 <- lme4::lmer(angle ~ temperature + (1 | recipe),
  data = cake
)

nd <- marginaleffects::datagrid(
  temperature = unique(cake$temperature),
  model = fm1
)
suppressWarnings(pred_marginaleffects <- marginaleffects::predictions(fm1, newdata = nd))

get_data_for_grid(pred_marginaleffects, collapse_by = TRUE)
#>    temperature recipe estimate
#> 1          175      A 29.13333
#> 2          185      A 31.53333
#> 3          195      A 30.80000
#> 4          205      A 33.53333
#> 5          215      A 38.66667
#> 6          225      A 35.06667
#> 7          175      B 26.86667
#> 8          185      B 29.40000
#> 9          195      B 31.73333
#> 10         205      B 32.13333
#> 11         215      B 34.46667
#> 12         225      B 35.26667
#> 13         175      C 27.93333
#> 14         185      C 28.93333
#> 15         195      C 31.73333
#> 16         205      C 30.86667
#> 17         215      C 34.40000
#> 18         225      C 35.73333
# get_data_for_grid(pred_marginaleffects, collapse_by = TRUE, residualize = TRUE)
```
