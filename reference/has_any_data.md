# Test rowwise if each row is missing / has all data in select columns

Test rowwise if each row is missing / has all data in select columns

## Usage

``` r
has_any_data(.data, ..., .name)

has_all_data(.data, ..., .name)

missing_any_data(.data, ..., .name)

missing_all_data(.data, ..., .name)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

- .name:

  Name of the new column with the logical index.

## Examples

``` r
data(mtcars)

mtcars[1, 1] <- NA
mtcars[2, ] <- NA

mtcars[1:3, 1:3] |>
  has_any_data(mpg:disp, .name = "has_any") |>
  has_all_data(mpg:disp, .name = "has_all") |>
  missing_any_data(mpg:disp, .name = "missing_any") |>
  missing_all_data(mpg:disp, .name = "missing_all")
#>                mpg cyl disp has_any has_all missing_any missing_all
#> Mazda RX4       NA   6  160    TRUE   FALSE        TRUE       FALSE
#> Mazda RX4 Wag   NA  NA   NA   FALSE   FALSE        TRUE        TRUE
#> Datsun 710    22.8   4  108    TRUE    TRUE       FALSE       FALSE
```
