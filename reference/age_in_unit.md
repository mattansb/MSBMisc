# Age in some units

Age in some units

## Usage

``` r
age_in_unit(
  DOB,
  REFDATE = Sys.Date(),
  years = TRUE,
  months = TRUE,
  weeks = TRUE,
  days = TRUE
)
```

## Arguments

- DOB, REFDATE:

  Two dates

- years, months, weeks, days:

  the units.

## Examples

``` r
DOB <- as.Date("1989-08-05")
TODAY <- Sys.Date()
age_in_unit(DOB, TODAY)
#> [1] "36 years, 10 months, 2 weeks, 4 days"
```
