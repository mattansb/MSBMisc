# Compare correlations

This function is a wrapper around
[psych::r.test](https://rdrr.io/pkg/psych/man/r.test.html).

## Usage

``` r
compare_cor(data, r1, r2, data2 = NULL, by = NULL, ci = 0.95)
```

## Arguments

- data:

  A data frame

- r1, r2:

  Names of variable for the first and second correlations

- data2:

  Where to look for the `r2` columns (if not provided, looked for on
  `data`).

- by:

  Name of column to split `data` by. The column must have only 2 unique
  values. If provided, the correlation between `r1` is compared between
  the two groups.

- ci:

  Confidence level for correlations.

## Value

A list of two data frames:

1.  The two correlations + their CIs

2.  The test results

## Details

- If `data2` is provided, the correlation between `r1` (in `data`) and
  `r2` (in `data2`) is compared.

  - If `r2` not provided, the correlation between `r1` (in `data`) and
    `r1` (in `data2`) is compared

- If `by` is provided `r1` (in `data`) is compared between the 2 groups.

- Else, a test for the difference of two dependent correlations is
  conducted.

## Examples

``` r
# Test dependent correlations ------------------
## different variables
compare_cor(mtcars, r1 = c("mpg", "hp"), r2 = c("drat", "am"))
#> $Correlations
#>       Variables          r   CI     CI_low    CI_high
#> r1  mpg with hp -0.7761684 0.95 -0.8852686 -0.5860994
#> r2 drat with am  0.7127111 0.95  0.4843991  0.8501319
#> 
#> $Test
#>                                                    Test         z           p
#> 1 Test of difference between two dependent correlations -6.133834 8.57858e-10
#> 

## 1 shared variable
compare_cor(mtcars, r1 = c("mpg", "hp"), r2 = c("mpg", "am"))
#> $Correlations
#>      Variables          r   CI     CI_low    CI_high
#> r1 mpg with hp -0.7761684 0.95 -0.8852686 -0.5860994
#> r2 mpg with am -0.2432043 0.95 -0.5456270  0.1152646
#> 
#> $Test
#>                                                      Test         t
#> 1 Test of difference between two correlated  correlations -5.563815
#>              p
#> 1 5.301858e-06
#> 



# Test independent correlations -----------------
## Different data sets
compare_cor(
  data = mtcars, r1 = c("mpg", "hp"),
  data2 = iris, r2 = c("Sepal.Length", "Sepal.Width")
)
#> $Correlations
#>                                Variables          r   CI     CI_low     CI_high
#> r1                   mpg with hp (data1) -0.7761684 0.95 -0.8852686 -0.58609941
#> r2 Sepal.Length with Sepal.Width (data2) -0.1175698 0.95 -0.2726932  0.04351158
#> 
#> $Test
#>                                                      Test        z            p
#> 1 Test of difference between two independent correlations 4.515732 6.309853e-06
#> 

## Groups
compare_cor(mtcars, r1 = c("mpg", "hp"), by = "am")
#> $Correlations
#>             Variables          r   CI     CI_low    CI_high
#> r1 mpg with hp (am=0) -0.8315065 0.95 -0.9332484 -0.6062733
#> r2 mpg with hp (am=1) -0.8006683 0.95 -0.9378951 -0.4467852
#> 
#> $Test
#>                                                      Test         z        p
#> 1 Test of difference between two independent correlations 0.2295313 0.818456
#> 
```
