# Bind matrices diagonally

Bind matrices diagonally

## Usage

``` r
dbind(..., .fill = NULL)
```

## Arguments

- ...:

  Matrices.

- .fill:

  Value to fill the off-"diagonal" values. If `NULL`, the value is the
  default value of the inputs' mode.

## Examples

``` r

M1 <- matrix(1:8, 2, 4)
M2 <- matrix(9:14, 2, 3)
dbind(M1, M2)
#>   1 2 3 4  5  6  7
#> 1 1 3 5 7  0  0  0
#> 2 2 4 6 8  0  0  0
#> 3 0 0 0 0  9 11 13
#> 4 0 0 0 0 10 12 14
dbind(M1, M2, .fill = NA)
#>    1  2  3  4  5  6  7
#> 1  1  3  5  7 NA NA NA
#> 2  2  4  6  8 NA NA NA
#> 3 NA NA NA NA  9 11 13
#> 4 NA NA NA NA 10 12 14


M1 <- matrix(letters[1:4], 2, 2)
M2 <- matrix(LETTERS[5:10], 3, 2)
dbind(M1, M2)
#>   1   2   3   4  
#> 1 "a" "c" ""  "" 
#> 2 "b" "d" ""  "" 
#> 3 ""  ""  "E" "H"
#> 4 ""  ""  "F" "I"
#> 5 ""  ""  "G" "J"
dbind(M1, M2, .fill = "Banana")
#>   1        2        3        4       
#> 1 "a"      "c"      "Banana" "Banana"
#> 2 "b"      "d"      "Banana" "Banana"
#> 3 "Banana" "Banana" "E"      "H"     
#> 4 "Banana" "Banana" "F"      "I"     
#> 5 "Banana" "Banana" "G"      "J"     


M1 <- matrix(TRUE, 2, 3)
M2 <- matrix(NA, 3, 4)
dbind(M1, M2)
#>       1     2     3     4     5     6     7
#> 1  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
#> 2  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
#> 3 FALSE FALSE FALSE    NA    NA    NA    NA
#> 4 FALSE FALSE FALSE    NA    NA    NA    NA
#> 5 FALSE FALSE FALSE    NA    NA    NA    NA
```
