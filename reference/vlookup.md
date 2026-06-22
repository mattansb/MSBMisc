# vlookup

vlookup

## Usage

``` r
vlookup(this, data, key, value, add = FALSE)
```

## Arguments

- this:

  A vector of values

- data:

  A data frame to search in

- key:

  Where should `this` be looked up?

- value:

  Name of the column from which values should be returned.

- add:

  Should `this` and the resulting values be returned as a data frame?
  (Else a vector)

## Examples

``` r
df <- data.frame(
  a = letters[c(1, 1:9)],
  b = 51:60
)

vlookup(c("a", "e", "c"), df, key = "a", value = "b")
#> Warning: Found more than 1 match. Returning the first.
#>  a  e  c 
#> 51 56 54 
vlookup(c("a", "e", "c"), df, key = "a", value = "b", add = TRUE)
#> Warning: Found more than 1 match. Returning the first.
#>   a  b
#> a a 51
#> e e 56
#> c c 54
```
