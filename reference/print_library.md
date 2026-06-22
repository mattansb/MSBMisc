# Print Loading/Attaching of Packages

Useful in RMarkdown.

## Usage

``` r
print_library(..., .character.only = FALSE, .version = TRUE, .load = TRUE)

print_require(..., .character.only = FALSE, .version = TRUE, .load = TRUE)

print_library_md(..., .character.only = FALSE, .version = TRUE, .load = TRUE)
```

## Arguments

- ...:

  Names of packages.

- .character.only:

  Is `...` from characters?

- .version:

  Print library version?

- .load:

  Load package, or just print?

## Examples

``` r
print_library(afex, tidyverse, emmeans, MASS,
  .load = FALSE
)
#> library(afex)      # 1.5.1
#> library(tidyverse) # 
#> library(emmeans)   # 2.0.3
#> library(MASS)      # 7.3.65 
```
