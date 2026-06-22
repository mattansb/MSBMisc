# Compute Simple Effects Omnibus Tests

This is a wrapper for
[`emmeans::joint_tests()`](https://rvlenth.github.io/emmeans/reference/joint_tests.html)
that provides an easy way to specify which simple effects we wish to
test, and within what variable(s).

## Usage

``` r
simple_effects(model, effect, inside, ...)
```

## Arguments

- model:

  The model.

- effect:

  The name of the required simple effect. e.g., `"A"` for a simple
  effect of *A*, or `"A:B"` for a simple *A-by-B* interaction.

- inside:

  A vector of the name(s) of the variable(s) within whose levels the
  `effect` will be tested. Can also be the name of an interaction (e.g.,
  `"B:C"`). If not specified, will use all the terms not in `effect`.

- ...:

  Passed to
  [`emmeans::joint_tests()`](https://rvlenth.github.io/emmeans/reference/joint_tests.html),
  e.g., `cov.reduce`, `at`, etc.

## Examples

``` r
library(afex)
#> Registered S3 method overwritten by 'car':
#>   method           from
#>   na.action.merMod lme4
#> ************
#> Welcome to afex. For support visit: http://afex.singmann.science/
#> - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
#> - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
#> - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
#> - Get and set global package options with: afex_options()
#> - Set sum-to-zero contrasts globally: set_sum_contrasts()
#> - For example analyses see: browseVignettes("afex")
#> ************
#> 
#> Attaching package: ‘afex’
#> The following object is masked from ‘package:lme4’:
#> 
#>     lmer

data(obk.long, package = "afex")
A <- aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long
)
#> Contrasts set to contr.sum for the following variables: treatment, gender

simple_effects(A, effect = "treatment")
#> phase = fup, hour = X1:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   2.510  0.1308
#>  M        2  10   5.561  0.0238
#> 
#> phase = post, hour = X1:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   1.627  0.2446
#>  M        2  10   6.363  0.0165
#> 
#> phase = pre, hour = X1:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.458  0.6454
#>  M        2  10   1.737  0.2251
#> 
#> phase = fup, hour = X2:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   9.720  0.0045
#>  M        2  10  13.600  0.0014
#> 
#> phase = post, hour = X2:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.770  0.4884
#>  M        2  10   4.717  0.0361
#> 
#> phase = pre, hour = X2:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.166  0.8495
#>  M        2  10   1.278  0.3205
#> 
#> phase = fup, hour = X3:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   2.051  0.1793
#>  M        2  10   6.105  0.0185
#> 
#> phase = post, hour = X3:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.417  0.6702
#>  M        2  10   2.824  0.1066
#> 
#> phase = pre, hour = X3:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   1.087  0.3741
#>  M        2  10   0.883  0.4433
#> 
#> phase = fup, hour = X4:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   5.994  0.0195
#>  M        2  10   5.000  0.0312
#> 
#> phase = post, hour = X4:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   1.234  0.3319
#>  M        2  10   5.524  0.0242
#> 
#> phase = pre, hour = X4:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.000  1.0000
#>  M        2  10   0.831  0.4635
#> 
#> phase = fup, hour = X5:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   1.401  0.2908
#>  M        2  10   2.534  0.1287
#> 
#> phase = post, hour = X5:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   1.761  0.2213
#>  M        2  10   5.282  0.0272
#> 
#> phase = pre, hour = X5:
#>  gender df1 df2 F.ratio p.value
#>  F        2  10   0.185  0.8339
#>  M        2  10   2.332  0.1475
#> 
#> Omnibus test for simple-simple-simple effect of treatment.
#>  

simple_effects(A, effect = "treatment:phase")
#> hour = X1:
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   3.403  0.0529
#>  M        4  10   1.034  0.4363
#> 
#> hour = X2:
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   3.346  0.0552
#>  M        4  10   2.443  0.1149
#> 
#> hour = X3:
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   2.687  0.0934
#>  M        4  10   1.408  0.3001
#> 
#> hour = X4:
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   3.155  0.0640
#>  M        4  10   0.758  0.5753
#> 
#> hour = X5:
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   0.686  0.6178
#>  M        4  10   2.044  0.1638
#> 
#> Omnibus test for simple-simple effect of treatment:phase.
#>  

simple_effects(A, effect = "phase", inside = "treatment")
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 7.04614e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 3.46062e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 5.69666e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 3.98462e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 1.2466e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 1.8101e-18
#>  treatment df1 df2 F.ratio p.value
#>  control     2  10   0.142  0.8693
#>  A           2  10   9.119  0.0056
#>  B           2  10  28.272 <0.0001
#> 
#> Omnibus test for simple effect of phase.
#>  

simple_effects(A, effect = "phase", inside = c("treatment", "gender"))
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 9.10386e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.66233e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.28876e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 4.55274e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 8.21709e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 1.48888e-17
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.97493e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 9.39923e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 7.67699e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 3.12618e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 4.50659e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.75745e-18
#> gender = F:
#>  treatment df1 df2 F.ratio p.value
#>  control     2  10   0.416  0.6708
#>  A           2  10   4.079  0.0507
#>  B           2  10  18.728  0.0004
#> 
#> gender = M:
#>  treatment df1 df2 F.ratio p.value
#>  control     2  10   1.823  0.2114
#>  A           2  10   5.248  0.0276
#>  B           2  10  11.015  0.0030
#> 
#> Omnibus test for simple-simple effect of phase.
#>  
# simple_effects(A, effect = "phase", inside = "treatment:gender") # same

simple_effects(A,
  effect = "phase", inside = c("treatment", "gender"),
  at = list(gender = "F")
)
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 9.10386e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.66233e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.28876e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.97493e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 9.39923e-18
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 7.67699e-19
#> gender = F:
#>  treatment df1 df2 F.ratio p.value
#>  control     2  10   0.416  0.6708
#>  A           2  10   4.079  0.0507
#>  B           2  10  18.728  0.0004
#> 
#> Omnibus test for simple-simple effect of phase.
#>  

simple_effects(A, effect = "phase:treatment", inside = "gender")
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 5.34581e-20
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 2.70112e-20
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 3.84303e-19
#> Error in solve.default(zcov, z) : 
#>   system is computationally singular: reciprocal condition number = 1.02243e-19
#>  gender df1 df2 F.ratio p.value
#>  F        4  10   4.498  0.0245
#>  M        4  10   1.930  0.1819
#> 
#> Omnibus test for simple effect of phase:treatment.
#>  
```
