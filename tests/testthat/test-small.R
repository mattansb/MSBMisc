library(testthat)
library(MSBMisc)

test_that("age_in_unit", {
  expect_equal(age_in_unit("1989-08-05", "2021-11-16"), "32 years, 3 months, 1 weeks, 4 days")
})


test_that("seq", {
  expect_equal(seq_range(1:4, length.out = 3), seq(1, 4, length.out = 3))

  expect_equal(mean_sd(-1:1), c(`-SD` = -1, Mean = 0, `+SD` = 1))
})
