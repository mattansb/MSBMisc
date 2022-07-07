if (FALSE) {
  library(testthat)
  library(MSBMisc)
}

test_that("print_library", {
  expect_match(print_library(MSBMisc), "0.0.1.13", fixed = TRUE)
  expect_match(print_library(MSBMisc), "library", fixed = TRUE)
  expect_match(print_require(MSBMisc), "require", fixed = TRUE)
})

test_that("age_in_unit", {
  skip_if_not_installed("lubridate")
  expect_equal(age_in_unit("1989-08-05", "2021-11-16"), "32 years, 3 months, 1 weeks, 4 days")
})


test_that("seq", {
  expect_equal(seq_range(1:4, length.out = 3, padding = 0),
               seq(1, 4, length.out = 3))

  expect_equal(mean_sd(-1:1), c("-SD" = -1, "Mean" = 0, "+SD" = 1))
})


test_that("cw", {
  expect_error(cw(-2, 1))
  expect_equal(cw(-2, 2, 0), c(-1, 1, 0))
})

test_that("chisq FU", {
  skip_if_not_installed("effectsize")

  M <- as.table(rbind(c(762, 327, 468),
                      c(484, 239, 477)))
  dimnames(M) <- list(
    gender = c("F", "M"),
    party = c("Democrat", "Independent", "Republican")
  )

  res <- chisq.test(M)

  o1 <- chisq_pairwise(res)
  o2 <- chisq_pairwise(res, population_in_row = FALSE)
  o3 <- chisq_residual(res)

  expect_equal(nrow(o1), 1L)
  expect_equal(nrow(o2), 3L)
  expect_equal(nrow(o3), 6L)
  expect_equal(ncol(o1), 8L)
  expect_equal(ncol(o2), 8L)
  expect_equal(ncol(o3), 9L)

  expect_equal(o1$Chi.sq, 30.07015, tolerance = 0.0001)
  expect_equal(o1$df, 2L)
  expect_equal(o2$Chi.sq, c(1.7178, 29.0595, 9.3356), tolerance = 0.0001)
  expect_equal(o2$df, c(1,1,1))
  expect_equal(o3$z.value, c(2.1989, -2.5047, 0.4114, -0.4686, -2.8432, 3.2387), tolerance = 0.0001)
  expect_equal(o3$n.obs, c(762, 484, 327, 239, 468, 477))
})


test_that("r_SB", {
  expect_equal(r_SB(0.57), 0.7261146, tolerance = 0.0001)
})

test_that("r_SB", {
  skip_if_not_installed("dplyr")
  data(mtcars)

  mtcars[1, 1] <- NA
  mtcars[2, ] <- NA

  O <- mtcars[1:3, 1:3] |>
    has_any_data(mpg:disp, .name = "has_any") |>
    has_all_data(mpg:disp, .name = "has_all") |>
    missing_any_data(mpg:disp, .name = "missing_any") |>
    missing_all_data(mpg:disp, .name = "missing_all")

  E <- data.frame(has_any = c(TRUE, FALSE, TRUE),
                  has_all = c(FALSE, FALSE, TRUE),
                  missing_any = c(TRUE, TRUE, FALSE),
                  missing_all = c(FALSE, TRUE, FALSE))

  expect_equal(O[,4:7], E, ignore_attr = TRUE)
})


test_that("vlookup", {
  df <- data.frame(a = letters[c(1,1:9)],
                   b = 51:60)

  expect_warning(r <- vlookup(c("a", "e", "c"), df, key = "a", value = "b"))
  expect_equal(r, c(a = 51L, e = 56L, c = 54L))

  expect_warning(r <- vlookup(c("a", "e", "c"), df, key = "a", value = "b", add = TRUE))
  expect_s3_class(r, "data.frame")
})


test_that("simple_effects", {
  skip_if_not_installed("afex")
  skip_if_not_installed("emmeans")
  skip_if_not_installed("insight")
  skip_if_not_installed("stringr")


  obk.long <- afex::obk.long
  A <- afex::aov_car(value ~ treatment * gender + Error(id/(phase*hour)),
                     data = obk.long)

  se1 <- simple_effects(A, effect = "phase", inside = c("treatment", "gender"))
  expect_s3_class(se1, "summary_emm")
  expect_equal(nrow(se1), 6L)
  expect_equal(ncol(se1), 6L)

  se2 <- simple_effects(A, effect = "phase:treatment", inside = "gender")
  expect_s3_class(se2, "summary_emm")
  expect_equal(nrow(se2), 2L)
  expect_equal(ncol(se2), 5L)
})


test_that("ll lnorm", {
  skip_if_not_installed("insight")

  data("mtcars")
  mtcars$mpg <- floor(mtcars$mpg)

  m0 <- lm(log(mpg) ~ 1, mtcars)
  m1 <- lm(log(mpg) ~ factor(cyl), mtcars)
  m2 <- lm(log(mpg) ~ factor(cyl) * am, mtcars)

  expect_equal(sapply(list(m0, m1, m2), AIC_lnorm), c(205.379, 168.365, 170.345), tolerance = 0.001)
  expect_equal(sapply(list(m0, m1, m2), BIC_lnorm), c(208.310, 174.228, 180.605), tolerance = 0.001)


  mb <- lm(mpg ~ factor(cyl) * am, mtcars)
  expect_error(logLik_lnorm(mb), "not log-normal")
  expect_error(AIC_lnorm(mb), "not log-normal")
  expect_error(BIC_lnorm(mb), "not log-normal")
})


test_that("delta method", {
  skip_if_not_installed("emmeans")

  m <- glm(am ~ factor(cyl),
           family = binomial(), data = mtcars)

  em <- emmeans::emmeans(m, ~ cyl) |>
    emmeans::regrid(transform = "none")

  names(em@bhat) <- paste0("p", c(4, 6, 8))

  d <- delta_method(
    1/(1+exp(-p4)), 1/(1+exp(-p6)), 1/(1+exp(-p8)),
    .means = em@bhat, .V = em@V
  )

  em <- emmeans::regrid(em)

  expect_equal(d$means, em@bhat, ignore_attr = TRUE)
  expect_equal(d$V, em@V, ignore_attr = TRUE)
})
