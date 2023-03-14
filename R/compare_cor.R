#' Compare correlations
#'
#' This function is a wrapper around [psych::r.test].
#'
#' @param data A data frame
#' @param r1,r2 Names of variable for the first and second correlations
#' @param data2 Where to look for the `r2` columns (if not provided, looked for
#'   on `data`).
#' @param by Name of column to split `data` by. The column must have only 2
#'   unique values. If provided, the correlation between `r1` is compared
#'   between the two groups.
#' @param ci Confidence level for correlations.
#'
#' @details
#' - If `data2` is provided, the correlation between `r1` (in `data`) and `r2` (in `data2`) is compared.
#'     - If `r2` not provided, the correlation between `r1` (in `data`) and `r1` (in `data2`) is compared
#' - If `by` is provided `r1` (in `data`) is compared between the 2 groups.
#' - Else, a test for the difference of two dependent correlations is conducted.
#'
#' @return
#' A list of two data frames:
#' 1. The two correlations + their CIs
#' 2. The test results
#'
#'
#' @examples
#' # Test dependent correlations ------------------
#' ## different variables
#' compare_cor(mtcars, r1 = c("mpg", "hp"), r2 = c("drat", "am"))
#'
#' ## 1 shared variable
#' compare_cor(mtcars, r1 = c("mpg", "hp"), r2 = c("mpg", "am"))
#'
#'
#'
#' # Test independent correlations -----------------
#' ## Different data sets
#' compare_cor(
#'   data = mtcars, r1 = c("mpg", "hp"),
#'   data2 = iris, r2 = c("Sepal.Length", "Sepal.Width")
#' )
#'
#' ## Groups
#' compare_cor(mtcars, r1 = c("mpg", "hp"), by = "am")
#'
#' @export
compare_cor <- function(data, r1, r2, data2 = NULL, by = NULL,
                        ci = 0.95) {
  .check_namespace("psych", "correlation")

  if (!is.null(by)) {
    stopifnot(missing(r2), is.null(data2))
    splits <- split(data, data[[by]])
    stopifnot(length(splits) == 2L)
    data <- splits[[1]]
    data2 <- splits[[2]]
    r2 <- r1
  }

  if (missing(r2)) {
    stopifnot(!is.null(data2))
    r2 <- r1
  }

  stopifnot(
    length(r1) == 2L,
    length(r2) == 2L,
    is.character(r1),
    is.character(r2),
    all(r1 %in% colnames(data))
  )

  if (is.null(data2)) {
    # This is a dependent correlation
    data <- na.omit(data[unique(c(r1, r2))])
    n <- nrow(data)

    if (!any(r2 %in% r1)) {
      # Different variables
      r <- cor(data[c(r1, r2)])

      r12 <- r[1, 2]
      r34 <- cor(data[r2])[1, 2]

      # Other cors
      r12 <- cor(data[r1])[1, 2]
      r34 <- cor(data[r2])[1, 2]

      out <- psych::r.test(
        n = n,
        r12 = r12 <- r[1, 2],
        r34 = r34 <- r[3, 4],
        # Other cors
        r13 = r[1, 3],
        r14 = r[1, 4],
        r23 = r[2, 3],
        r24 = r[2, 4]
      )
      Corr1 <- data.frame(r = r12, CI = ci, correlation::cor_to_ci(r12, n, ci = ci))
      Corr2 <- data.frame(r = r34, CI = ci, correlation::cor_to_ci(r34, n, ci = ci))
    } else {
      # One variable in common
      v2 <- intersect(r1, r2)
      v1 <- setdiff(r1, v2)
      v3 <- setdiff(r2, v2)

      out <- psych::r.test(
        n = n,
        r12 = r12 <- cor(data[[v1]], data[[v2]]),
        r13 = r13 <- cor(data[[v1]], data[[v3]]),
        # Other cors
        r23 = cor(data[[v2]], data[[v3]])
      )
      Corr1 <- data.frame(r = r12, CI = ci, correlation::cor_to_ci(r12, n, ci = ci))
      Corr2 <- data.frame(r = r13, CI = ci, correlation::cor_to_ci(r13, n, ci = ci))
    }
  } else {
    stopifnot(all(r2 %in% colnames(data2)))
    # independent variables

    data <- na.omit(data[r1])
    n <- nrow(data)

    data2 <- na.omit(data2[r2])
    n2 <- nrow(data2)

    out <- psych::r.test(
      n = n, n2 = n2,
      r12 = r12 <- cor(data)[1, 2],
      r34 = r34 <- cor(data2)[1, 2]
    )
    Corr1 <- data.frame(r = r12, CI = ci, correlation::cor_to_ci(r12, n, ci = ci))
    Corr2 <- data.frame(r = r34, CI = ci, correlation::cor_to_ci(r34, n2, ci = ci))
  }


  res <- rbind(Corr1, Corr2)
  rownames(res) <- paste0("r", 1:2)
  vn <- c(paste0(r1, collapse = " with "), paste0(r2, collapse = " with "))
  if (!is.null(data2)) {
    if (!is.null(by)) {
      vn <- paste0(vn, " (", by, "=", names(splits), ")")
    } else {
      vn <- paste0(vn, " (", c("data1", "data2"), ")")
    }
  }
  res <- cbind(Variables = vn, res)
  list(Correlations = res, Test = as.data.frame(unclass(out)[-1]))
}
