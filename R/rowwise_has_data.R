#' Test rowwise if each row is missing / has all data in select columns
#'
#' @inheritParams dplyr::select
#' @param .name Name of the new column with the logical index.
#'
#' @examples
#' if (require("dplyr")) {
#'   data(mtcars)
#'
#'   mtcars[1, 1] <- NA
#'   mtcars[2, ] <- NA
#'
#'   mtcars[1:3, 1:3] |>
#'     has_any_data(mpg:disp, .name = "has_any") |>
#'     has_all_data(mpg:disp, .name = "has_all") |>
#'     missing_any_data(mpg:disp, .name = "missing_any") |>
#'     missing_all_data(mpg:disp, .name = "missing_all")
#' }
#'
#' @export
has_any_data <- function(.data, ..., .name) {
  .check_namespace("dplyr")
  .data[[.name]] <- apply(!is.na(dplyr::select(.data, ...)), 1, any)
  .data
}

#' @export
#' @rdname has_any_data
has_all_data <- function(.data, ..., .name) {
  .check_namespace("dplyr")
  .data[[.name]] <- apply(dplyr::select(.data, ...), 1, Negate(anyNA))
  .data
}

#' @export
#' @rdname has_any_data
missing_any_data <- function(.data, ..., .name) {
  .data <- has_all_data(.data, ..., .name = .name)
  .data[[.name]] <- !.data[[.name]]
  .data
}

#' @export
#' @rdname has_any_data
missing_all_data <- function(.data, ..., .name) {
  .data1 <- has_all_data(.data, ..., .name = .name)
  .data2 <- has_any_data(.data, ..., .name = .name)
  .data1[[.name]] <- !.data1[[.name]] & !.data2[[.name]]
  .data1
}
