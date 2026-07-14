#' Deprecated functions
#'
#' @param ... Arguments (unused)
#'
#' @export
crop_coord_polar <- function(...) {
  .Deprecated(new = "ggplot2::coord_radial", old = "crop_coord_polar")
}

#' @export
#' @rdname crop_coord_polar
pred_strength <- function(...) {
  .Deprecated(new = "pred_strength_min", old = "pred_strength")
}
