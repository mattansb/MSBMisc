#' @keywords internal
.check_namespace <- function(..., quietly = FALSE) {
  pkgs <- c(...)

  status <- sapply(pkgs, requireNamespace, quietly = TRUE)

  if (res <- any(!status)) {
    bad_pkgs <- pkgs[!status]

    if (!quietly) {
      stop(
        "The following packages are not installed and are required for this function:\n",
        paste("\t-", bad_pkgs, collapse = "\n"),
        call. = FALSE
      )
    } else {
      warning(
        "The following packages are not installed and are required for some stuff:\n",
        paste("\t-", bad_pkgs, collapse = "\n"),
        call. = FALSE
      )
    }
  }
  invisible(!res)
}
