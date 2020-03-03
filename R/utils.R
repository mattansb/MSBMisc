#' @keywords internal
.check_namespace <- function(...) {
  pkgs <- c(...)

  status <- sapply(pkgs, requireNamespace, quietly = TRUE)

  if (any(!status)) {
    bad_pkgs <- pkgs[!status]

    stop(
      "The following packages are not installed and are required for this function:\n",
      paste("\t-", bad_pkgs, collapse = "\n"),
      call. = FALSE
    )
  } else {
    invisible(NULL)
  }
}
