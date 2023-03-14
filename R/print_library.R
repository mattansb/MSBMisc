#' Print Loading/Attaching of Packages
#'
#' Useful in RMarkdown.
#'
#' @param ... Names of packages.
#' @param .character.only Is `...` from characters?
#' @param .version Print library version?
#' @param .load Load package, or just print?
#'
#' @examples
#' print_library(afex, tidyverse, emmeans, MASS,
#'               .load = FALSE)
#'
#' @export
#'
print_library <-
  function(...,
           .character.only = FALSE,
           .version = TRUE,
           .load = TRUE) {
    cl <- match.call()
    type <- ifelse(grepl("require$", deparse(cl[[1]])), "require", "library")

    if (.character.only) {
      pkgs <- c(...)
    } else {
      pkgs <- sapply(match.call(expand.dots = FALSE)$`...`, deparse)
    }

    if (.load) {
      suppressMessages(suppressWarnings(suppressPackageStartupMessages(sapply(pkgs, require, character.only = TRUE))))
    }

    vs <- ""
    if (.version) {
      vs <- pkgs |> lapply(\(x){
        tryCatch(utils::packageVersion(x),
                 error = function(e) "")
      }) |>
        sapply(as.character)

      vs <- (max(nchar(pkgs)) - nchar(pkgs)) |>
        sapply(\(x) paste0(rep(" ", x), collapse = "")) |>
        paste0(" # ", vs)
    }

    out <- paste0(type, "(", pkgs, ")", vs)
    class(out) <- c("msb_print_library", class(out))
    out
  }

#' @export
#' @rdname print_library
print_require <- print_library

#' @export
print.msb_print_library <- function(x, ...) {
  cat(paste0(x, collapse = "\n"), "\n")
}

#' @export
#' @rdname print_library
print_library_md <- function(...) {
  cl <- match.call()
  cl[[1]] <- quote(MSBMisc::print_library)
  x <- eval.parent(cl)
  cat("```r\n")
  print(x)
  cat("```\n")
  invisible(NULL)
}
formals(print_library_md) <- formals(print_library)
