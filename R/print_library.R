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
    if (.character.only) {
      pkgs <- c(...)
    } else {
      pkgs <- sapply(match.call(expand.dots = FALSE)$`...`, deparse)
    }

    if (.load) {
      suppressMessages(suppressWarnings(suppressPackageStartupMessages(
        .check_namespace(pkgs)
      )))
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

    cat(paste0("library(", pkgs, ")", vs, collapse = "\n"), "\n")
  }

#' @export
#' @rdname print_library
print_require <- print_library
