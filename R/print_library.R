#' Print Loading/Attaching of Packages
#'
#' Useful in RMarkdown.
#'
#' @param ... Names of packages.
#' @param .character.only Is \code{...} from characters?
#' @param .version Print library version?
#' @param .load Load package, or just print?
#'
#' @example examples/examples.print_library.R
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
      pkgs <- sapply(match.call(expand.dots = F)$`...`, deparse)
    }

    if (.load) {
      res <-
        suppressMessages(suppressWarnings(suppressPackageStartupMessages(
          sapply(
            pkgs,
            require,
            character.only = TRUE,
            quietly = TRUE
          )
        )))

      if (!all(res)) {
        stop("Could not load ", paste0(pkgs[!res], collapse = ", "), call. = FALSE)
      }
    }

    vs <- ""
    if (.version) {
      vs <- lapply(pkgs, function(x){
        tryCatch(packageVersion(x),
                 error = function(e) "")
      })
      vs <- sapply(vs, as.character)

      sps <- max(nchar(pkgs)) - nchar(pkgs)
      sps <-
        sapply(sps, function(x)
          paste0(rep(" ", x), collapse = ""))

      vs <- paste0(sps, " # ", vs)
    }

    cat(paste0("library(", pkgs, ")", vs, collapse = "\n"), "\n")
  }

#' @export
#' @rdname print_library
print_require <- print_library
