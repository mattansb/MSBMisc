# pasteAsComment ----------------------------------------------------------

#' Copy / Paste as Comment / Roxygen
#'
#' Adpated from [stla/pasteAsComment](https://github.com/stla/pasteAsComment)
#'
#' @param prefix The prefix to use for the copied text.
#'
#' @export
Paste <- function(prefix = "#> ") {
  .check_namespace("rstudioapi")
  if (!rstudioapi::hasFun("insertText")) {
    stop("Your RStudio version is too old.", call. = FALSE)
  }
  textToInsert <- .prefix_from_clipboard(prefix)
  rstudioapi::insertText(textToInsert)
}

#' @export
#' @rdname Paste
PasteRoxygen <- function() {
  Paste("#' ")
}

#' @export
#' @rdname Paste
Copy <- function(prefix = "#> ") {
  .check_namespace("clipr")
  textToInsert <- .prefix_from_clipboard(prefix)
  clipr::write_clip(textToInsert)
}

#' @export
#' @rdname Paste
CopyRoxygen <- function() {
  Copy("#' ")
}

# bracketify --------------------------------------------------------------
# from

#' bracketify
#'
#' Adpated from [stla/bracketify](https://github.com/stla/bracketify)
#'
#' @export
bracketifyFile <- function() {
  .check_namespace("rstudioapi")
  if (rstudioapi::hasFun("getSourceEditorContext")) {
    editorContext <- rstudioapi::getSourceEditorContext()
  } else {
    stop("This version of RStudio is too old!")
  }
  contents <- paste0(editorContext[["contents"]], collapse = "\n")
  newContents <- gsub(
    "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)*)\\2",
    # "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)* *)\\2",
    '\\1[["\\3"]]',
    contents,
    perl = TRUE
  )
  rstudioapi::setDocumentContents(newContents, editorContext[["id"]])
}

#' @rdname bracketifyFile
#' @export
bracketifySelection <- function() {
  .check_namespace("rstudioapi")
  if (rstudioapi::hasFun("getSourceEditorContext")) {
    editorContext <- rstudioapi::getSourceEditorContext()
  } else {
    stop("This version of RStudio is too old!")
  }
  selection <- editorContext[["selection"]][[1L]]
  text <- selection[["text"]]
  if (text == "") {
    message("Nothing selected.")
    return(invisible(NULL))
  }
  newText <- gsub(
    "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)*)\\2",
    # "(\\w*)\\$(`?)( *\\w+(?:[._\\- ]+\\w+)* *)\\2",
    '\\1[["\\3"]]',
    text,
    perl = TRUE
  )
  rstudioapi::modifyRange(selection[["range"]], newText, editorContext[["id"]])
}



# Utils -------------------------------------------------------------------


.prefix_from_clipboard <- function(prefix = "#> ") {
  .check_namespace("clipr")
  lines <- clipr::read_clip()
  lines <- paste0(prefix, lines)
  textToInsert <- paste0(lines, collapse = "\n")
}
