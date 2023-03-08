#' @export
save2read <- function(...) {
  x2y(substitute(...), from = "save", to = "read")
}
#' @export
save2load <- function(...) {
  x2y(substitute(...), from = "save", to = "load")
}
#' @export
write2read <- function(...) {
  x2y(substitute(...), from = "write", to = "read")
}
mod_str <- function(str, from, to){
  pttrn <- paste0("^#? ?(.*)", from, "([^\\(]*)\\(([^,]*), ?(.+)\\)$")
  repl <- paste0("\\3 <- \\1", to, "\\2(\\4)")
  if (to == "load" && from == "save") {
    if (!grepl(pttrn, str)) {
      pttrn <- sub("save", "Save", pttrn)
      repl <- sub("load", "Load", repl)
    }
  }
  gsub(pttrn, repl, str)
}
x2y <- function(..., from, to) {
  hasDots <- length(..1)
  if (hasDots) {
    if (length(..1) == 3) {
      x <- ..1
      if (length(x[[1]]) == 3) {
        x[[1]][[1]] <- as.character(x[[1]][[1]])
        x[[1]] <- paste(x[[1]][c(2,1,3)], collapse = "")
      }
      string <- paste0(x[[2]], " <- ", gsub(from, to, x[[1]]), "(\"", x[[3]], "\")")
    } else {
      string <- mod_str(..1, from = from, to = to)
    }
  }
  context <- rstudioapi::getActiveDocumentContext()
  isConsole <- context$id == "#console"
  if (isConsole) {
    r <- length(context$contents)
    rng <- rstudioapi::document_range(c(r, 1), c(r, Inf))
  }
  if (!(hasDots && isConsole)) {
    sel <- context$selection[[1]]
    # if no selected text find the line on which the save function is
    if (sel$text == "") {
      pttrn <- if (hasDots) paste0("^", from, "2", to, "| \\|> ?", from, "2", to) else
        paste0("^(# ?)?([^::]*:{2,3})?", from, ".*\\(.+\\)$")
      r <- 1:sel$range$start[[1]]
      r <- grep(pttrn, context$contents[r], ignore.case = TRUE)
      r <- r[length(r)]
      # if not on the call line :
      if (!length(r)) stop("didn't find the function")
      if (!hasDots) string <- mod_str(context$contents[[r]], from = from, to = to)
      if (!isConsole) rng <- rstudioapi::document_range(c(r, 1), c(r, Inf))
      # else use the selection range
    } else {
      rng <- sel$range
      if (!hasDots) {
        if (rng[[1]][[1]] != rng[[2]][[1]]) {
          sel$text <- strsplit(sel$text, "\n")[[1]]
          string <- vapply(sel$text, mod_str, "", from = from, to = to)
          string <- paste(string[grepl("<-", string)], collapse = "\n")
        } else {
          string <- mod_str(sel$text, from = from, to = to)
        }
      }
    }
  }
  rstudioapi::modifyRange(rng, string, context$id) |>
    invisible()
}
