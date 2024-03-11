#' @export
throw_error <- function(..., call. = TRUE, call.pos = 1L, class = ""){
  if (call.) {
    caller <- if (identical(parent.frame(), .GlobalEnv)) rlang::current_call() else rlang::caller_call(call.pos)
  }
  if (length(caller)) {
    caller <- as.character(caller[vapply(caller, is.name, logical(1))])
    caller <- paste0("\033[31;1m\033]8;;ide:help:", caller, "\a", caller, "\033]8;;\a\033[0m")
    head <- paste0("\033[31;1mError\033[m", " in `", caller, "\033[31m`:\033[0m")
  } else {
    head <- paste0("\033[31;1mError", ":\033[0m")
  }
  body <- paste0("\033[31m✖\033[0m ", ...)
  cat(head, body, sep = "\n")
  rlang::local_options(show.error.messages = FALSE)
  stop()
}

warn <- function(..., call. = TRUE, caller = NULL) {
  if (call. && missing(caller)) caller <- rlang::caller_call()
}
# x <- function() throw_error("no arguments provided")
