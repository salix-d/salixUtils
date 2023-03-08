#' Convert a vector to a string (for assignment)
#'
#' @param vec a vector to convert
#' @return a string version of the vertoc (ie: 'c("elt1", "elt2", ...)')
#'
#' @examples \dontrun{
#' grep("^A", names(islands), value = TRUE) |> vec2str(vec = _) |>
#'   cat()
#' grep("^A", names(islands), value = TRUE) |> vec2str(vec = _) |>
#'   clipr::write_clip()
#' }
#' @export
vec2str <- function(vec) {
  vec |>
    paste0('"', ..2 = _, '"') |>
    toString() |>
    paste0("c(", ..2 = _, ")")
}

#' Convert a line of text representing a vector to a string (for assignment)
#'
#' @param vec  a line of text to convert
#'
#' @return inplace replacement of the line to a string
#'
#' @examples \dontrun{
#' islands_startingWithA <- grep("^A", names(islands), value = TRUE) |> vec2strI(vec = _)
#' #---
#' # need a separator or to use alt + enter because ctrl + enter ends on the
#' # next line and so the range start there, so if the function is there too
#' # it will act there instead
#' grep("^A", names(islands), value = TRUE) |> vec2strI(vec = _)
#' }
#' @export
vec2strI <- function(vec) {
  context <- rstudioapi::getActiveDocumentContext()
  sel <- context$selection[[1]]
  string <- vec2str(vec)
  # if no selected text find the line on which the function got called
  # and the position to get the range to replace
  if (sel$text == "") {
    r <- sel$range$start[[1]]
    # find position of assignment or function if not assigned
    ln <- grep("<- ?|vec2strI", context$contents[[r]], value = TRUE)
    while (!length(ln)) {
      r <- r - 1
      ln <- grep("<- ?|vec2strI", context$contents[[r]], value = TRUE)
    }
    # if assigned, makes sure to not erase '<-' by adding 2 to the position
    if (grepl("<-", ln)) {
      c <- regexpr("<-", ln) + 2
      string <- paste0(" ", string)
    # if not, replace from start of line
    } else {
      c <- 1
    }
    rng <- rstudioapi::document_range(c(r, c), c(r, Inf))
  # else use the selection range
  } else {
    # make sure to not replace the assignment if selected!
    pttrn <- "^vec2strI(.*)$|\\|> vec2strI\\(\\)$|\\|> vec2strI\\(vec = _\\)$"
    if (!grepl(pttrn, sel$text) || grepl("<-", sel$text)) stop()
    rng <- sel$range
  }
  rstudioapi::modifyRange(rng, string, context$id) |>
    invisible()
}
