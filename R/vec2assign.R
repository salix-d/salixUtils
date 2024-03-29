#' Replace inline a vector object into a string formated for assignment
#'
#' @param vec a vector object
#'
#' @description Function to convert a vector to a string formated like
#' 'c("elt1", "elt2", ...)' so it can be used to assign a variable with the
#' content of the vector without having to rerun the code used to make it.
#' @return a character vector of length 1 where the function was called.
#' @export
#'
#' @examples
#' islands_startingWithA <- grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)
#' # if the function is there more than once in the document,
#' # add a separator ('#---') after the line if you run code with ctrl + enter
#' # since when doing that the range starts on the next line of text, so if the
#' # function is there too it will act there instead
#' # You can also use alt + enter to run the single line or
#' # select the text calling the function to run it.
#' islands_startingWithA <- grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)
#' #---
#' grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)

vec2assign <- function(vec) {
  context <- rstudioapi::getActiveDocumentContext()
  sel <- context$selection[[1]]
  string <- vec2assign_str(vec)
  # if no selected text find the line on which the function got called
  # and the position to get the range to replace
  if (sel$text == "") {
    r <- sel$range$start[[1]]
    # find position of assignment or function if not assigned
    ln <- grep("<- ?|vec2assign", context$contents[[r]], value = TRUE)
    while (!length(ln)) {
      r <- r - 1
      ln <- grep("<- ?|vec2assign", context$contents[[r]], value = TRUE)
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
    pttrn <- "^vec2assign(.*)$|\\|> vec2assign\\(\\)$|\\|> vec2assign\\(vec = _\\)$"
    if (!grepl(pttrn, sel$text) || grepl("<-", sel$text)) stop()
    rng <- sel$range
  }
  rstudioapi::modifyRange(rng, string, context$id) |> invisible(x = _)
}
#' Convert a vector into a string formated for assignment
#'
#' @param vec a vector object
#'
#' @description Function to convert a vector to a string formated like
#' 'c("elt1", "elt2", ...)' so it can be used to assign a variable with the
#' content of the vector without having to rerun the code used to make it.
#' @return a character vector of length 1
#' @export
#'
#' @examples
#' vec <- grep("^A", names(islands), value = TRUE)
#' cat(vec2assign_str(vec))
#' # then copy-paste from console to the assignment
#' # OR save it in clipboard with the clipr package and then paste it to the assignment
#' # clipr::write_clip(vec2assign_str(vec))
#' islands_startingWithA <- c("Africa", "Antarctica", "Asia", "Australia", "Axel Heiberg")
vec2assign_str <- function(vec) {
  vec |>
    paste0('"', ..2 = _, '"') |>
    toString() |>
    paste0("c(", ..2 = _, ")")
}
