str2lines_format <- function(string, width = 80){
  if (length(width) != 1)
    stop("'width' must be of length 1")
  if (!is.numeric(width) && !is.integer(width))
    stop("'width' must be of type integer or numeric")
  if (width <= 0)
    stop("'width' must be greater than 0")
  if ((n <- nchar(string, type = "w")) > width) {
    lttrs <- strsplit(string, "")[[1]]
    strings <- character(ceiling(n / (width - 1)))
    i <- 1
    for (j in seq_along(lttrs)) {
      l <- lttrs[j]
      n <- nchar(strings[[i]])
      if (n + 1 != width) {
        if (n == 0 && l == " ")
          next
        else
          strings[[i]] <- paste0(strings[[i]], l)
      } else {
        if (l %in% c(LETTERS, letters, 0:9) && lttrs[j + 1] %in% c(LETTERS, letters, 0:9)) {
          if (lttrs[j - 1] %in% c(LETTERS, letters, 0:9))
            strings[[i]] <- paste0(strings[[i]], "-")
          else
            strings[[i]] <- paste0(strings[[i]], " ")
          i <- i + 1
          strings[[i]] <- paste0(strings[[i]], l)
        } else {
          strings[[i]] <- paste0(strings[[i]], l)
          i <- i + 1
        }
      }
    }
    string <- concat(strings[strings != ""], "\n")
  }
  string
}
