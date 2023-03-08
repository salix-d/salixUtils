commentBox <- function( w = 80, align = c("left","center","right"), padding = 2, lineNum = NULL){
  if(missing(align)) align <- "left"
  context <- rstudioapi::getSourceEditorContext()
  if(missing(lineNum)) sel <- context$selection[[1]]
  else {
    lastLine <- lineNum[length(lineNum)]
    sel <- list(
      range <- rstudioapi::document_range(
        rstudioapi::document_position(lineNum[1], 1),
        rstudioapi::document_position(lastLine, 7777)
      ),
      text <- readLines(rstudioapi::documentPath(context$id),
                        n = lastLine)[lineNum]
    )
  }
  txtBox <- format_box(sel$text, align = align, w = w, padding = padding)
  rstudioapi::modifyRange(sel$range, txtBox, context$id)
}

format_box <- function(lines, align, w, padding){
  if(grepl("\\n", lines)){
    lines <- strsplit(lines, "\\n")
    lines <- gsub("^[# ]+|[# =]+$", "", lines)
    if(any(longLines <- grepl(paste0(".{", w - (padding * 2) + 1,"}"), lines))){
      if(sum(longLines)==1) lines[[which(longLines)]] <- list(split_lines(lines[[which(longLines)]], w = w))
      else lines[longLines] <- sapply(lines[longLines], split_lines, w = w)
      lines <- unlist(lines)
    }
  } else {
    lines <- gsub("^[# ]+|[# =]+$", "", lines)
    if(nchar(lines) > w) lines <- split_lines(lines, w = w)
  }
  lines <- vapply(lines, align_line, character(1), align = align, w = w, padding = padding)
  hl <- paste(rep("#", w), collapse = "")
  pad <- paste(rep(" ", w-2), collapse = "")
  lines <- c(hl, paste0("#", c(pad,lines,pad), "#"), hl)
  paste(lines, collapse = "\n")
}

split_lines <- function (x, w){
  x <- gsub("^ +| +$", "", x)
  if (nchar(x) <= (w - 10)) {
    return(x)
  }
  s <- strsplit(x, "\\s+")[[1]]
  l <- length(s)
  if (l == 1) {
    coupe <- nchar(s)/2
    s <- c(substr(s, 1, coupe), substr(s, 1 + coupe, nchar(s)))
  }
  n <- floor(l/2)
  s1 <- paste(s[1:n], collapse = " ")
  s2 <- paste(s[(n + 1):l], collapse = " ")
  c(Recall(s1, w = w), Recall(s2, w = w))
}
align_line <- function(line, align, w, padding){
  l <- nchar(line)
  if(align=="center"){
    n <- m <- (w - l - 2)/2
    if(n != as.integer(n)) m <- n + 1
  } else if(align=="right") {
    n <- w - l - padding - 2
    m <- padding
  } else {
    n <- padding
    m <- w - l - padding - 2
  }
  paste(c(rep(" ", n), line, rep(" ", m)), collapse = "")
}
