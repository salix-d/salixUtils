#' Split your file(s) containing multiple functions into multiple files containing one function.
#'
#' @param filePath     (character) The path(s) of the directory or file(s) you want to split.
#' @param outDir        (character) Optional. A directory path where to write the new files. If missing, place them in the filePath directory. Can be the same length as filePath since it will be mapp(ly)ed.
#' @param keepComments  (logical) Whether to keep the comments before the functions and/or at the end of the file. Default is `TRUE`.
#' @param recursive     (logical) When filePath is a directory, should the file be listed recursively (with `list.files`). Default is `TRUE`.
#'
#' @return NULL
#' @export
split_file_into_oneFunFiles <- function(filePath,
                                        outDir = NULL,
                                        keepComments = TRUE,
                                        recursive = TRUE) {
  if (!length(filePath))
    stop("argument 'filePath' is missing, with no default")
  if (length(filePath) == 1 && dir.exists(filePath)) {
    filePath <- list.files(filePath, pattern = "*.[Rr]",
                           full.names = TRUE,
                           recursive = recursive)
  }
  if (is.null(outDir)) {
    dir <- dirname(filePath)
  } else{
    dir <- outDir
    if (!dir.exists(dir))
      dir.create(dir)
  }
  if (length(filePath) > 1) {
    for (x in filePath) {
      mapply(split_file_into_oneFunFiles, x, dir)
    }
  } else {
    if (!is.character(filePath))
      stop("'filePath' should be of class character")
    if (is.null(filePath) || is.na(filePath) || !nzchar(filePath))
      stop("'filePath' can't be empty")
    lines <- stringi::stri_read_lines(filePath)
    funMap <- rbind(find_funs(lines), find_method_funs(lines))
    if (!length(funMap)) {
      stop("Sorry, didn't find any function assignment.")
    }
    funMap <- funMap[order(funMap$n),]
    rownames(funMap) <- NULL
    funMap$dif <- funMap$n - c(0, funMap$end[-nrow(funMap)])
    funMap$begin <- funMap$n
    nrw <- nrow(funMap)
    if (keepComments) {
      funMap$begin[funMap$dif > 1] <-
        vapply(seq_len(nrw)[funMap$dif > 1], \(i) {
          e <- funMap$begin[i]
          b <- e - funMap$dif[i] + 1L
          find_last_comment(lines, b, e)
        }, 0)
      if (funMap$end[nrw] != length(lines)) {
        funMap$end[nrw] <- find_last_comment(lines,
                                             length(lines),
                                             funMap$end[nrw])
      }
    }
    funMap <- find_S3methods(funMap = funMap)
    for (i in seq_len(nrw)) {
      if (funMap$type[i] == "method" &&
          any(funMap$name[1:(i - 1)] == funMap$name[i])) {
        cat(lines[funMap$begin[i]:funMap$end[i]],
            sep = "\n",
            file = file.path(dir, paste0(funMap$name[i], ".R")),
            append = TRUE)
      } else {
        cat(lines[funMap$begin[i]:funMap$end[i]],
            sep = "\n",
            file = file.path(dir, paste0(funMap$name[i], ".R")))
      }
    }
  }
}
find_S3methods <- function(funMap){
  mtd <- stringi::stri_detect_fixed(funMap$name, ".")
  if (sum(mtd, na.rm = TRUE)) {
    nms <- stringi::stri_extract_first_regex(funMap$name[mtd], "^[^\\.]+(?=\\.)")
    names(nms) <- funMap$name[mtd]
    nms2 <- funMap$name[funMap$name %in% nms]
    if (length(nms2)) {
      names(nms2) <- nms2
      nms <- c(nms2, nms)
    }
    n <- funMap$name %in% names(nms)
    if (sum(n, na.rm = TRUE) != length(nms)) stop()
    funMap$name[n] <- nms
    funMap$type[n] <- "method"
    funMap$type[n][!duplicated(nms)] <- "first"
  }
  funMap
}
find_funs <- function(lines){
  multiLinesFun <-
    stringi::stri_detect_regex(lines,
                               "\\S+\\s*(=|<-)\\s*(function|\\\\)\\([^\\)]*\\)\\s*\\{") |> which()
  multiLinesFun <- multiLinesFun[!vapply(multiLinesFun, \(i){
    if (i > 1)
      stringi::stri_detect_regex(lines[i - 1], ",\\s*$")
    else
      FALSE
  }, NA)]
  oneLineFun <-
    stringi::stri_detect_regex(lines,
                               "\\S+\\s*(=|<-)\\s*(function|\\\\)\\([^\\)]*\\)\\s*[^\\{]*$") |> which()
  if (length(oneLineFun)) {
    tmp <- vapply(oneLineFun, \(n) {
      m <-
        stringi::stri_detect_regex(lines[-(1:n)], "^\\s*[^#]", max_count = 1) |> which()
      stringi::stri_detect_regex(lines[n + m], "^\\s*\\{")
    }, NA)
    multiLinesFun <- sort(c(multiLinesFun, oneLineFun[tmp]))
    oneLineFun <- oneLineFun[!tmp]
  }
  funMap <- rbind(
    if (length(oneLineFun))
      data.frame(type = "one", n = oneLineFun, end = oneLineFun)
    else
      NULL,
    if (length(multiLinesFun))
      data.frame(type = "multi", n = multiLinesFun, end = 0L)
    else
      NULL
  )
  if (length(funMap)) {
    funMap <- funMap[order(funMap$n),]
    rownames(funMap) <- NULL
    for (i in seq_len(sum(funMap$type == "multi"))) {
      l <- length(lines)
      j <- funMap$n[funMap$type == "multi"][i]
      n <- stringi::stri_count_fixed(lines[j], c("{", "}"))
      while ((n[1] == 0 || n[1] != n[2]) && j < l) {
        j <- j + 1L
        n <- n + stringi::stri_count_fixed(lines[j], c("{", "}"))
      }
      funMap$end[funMap$type == "multi"][i] <- j
    }
    if (any(is.na(funMap$end)) || !all(funMap$end >= funMap$n)) {
      stop("Sorry, having trouble matching opening and closing brackets")
    }
    funMap$name <-
      stringi::stri_extract_first_words(lines[funMap$n])
    funMap
  } else {
    NULL
  }
}
find_method_funs <- function(lines){
  methodsFun.begin <-
    stringi::stri_detect_regex(lines, "(methods::)?(setGeneric|setMethod)\\(") |> which()
  if (length(methodsFun.begin)) {
    methodsFun.ends <- integer(length(methodsFun.begin))
    for (i in seq_along(methodsFun.begin)) {
      l <- length(lines)
      n <- c(1L, 0L)
      j <- methodsFun.begin[i]
      while (n[1] != n[2] && j < l) {
        j <- j + 1L
        n <- n + stringi::stri_count_fixed(lines[j], c("(", ")"))
      }
      methodsFun.ends[[i]] <- j
    }
    if (length(methodsFun.begin) != length(methodsFun.ends)) {
      stop(
        "Sorry, having trouble matching opening and closing parenthesis of method functions"
      )
    }
    methodsFun <-
      data.frame(type = "method",
                 n = methodsFun.begin,
                 end = methodsFun.ends)
    methodsFun$name <-
      stringi::stri_extract_first_regex(lines[methodsFun$n], "(?<=\"|')\\S+(?=\"|')")
    if (any(is.na(methodsFun$name))) {
      methodsFun$name[is.na(methodsFun$name)] <-
        stringi::stri_extract_first_regex(lines[methodsFun$n + 1L], "(?<=\"|')\\S+(?=\"|')")
    }
    if (any(is.na(methodsFun$name))) {
      stop("Sorry, having trouble finding the names of the method functions")
    }
    methodsFun$type[!duplicated(methodsFun$name)] <- "first"
  } else {
    NULL
  }
}
find_last_comment <- function(x, b, e) {
  n <- which(stringi::stri_detect_regex(x[b:e], "^\\s*#"))
  if (length(n))
    b + max(n) - 1L
  else
    e
}
