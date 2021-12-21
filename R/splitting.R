subsetArr <- function(arr, margin, ind, commas){
  commas[[margin]] <- ind
  eval(str2lang(paste0("arr[", paste(commas, collapse = ""), "]")))
}
splitLen <- function(x, lenOut, begin=NULL, end=NULL, ...){
  print(match.call())
  if (!is.null(begin) || !is.null(end)) x <- x[begin:end]
  l <- length(x)
  grp <- rep(1:lenOut, each = ceiling(l/lenOut))[1:l]
  unname(split(x, grp))
}
setGeneric("splitLen",signature = "x")
setMethod("splitLen", signature = "data.frame",
          definition = function(x, lenOut, begin=NULL, end=NULL, margin = 1, ...){
            if(!is.null(begin) || !is.null(end)){
              x <- if(margin==1) x[begin:end, ] else x[,begin:end]
              l <- dim(x)[margin]
            }
            l <- dim(x)[margin]
            grp <- rep(1:lenOut, each = ceiling(l/lenOut))[1:l]
            if(margin == 1){
              unname(split.data.frame(x, grp))
            } else {
              split(seq(ncol(x)), grp)
              sapply(split(seq(ncol(x)), grp), function(i) x[,i], simplify = FALSE)
            }
          })
setMethod("splitLen", signature = "matrix",
          definition = function(x, lenOut, begin=NULL, end=NULL, margin = 1, ...){
            if(!is.null(begin) || !is.null(end)){
              x <- if(margin==1) x[begin:end, ] else x[,begin:end]
              l <- dim(x)[margin]
            }
            l <- dim(x)[margin]
            grp <- rep(1:lenOut, each = ceiling(l/lenOut))[1:l]
            if(margin == 1){
              unname(split.data.frame(x, grp))
            } else {
              split(seq(ncol(x)), grp)
              sapply(split(seq(ncol(x)), grp), function(i) x[,i], simplify = FALSE)
            }
          })
setMethod("splitLen", signature = "array",
          definition = function(x, lenOut, begin=NULL, end=NULL, margin = 1, ...){
            commas <- as.list(rep(",", length(dim(x))))
            if(!is.null(begin) || !is.null(end)){
              x <- subsetArr(arr = x, margin = margin, ind = paste0(begin,":",end), commas = commas)
            }
            l <- dim(x)[margin]
            grp <- rep(1:lenOut, each = ceiling(l/lenOut))[1:l]
            grp <- split(seq(l), grp)
            sapply(grp, subsetArr, arr = x, margin = margin, commas = commas, simplify = FALSE)
          })
splitBy <- function(x, by, begin=NULL, end=NULL,...){
  if(!missing(begin)||!missing(end)) x <- x[begin:end]
  l <- length(x)
  unname(split(x, rep(1:ceiling(l/by), each = by)[1:l]))
}
setGeneric("splitBy", signature = "x")
setMethod("splitBy", signature = "data.frame",
          definition = function(x, by, begin=NULL, end=NULL, margin = 1){
            if(!is.null(begin) || !is.null(end)){
              x <- if(margin==1) x[begin:end, ] else x[,begin:end]
            }
            l <- dim(x)[margin]
            grp <- rep(1:ceiling(l/by), each = by)[1:l]
            if(margin == 1){
              unname(split.data.frame(x, grp))
            } else {
              split(seq(ncol(x)), grp)
              sapply(split(seq(ncol(x)), grp), function(i) x[,i], simplify = FALSE)
            }
          })
setMethod("splitBy", signature = "matrix",
          definition = function(x, by, begin=NULL, end=NULL, margin = 1){
            if(!is.null(begin) || !is.null(end)){
              x <- if(margin==1) x[begin:end, ] else x[,begin:end]
            }
            l <- dim(x)[margin]
            if(is.null(by)) by <- ceiling((end-begin+1)/2)
            grp <- rep(1:ceiling(l/by), each = by)[1:l]
            if(margin == 1){
              unname(split.data.frame(x, grp))
            } else {
              split(seq(ncol(x)), grp)
              sapply(split(seq(ncol(x)), grp), function(i) x[,i], simplify = FALSE)
            }
          })
setMethod("splitBy", signature = "array",
          definition = function(x, by, begin=NULL, end=NULL, margin = 1){
            commas <- as.list(rep(",", length(dim(x))))
            if(!is.null(begin) || !is.null(end)){
              x <- subsetArr(arr = x, margin = margin, ind = paste0(begin,":",end), commas = commas)
            }
            l <- dim(x)[margin]
            grp <- rep(1:ceiling(l/by), each = by)[1:l]
            grp <- split(seq(l), grp)
            sapply(grp, subsetArr, arr = x, margin = margin, commas = commas, simplify = FALSE)
          })
