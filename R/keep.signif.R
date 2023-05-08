#' Title
#'
#' @param x          numeric vector between 0 and 1
#' @param threshold  Default is 0.05
#'
#' @export
methods::setGeneric(name = "keep.signif",
                    def = function(x, threshold = 0.05){
                      methods::makeStandardGeneric("keep.signif")
                    },
                    signature = "x")
methods::setMethod(f = "keep.signif", signature = "list",
                   definition =  function(x, threshold){
                     if (inherits(x[[1]], "list")) {
                       n <- lapply(x, keep.signif, threshold = threshold)
                       x[lengths(n) > 0]
                     } else {
                       x[vapply(x, keep.signif, logical(1), threshold = threshold)]
                     }
                   })
methods::setMethod(f = "keep.signif", signature = "anova",
                   definition =  function(x, threshold){
                     any(x$`Pr(>F)` <= threshold, na.rm = TRUE)
                   })
methods::setOldClass("htest")
methods::setMethod(f = "keep.signif", signature = "htest",
                   definition =  function(x, threshold){
                     any(x$p.value <= threshold, na.rm = TRUE)
                   })
