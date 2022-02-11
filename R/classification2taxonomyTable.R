#' @export
classification2taxonomyTable <- class2taxo <- function(li,
                                                       ranks = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                                                       rnames = T){
  if(class(li) != "classification") stop("Wrong input class. Object's class must be 'classification' (from taxize)")
  l <- length(ranks)
  out <- vapply(li, function(x){
    r <- match(ranks, x$rank, 0L)
    out <- array(NA_character_, dim = l, dimnames = list(ranks))
    out[x$rank[r]] <- x$name[r]
    out
  }, character(l), USE.NAMES = rnames)
  if(l==1) as.data.frame(out)
  else {
    as.data.frame(t(out))
  }
}
