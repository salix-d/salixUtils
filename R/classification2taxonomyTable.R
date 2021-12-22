#' @export
classification2taxonomyTable <- class2taxo <- function(li,
                                                       ranks = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                                                       rnames = T){
  if(class(li) != "classification") stop("Wrong input class. Object's class must be 'classification' (from taxize)")
  out <- sapply(li, function(x){
    out <- x[x$rank %in% ranks, c("name", "rank")]
    out <- stats::setNames(out$name, out$rank)
    out[ranks[!ranks %in% names(out)]] <- NA_character_
    return(out[ranks])
  })
  out <- data.frame(t(out), row.names = if(rnames) names(li) else NULL)
  return(out)
}
