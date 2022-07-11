.classification_gbif <- function(name, rank = NULL, kingdom = NULL, phylum = NULL, class = NULL,
                                 order = NULL, family = NULL, genus = NULL, strict = FALSE,
                                 start = NULL, asList = TRUE, rows = NULL){
  if(!is.character(name)) stop("`name` must be a character vector.")
  url = "https://api.gbif.org/v1/species/match"
  ranks <- c("kingdom", "phylum", "class", "order", "family",
             "genus", "species")
  params <- Filter(length, list(name = name, rank = rank, kingdom = kingdom,
                                phylum = phylum, class = class, order = order, family = family,
                                genus = genus, strict = strict, offset = start,
                                verbose = TRUE))
  cli <- crul::HttpClient$new(url = url)
  temp <- cli$get(query = params)
  temp$raise_for_status()
  tt <- jsonlite::fromJSON(temp$parse("UTF-8"), FALSE)
  empty_res <- list(NA)
  if (all(names(tt) %in% c("confidence", "synonym", "matchType"))) {
    return(empty_res)
  }
  dat <- data.frame(tt[!names(tt) %in% c("alternatives",
                                         "note")], stringsAsFactors = FALSE)
  if(any(filt_params <- names(params) %in% c("rank", ranks[1:6]))){
    filt_params <- params[filt_params]
    if(all(!names(filt_params) %in% names(tt))) return(empty_res)
    if(all.equal(filt_params, tt[names(filt_params)])){
      dd <- dat
    }
  } else {
    dd <- as.data.frame(data.table::rbindlist(c(list(dat), lapply(tt$alternatives, `[<-`, j = lengths(x)==0, value = NA)), use.names = TRUE,
                                              fill = TRUE, idcol = FALSE))
    dd <- dd[!is.na(dd[["usageKey"]]),]
    rownames(dd) <- NULL
  }
  if(nrow(dd)>1){
    if(!is.null(rows) && (is.integer(rows) |! is.numeric(rows)))
      dd <- dd[rows,]
    else {
      message(paste0("More than one GBIF ID found for taxon ", name, "!"))
      print(dd[,c("scientificName", "canonicalName", "rank", "status", "matchType", "synonym", "kingdom")])
      keep <- as.integer(readline("Enter a rownumber to select that taxon (other inputs will return empty):"))
      if(!is.na(keep) && keep > 0 && keep <= nrow(dd)) dd <- dd[keep,]
      else return(empty_res)
    }
  }
  names(dd) <- tolower(names(dd))
  rnk <- unique(tolower(dd$rank))
  if(length(rnk)==1){
    dd[,rnk] <- dd$canonicalname
    dd[,paste0(rnk, "key")] <- dd$usagekey
  } else {
    for(i in seq_along(rnk)){
      dd[i,rnk[i]] <- dd$canonicalname[i]
      dd[i,paste0(rnk[i], "key")] <- dd$usagekey[i]
    }
  }
  ranks <- ranks[ranks %in% names(dd)]
  if(any(!rnk %in% ranks)) ranks <- unique(c(ranks, rnk))
  ranks_name_col <- vapply(paste0(ranks,"$"), grep, 0, x = names(dd))
  ranks_id_col <- vapply(paste0(ranks,"key$"), grep, 0, x = names(dd))
  names(dd)[ranks_id_col] <- paste0(names(dd)[ranks_name_col], "_id")
  names(dd)[ranks_name_col] <- paste0(names(dd)[ranks_name_col], "_name")
  if(asList){
    lapply(seq_len(nrow(dd)), function(r){
      data.frame(
        name = c(dd[r,ranks_name_col], recursive = TRUE),
        rank = ranks,
        id = c(dd[r,ranks_id_col], recursive = TRUE),
        row.names = NULL
      )
    })
  } else {
    dd[,c(rbind(ranks_name_col, ranks_id_col), recursive = TRUE)]
  }
}
classification_gbif <- function (name, rank = NULL, kingdom = NULL, phylum = NULL, class = NULL,
                                 order = NULL, family = NULL, genus = NULL, strict = FALSE,
                                 start = NULL, asList = TRUE, rows = NULL){
  params <- Filter(length, list(FUN = .classification_gbif, name = name, rank = rank, kingdom = kingdom,
                                phylum = phylum, class = class, order = order, family = family,
                                genus = genus, strict = strict, start = start, asList = asList, rows = rows))
  out <- do.call(mapply, c(params, SIMPLIFY = FALSE, USE.NAMES = TRUE))
  structure(unlist(out, recursive = FALSE), class = "classification", db = "gbif")
}
classification_gbif(c("Salix", "Acer", "Pinus"), kingdom = "Plantae", strict = TRUE, rows = 1)
all.equal(classification_gbif("Salix", kingdom = "Plantae"), classification_gbif("Salix"))

classification_gbif(c("Acer saccharum", "Abies balsamea", "Salix arctica"), kingdom = c("Animalia", "Plantae", "Plantae"), strict = TRUE, rows = 1)
classification_gbif(c("Salix", "Acer", "Pinus"), kingdom = c("Animalia", "Plantae", "Plantae"), strict = TRUE, rows = 1)
