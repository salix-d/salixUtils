#' Get taxonomic classification data from GBIF
#' @param name     character. Scientific name of the taxon.
#' @param rank     character. Taxonomic rank of the taxon. One of 'kingdom', 'phylum', 'class', 'order', 'family', 'genus' or 'species'.
#' @param kingdom  character. Which kingdom should the taxon be a child of.
#' @param phylum   character. Which phylum should the taxon be a child of. Ignored if rank is higher.
#' @param class    character. Which class should the taxon be a child of. Ignored if rank is higher.
#' @param order    character. Which order should the taxon be a child of. Ignored if rank is higher.
#' @param family   character. Which family should the taxon be a child of. Ignored if rank is higher.
#' @param genus    character. Which genus should the taxon be a child of. Ignored if rank is higher.
#' @param strict   logical.   If matching should be strict.
#' @param start    numeric.
#' @param asList   logical.   If the output should be a list. Default TRUE.
#' @param rows     numeric.
#'
#' @export
classification_gbif <- function(name, rank = NULL, kingdom = NULL, phylum = NULL,
                                class = NULL, order = NULL, family = NULL,
                                genus = NULL, strict = FALSE, start = NULL,
                                asList = TRUE, rows = NULL){
  params <- list(FUN = .classification_gbif,
                 name = name, rank = rank, kingdom = kingdom, phylum = phylum,
                 class = class, order = order, family = family, genus = genus,
                 strict = strict, start = start, asList = asList, rows = rows)
  params <- params[lengths(params) > 0]
  out <- do.call(mapply, c(params, SIMPLIFY = FALSE, USE.NAMES = TRUE))
  structure(unlist(out, recursive = FALSE), class = "classification", db = "gbif")
}

.classification_gbif <- function(name, rank = NULL, kingdom = NULL, phylum = NULL,
                                 class = NULL, order = NULL, family = NULL,
                                 genus = NULL, strict = FALSE, start = NULL,
                                 asList = TRUE, rows = NULL){
  url = "https://api.gbif.org/v1/species/match"
  ranks <- c("kingdom", "phylum", "class", "order", "family",
             "genus", "species")
  params <- c(name = name, rank = rank, kingdom = kingdom, phylum = phylum,
              class = class, order = order, family = family, genus = genus,
              strict = strict, start = start, verbose = TRUE)
  params[["rank"]] <- tolower(params[["rank"]])
  if (length(params[["rank"]]) &&
     length(params[c('kingdom', 'phylum', 'class', 'order', 'family', 'genus')])) {
    n <- which(c('kingdom', 'phylum', 'class', 'order', 'family', 'genus') == params[["rank"]])
    if (length(n) && n < 6) {
      ranks2rm <- names(params) %in% c('kingdom', 'phylum', 'class', 'order', 'family', 'genus')[n:6]
      warning("Ignoring ", toStr(paste0("`", names(params)[ranks2rm], "`")), " argument", if (sum(ranks2rm) > 1) "s"," as `rank` is higher or equal.")
      params <- params[!ranks2rm]
    }
  }
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
  if (any((filt_params <- names(params) %in% c("rank", ranks[1:6])))) {
    filt_params <- params[filt_params]
    if (all(tolower(filt_params) == tolower(tt[names(filt_params)]))) {
      dd <- dat
    } else return(empty_res)
  } else {
    dd <- as.data.frame(
      data.table::rbindlist(
        c(list(dat),
          lapply(tt$alternatives, `[<-`, j = lengths(tt$alternatives) == 0, value = NA)),
        use.names = TRUE, fill = TRUE, idcol = FALSE))
    dd <- dd[!is.na(dd[["usageKey"]]),]
    rownames(dd) <- NULL
  }
  if (nrow(dd) > 1) {
    if (!is.null(rows) && (is.integer(rows) | !is.numeric(rows)))
      dd <- dd[rows,]
    else {
      message(paste0("More than one GBIF ID found for taxon ", name, "!"))
      print(dd[,c("scientificName", "canonicalName", "rank", "status", "matchType", "synonym", "kingdom")])
      keep <- as.integer(readline("Enter a rownumber to select that taxon (other inputs will return empty):"))
      if (!is.na(keep) && keep > 0 && keep <= nrow(dd)) dd <- dd[keep,]
      else return(empty_res)
    }
  }
  names(dd) <- tolower(names(dd))
  rnk <- unique(tolower(dd$rank))
  if (length(rnk) == 1) {
    dd[,rnk] <- dd$canonicalname
    dd[,paste0(rnk, "key")] <- dd$usagekey
  } else {
    for (i in seq_along(rnk)) {
      dd[i,rnk[i]] <- dd$canonicalname[i]
      dd[i,paste0(rnk[i], "key")] <- dd$usagekey[i]
    }
  }
  ranks <- ranks[ranks %in% names(dd)]
  if (any(!rnk %in% ranks)) ranks <- unique(c(ranks, rnk))
  ranks_name_col <- vapply(paste0(ranks,"$"), grep, 0, x = names(dd))
  ranks_id_col <- vapply(paste0(ranks,"key$"), grep, 0, x = names(dd))
  names(dd)[ranks_id_col] <- paste0(names(dd)[ranks_name_col], "_id")
  names(dd)[ranks_name_col] <- paste0(names(dd)[ranks_name_col], "_name")
  if (asList) {
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
