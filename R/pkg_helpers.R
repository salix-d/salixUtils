get_imports <- function() {
  ns_lines <- grep_files("::")
  vapply(ns_get(ns_lines), \(ns){
    f <- ns_getFunctions(ns, ns_lines)
    paste("#' @importFrom", ns, f, collapse = "\n")
  }, "") |>
    paste(collapse = "\n") |>
    clipr::write_clip()
}
# find all namespace called in your package/folder
ns_get <- function(ns_lines = NULL){
  if (!length(ns_lines)) ns_lines <- grep_files("::")
  ns_lines |>
    stri_extract("[[[:alpha:]]\\.]+(?=::)",
                 simplify = TRUE,
                 omit_no_match = TRUE) |>
    c(recursive = TRUE) |>
    salixUtils::rm_empty() |>
    unique()
}
# find all functions used from an imported namespace in your package/folder
ns_getFunctions <- function(ns, ns_lines = NULL) {
  if (!length(ns_lines))
    ns_lines <- grep_files(paste0("'",ns,"::'"))
  ns_lines |>
    stringi::stri_extract_all_regex(paste0("(?<=",ns,"::)[^\\(\\) ]+"),
                                    simplify = TRUE,
                                    omit_no_match = TRUE) |>
    c(recursive = TRUE) |>
    salixUtils::rm_empty() |>
    unique()
}
