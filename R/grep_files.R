#' Grep your file like in terminal
#'
#' @param pattern    (character) pattern to look for
#' @param recursive  (logical) Use "-r" when `TRUE` (Default)
#' @param include    (character) File pattern to use with "--include"
#' @param output     (character) One of "all", "lines" or "names". Whether to return just the lines or the names of the file, or all datails.
#'
#' @return    (character) a vector of the results
#' @export
grep_files <- function(pattern, recursive = TRUE, include="*.[Rr]", output = c("all", "lines", "names")){
    output <- match.arg(output, c("all", "lines", "names"), several.ok = FALSE)
    cmd <- paste0("grep -n ",
                  if (recursive) "-r " else NULL,
                  "--include='", include, "' '",
                  pattern,
                  "'")
    out <- suppressWarnings(system(cmd, intern = TRUE))
    if (length(out)) {
        switch(
            output,
            all = out,
            lines = {
                out <- str_split(out, ":", simplify = TRUE)[,1:2]
                vapply(unique(out[,1]), \(y) paste(out[out[,1] == y, 2], collapse = " "), "")
            },
            "names" = unique(str_split(out, ":", simplify = TRUE)[,1]),
            NULL
        )
    } else {
        NULL
    }
}
