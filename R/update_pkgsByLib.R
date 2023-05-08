#' Update packages by library paths
#'
#' @param type    One of 'source', 'binary' or 'both'. (parameter from the install.packages function)
#' @details
#' If `R_LIBS_SITE` is set, it will install packages from there first, else it will be in order of `R_LIBS_USER` and/or `.libPaths()`.
#'
#'
#' @export
#'
update_pkgsByLib <- function(type = "source"){
  libpaths <- Sys.getenv(c("R_LIBS_SITE", "R_LIBS_USER"), names = FALSE)
  #in case there are other not in those setting
  libpaths <- c(libpaths, setdiff(libpaths, .libPaths()))
  libpaths <- c(libpaths[base_lib], libpaths[!base_lib])
  old.pkg <- old.packages()
  while (length(old.pkg)) {
    for (lib in libpaths) {
      # getting packages that need updating :
      pkg2inst <- old.pkg[old.pkg[,"LibPath"] == lib, "Package"]
      if (!length(pkg2inst)) break
      # break if none in that lib
      pkg_loaded <- pkg2inst %in% (.packages())
      # check if loaded
      if (any(pkg_loaded)) {
        # unload if it is
        for (x in pkg2inst[pkg_loaded]) {
          detach(paste0("package:", x),
                 unload = TRUE)
        }
      }

      pkg2inst |>
        vapply(
          FUN = packageDescription,
          fields = "Imports",
          drop = TRUE,
          FUN.VALUE = character(1L)
        ) |>
        c(recursive = TRUE, use.names = FALSE) |>
        unique() |>
        intersect(old.pkg[,"Package"]) |>
        c(pkg2inst) |>
        unique() |>
        install.packages(
          lib = lib,
          destdir = "D:/R/tmp_pkg_install/",
          dependencies = NULL,
          type = type
        )

      # remove installed package
      old.pkg <- old.pkg[LibPath != lib]
    }
  }
}
