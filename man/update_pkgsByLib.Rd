% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_pkgsByLib.R
\name{update_pkgsByLib}
\alias{update_pkgsByLib}
\title{Update packages by library paths}
\usage{
update_pkgsByLib(type = "source")
}
\arguments{
\item{type}{One of 'source', 'binary' or 'both'. (parameter from the install.packages function)}
}
\description{
Update packages by library paths
}
\details{
If \code{R_LIBS_SITE} is set, it will install packages from there first, else it will be in order of \code{R_LIBS_USER} and/or \code{.libPaths()}.
}
