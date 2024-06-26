% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm2.R
\name{rm2}
\alias{rm2}
\title{Remove Objects from a Specified Environment containing a specified pattern}
\usage{
rm2(pattern, pos = -1, envir = NULL, inherits = FALSE)
}
\arguments{
\item{pattern}{a regular expression. Only names matching pattern will be removed.}

\item{pos}{where to do the removal. By default, uses the current environment.}

\item{envir}{the environment to use.}

\item{inherits}{should the enclosing frames of the environment be inspected?}
}
\description{
remove object(s) matching a pattern from an environment. The arguments pos, envir and inherits are passed to \link[base:rm]{remove}. A prompt ask to confirm the removal of variables in case the pattern matches somethings the user wants to keep.
}
\examples{
\dontrun{
x <- letters
rm2("^x$")
}
}
