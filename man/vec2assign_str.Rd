% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vec2assign.R
\name{vec2assign_str}
\alias{vec2assign_str}
\title{Convert a vector into a string formated for assignment}
\usage{
vec2assign_str(vec)
}
\arguments{
\item{vec}{a vector object}
}
\value{
a character vector of length 1
}
\description{
Function to convert a vector to a string formated like
'c("elt1", "elt2", ...)' so it can be used to assign a variable with the
content of the vector without having to rerun the code used to make it.
}
\examples{
vec <- grep("^A", names(islands), value = TRUE)
cat(vec2assign_str(vec))
# then copy-paste from console to the assignment
# OR save it in clipboard with the clipr package and then paste it to the assignment
# clipr::write_clip(vec2assign_str(vec))
islands_startingWithA <- c("Africa", "Antarctica", "Asia", "Australia", "Axel Heiberg")
}
