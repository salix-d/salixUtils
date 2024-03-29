% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vec2assign.R
\name{vec2assign}
\alias{vec2assign}
\title{Replace inline a vector object into a string formated for assignment}
\usage{
vec2assign(vec)
}
\arguments{
\item{vec}{a vector object}
}
\value{
a character vector of length 1 where the function was called.
}
\description{
Function to convert a vector to a string formated like
'c("elt1", "elt2", ...)' so it can be used to assign a variable with the
content of the vector without having to rerun the code used to make it.
}
\examples{
islands_startingWithA <- grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)
# if the function is there more than once in the document,
# add a separator ('#---') after the line if you run code with ctrl + enter
# since when doing that the range starts on the next line of text, so if the
# function is there too it will act there instead
# You can also use alt + enter to run the single line or
# select the text calling the function to run it.
islands_startingWithA <- grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)
#---
grep("^A", names(islands), value = TRUE) |> vec2assign(vec = _)
}
