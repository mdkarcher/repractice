% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/question_helpers.R
\name{comma_and}
\alias{comma_and}
\title{Concatenate a list with commands and a concluding "and"}
\usage{
comma_and(x, oxford = TRUE)
}
\arguments{
\item{x}{vector string-coercible objects.}

\item{oxford}{logical whether to include the Oxford comma.}
}
\value{
string of comma concatenated (string-coerced) objects.
}
\description{
Concatenate a list with commands and a concluding "and"
}
\examples{
comma_and(1:3)
}
