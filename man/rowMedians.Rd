% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowMedians.R
\name{rowMedians}
\alias{rowMedians}
\alias{colMedians}
\title{Row-/Colwise Medians}
\usage{
rowMedians(x, na.rm = FALSE)

colMedians(x, na.rm = FALSE)
}
\arguments{
\item{x}{\code{matrix}}

\item{na.rm}{\code{logical(1)}, should \code{NA} be removed?}
}
\value{
\code{numeric}, medians for each row/column.
}
\description{
Row-/Colwise Medians similar to \code{rowMeans}.
}
\examples{
m <- matrix(1:8, nrow = 2)
rowMedians(m)
colMedians(m)
}
