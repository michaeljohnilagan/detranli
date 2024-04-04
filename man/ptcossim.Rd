% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ptcossim.R
\name{ptcossim}
\alias{ptcossim}
\title{Person-total cosine similarity}
\usage{
ptcossim(x, ref)
}
\arguments{
\item{x}{data (matrix or dataframe) of interest.}

\item{ref}{reference sample (matrix or dataframe). Must have same number of columns as \code{x}.}
}
\value{
Vector of person-total cosine similarities.
}
\description{
Calculates the person-total cosine similarity with respect to some reference sample.
}