% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/drop_in_place.r
\name{drop_in_place}
\alias{drop_in_place}
\title{Efficiently Remove Rows from a data.table, data.frame or list}
\usage{
drop_in_place(.data, .n = NULL, .keep = NULL)
}
\arguments{
\item{.data}{A data.table, data.frame or list}

\item{.n}{The first .n rows of the table will be deleted.}

\item{.keep}{The same as i in a data.table, .data[i,j]. I.e. a selector
               that specifies which rows should be kept. This must not be a join.}
}
\description{
This function allows you to delete rows from a data.table, data.frame or list
without copying the whole table. Instead you need space in RAM for the
table + 1 column. This function might change the row order of the table if it
is not keyed.
}

