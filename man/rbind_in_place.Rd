% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rbind_in_place.r
\name{rbind_in_place}
\alias{rbind_in_place}
\title{Efficiently Combine Two data.tables, data.frames and lists}
\usage{
rbind_in_place(.X, .Y, .keep.Y = FALSE, .use.names = FALSE, .fill = FALSE)
}
\arguments{
\item{.X}{A data.table, data.frame or list. .Y will be appended to this}

\item{.Y}{A data.table, data.frame or list. This will be appended to .X.
Unless you set .keep.Y to TRUE .Y will be deleted in the process.}

\item{.keep.Y}{Logical. Should .Y be deleted while combining the tables?}

\item{.use.names}{Logical. Like with rbind if you choose FALSE (default)
columns will be merged by position. Otherwise by column names.}

\item{.fill}{If set to TRUE columns of .X not present in .Y will be filled
               with NA. Otherwise you will get an error.
               Note that this function does not add additional columns to .X.}
}
\value{
The operation is done by reference so you do not need the return value.
Nevertheless .X (with the appended .Y) is returned for ease of use with
magrittr.
}
\description{
This function appends one data.table to another by "semi-reference." This
means that you need enough memory for both inputs + 1 column instead of twice
the memory of both inputs combine like you do for regular rbind (data.table or
not).
}
\examples{
# Bind by position
DF.1 <- data.frame(x=1:5, y=10:6)
DF.2 <- data.frame(y=5:1, x=6:10)

rbind_in_place(DF.1, DF.2)

# Bind by name
DF.1 <- data.frame(x=1:5, y=10:6)
DF.2 <- data.frame(y=5:1, x=6:10)

rbind_in_place(DF.1, DF.2, .use.names = TRUE)

# Fill missing columns
DF.1 <- data.frame(x=1:5, y=10:6, z=1:5)
DF.2 <- data.frame(y=5:1, x=6:10)

rbind_in_place(DF.1, DF.2, .use.names = TRUE, .fill = TRUE)
}

