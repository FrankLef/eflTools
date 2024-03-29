% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ren_many.R
\name{ren_many}
\alias{ren_many}
\title{Rename Many Columns}
\usage{
ren_many(data, ren, ignore_case = TRUE)
}
\arguments{
\item{data}{Dataframe.}

\item{ren}{Named character vector with c("new name" = "old name").}

\item{ignore_case}{Ignore case if \code{TRUE} (default).}
}
\value{
Dataframe with new names.
}
\description{
Rename many columns  using a named vector.
}
\details{
Rename many columns using a named character vector where the names are the
new name and the element the old name. See the example.
}
\examples{
df <- iris
df <- ren_many(df,
 ren = c("sepal.length" = "Sepal.Length", "flower" = "Species"))
stopifnot(identical(
 names(df),
 c("sepal.length", "Sepal.Width", "Petal.Length", "Petal.Width", "flower")
 ))
}
