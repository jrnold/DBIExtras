\docType{methods}
\name{extract-methods}
\alias{[[-methods}
\alias{[[,DBIConnection,character,missing-method}
\alias{[[<--methods}
\alias{[[<-,DBIConnection,character,missing,ANY-method}
\alias{[[<-,DBIConnection,character,missing,NULL-method}
\alias{$-methods}
\alias{$,DBIConnection-method}
\alias{$<--methods}
\alias{$<-,DBIConnection,ANY-method}
\alias{$<-,DBIConnection,NULL-method}
\alias{extract-methods}
\title{Extract and Assignement Methods}
\description{
  Extract and Assignement Methods
}
\examples{
## Not run:
library(RSQLite)
data(iris)
tmpdb <- tempfile()
drv <- dbDriver("SQLite")
con <- dbConnect(drv, tmpdb)

## No tables currently defined
dbListTables(con)
## Create table iris
con[["iris"]] <- iris
dbListTables(con)
## Read table
con[["iris"]]
## Delete Table
con[["iris"]] <- NULL
dbListTables(con)

## $ can also be used to read/write/delete tables
con$iris <- iris
con$iris
con$iris <- NULL
dbListTables(con)
}
\keyword{methods}

