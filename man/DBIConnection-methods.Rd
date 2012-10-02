\docType{methods}
\name{[[,DBIConnection,character,missing-method}
\alias{$}
\alias{$,DBIConnection-method}
\alias{$<-,DBIConnection,ANY-method}
\alias{$<-,DBIConnection,NULL-method}
\alias{DBIConnection methods}
\alias{[[,DBIConnection,character,missing-method}
\alias{[[<-,DBIConnection,character,missing,ANY-method}
\alias{[[<-,DBIConnection,character,missing,NULL-method}
\alias{length}
\alias{length,DBIConnection-method}
\alias{names}
\alias{names,DBIConnection-method}
\alias{show}
\alias{show,DBIConnection-method}
\title{DBIConnection Generics}
\description{
  This defines "[[" and "$" methosd for
  \code{dbConenction}s to write, read, and delete tables.
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

