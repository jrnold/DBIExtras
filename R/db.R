##' @export
setGeneric("length")

##' @export
setGeneric("names")

## Export all the generics here
##' @exportMethod [[
##' @exportMethod "$"
##' @exportMethod dbDisconnect
NULL

##' DBIConnection Generics
##'
##' This defines "[[" and "$" methosd for \code{dbConenction}s to
##' write, read, and delete tables.
##'
##' @docType methods
##' @rdname DBIConnection-methods
##' @name [[,DBIConnection,character,missing-method
##' @aliases [[,DBIConnection,character,missing-method
##' @examples
##' ## Not run:
##' library(RSQLite)
##' data(iris)
##' tmpdb <- tempfile()
##' drv <- dbDriver("SQLite")
##' con <- dbConnect(drv, tmpdb)
##'
##' ## No tables currently defined
##' dbListTables(con)
##' ## Create table iris
##' con[["iris"]] <- iris
##' dbListTables(con)
##' ## Read table
##' con[["iris"]]
##' ## Delete Table
##' con[["iris"]] <- NULL
##' dbListTables(con)
##'
##' ## $ can also be used to read/write/delete tables
##' con$iris <- iris
##' con$iris
##' con$iris <- NULL
##' dbListTables(con)
##'
setMethod("[[", c("DBIConnection", "character", "missing"),
          function(x, i, j, ...) {
              dbReadTable(x, i, ...)
          })

##' @docType methods
##' @name DBIConnection methods
##' @rdname DBIConnection-methods
##' @name [[<-,DBIConnection,character,missing,NULL-method
##' @aliases [[<-,DBIConnection,character,missing,ANY-method
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "ANY"),
          function(x, i, j, ..., value) {
              dbWriteTable(x, i, value, ...)
              x
          })

##' @docType methods
##' @rdname DBIConnection-methods
##' @name [[<-,DBIConnection,character,missing,NULL-method
##' @aliases [[<-,DBIConnection,character,missing,NULL-method
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "NULL"),
          function(x, i, j, ..., value) {
              dbRemoveTable(x, i, ...)
              x
          })

##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $,DBIConnection-method
##' @name $,DBIConnection-method
setMethod("$", signature(x="DBIConnection"),
          function(x, name) dbReadTable(x, name))

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,ANY-method
##' @name $<-,DBIConnection,ANY-method
setReplaceMethod("$", signature(x="DBIConnection",value="ANY"),
                 function(x, name, value) {
                     dbWriteTable(x, name, value)
                     x
                 })
##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,NULL-method
##' @name $<-,DBIConnection,NULL-method
setReplaceMethod("$", signature(x="DBIConnection", value="NULL"),
                 function(x, name, value) {
                     dbRemoveTable(x, name)
                     x
                 })

################################33
## setMethod("[", c("DBIConnection", "character", "missing"),
##           function(x, i, j, ...) {
##               structure(lapply(i, function(table) dbReadTable(x, i)),
##                         names=i)
##               x
##           })

## setReplaceMethod("[", c("DBIConnection", "character", "missing", "ANY"),
##           function(x, i, j, ..., value) {
##               for (idx in seq_along(i)) {
##                   dbWriteTable(x, i[idx], value, ...)
##               }
##               x
##           })

## setReplaceMethod("[", c("DBIConnection", "character", "missing", "data.frame"),
##           function(x, i, j, ..., value) {
##               for (idx in seq_along(i)) {
##                   dbWriteTable(x, i[idx], value, ...)
##               }
##               x
##           })

## setReplaceMethod("[", c("DBIConnection", "character", "missing", "list"),
##           function(x, i, j, ..., value) {
##               if (length(i) != length(value)) {
##                   stop("length(i) != length(value)")
##               }
##               for (idx in seq_along(i)) {
##                   dbWriteTable(x, i[idx], value[idx], ...)
##               }
##               x
##           })

## setReplaceMethod("[", c("DBIConnection", "character", "missing", "NULL"),
##           function(x, i, j, ..., value) {
##               for (table in i) {
##                   dbDeleteTable(x, table, ...)
##               }
##               x
##           })

###########################################

##' @name show
##' @docType methods
##' @rdname DBIConnection-methods
##' @docType methods
##' @aliases show,DBIConnection-method
##' @export
setMethod("show", "DBIConnection", function(object) str(object))

##' @name names
##' @docType methods
##' @rdname DBIConnection-methods
##' @docType methods
##' @aliases names,DBIConnection-method
##' @export
setMethod("names", "DBIConnection",
          function(x) dbListTables(x))

##' @name length
##' @docType methods
##' @rdname DBIConnection-methods
##' @docType methods
##' @aliases length,DBIConnection-method
##' @export
setMethod("length", "DBIConnection",
          function(x) length(dbListTables(x)))

###################################################

##' DBIDriver methods
##'
##' For signature \code{DBIDriver} disconnect all
##' connections. For signature \code{character}, coerce
##' the character into \code{DBIDriver} and run generic.
##'
##' @name dbDisconnect
##' @docType methods
##' @aliases dbDisconnect,DBIDriver-method
setMethod("dbDisconnect", "DBIDriver",
          function(conn, ...) {
              all(sapply(dbListConnections(conn),
                         dbDisconnect, ... = ...))
          })

##' Character methods
##'
##' @name dbDisconnect
##' @docType methods
##' @aliases dbDisconnect,character-method
setMethod("dbDisconnect", "character",
          function(conn) dbDisconnect(dbDriver(conn)))

