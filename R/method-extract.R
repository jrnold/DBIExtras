##' @exportMethod $
##' @exportMethod $<-
##' @exportMethod [[
##' @exportMethod [[<-
NULL

##' Extract and Assignement Methods 
##'
##' @name extract-methods
##' @rdname extract-methods
##' @docType methods
##' @keywords methods
##'
##' @aliases [[-methods
##' @aliases [[,DBIConnection,character,missing-method
##' @aliases [[<--methods
##' @aliases [[<-,DBIConnection,character,missing,ANY-method
##' @aliases [[<-,DBIConnection,character,missing,NULL-method
##' @aliases $-methods
##' @aliases $,DBIConnection-method
##' @aliases $<--methods
##' @aliases $<-,DBIConnection,ANY-method
##' @aliases $<-,DBIConnection,NULL-method
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
##'
NULL

# ----------------------

setMethod("[[", c("DBIConnection", "character", "missing"),
          function(x, i, j, ...) {
              dbReadTable(x, i, ...)
          })

setReplaceMethod("[[", c("DBIConnection", "character", "missing", "ANY"),
          function(x, i, j, ..., value) {
              dbWriteTable(x, i, value, ...)
              x
          })

setReplaceMethod("[[", c("DBIConnection", "character", "missing", "NULL"),
          function(x, i, j, ..., value) {
              dbRemoveTable(x, i, ...)
              x
          })

# ----------------------

setMethod("$", signature(x="DBIConnection"),
          function(x, name) dbReadTable(x, name))

setReplaceMethod("$", signature(x="DBIConnection",value="ANY"),
                 function(x, name, value) {
                     dbWriteTable(x, name, value)
                     x
                 })

setReplaceMethod("$", signature(x="DBIConnection", value="NULL"),
                 function(x, name, value) {
                     dbRemoveTable(x, name)
                     x
                 })
