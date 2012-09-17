##' @export
setGeneric("length")
##' @export
setGeneric("names")

##' DBIConnection Generics
##'
##' @name [[
##' @aliases [[,DBIConnection-method,character-method,missing-method
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases [[,DBIConnection,character,missing-method
##' @export
setMethod("[[", c("DBIConnection", "character", "missing"),
          function(x, i, j, ...) {
              dbReadTable(x, i, ...)
          })

##' Extract DBIConnection tables
##'
##' @name [[
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases [[<-,DBIConnection,character,missing,ANY-method
##' @export
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "ANY"),
          function(x, i, j, ..., value) {
              dbWriteTable(x, i, value, ...)
              x
          })

##' @name [[
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases [[<-,DBIConnection,character,missing,NULL-method
##' @export
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "NULL"),
          function(x, i, j, ..., value) {
              dbRemoveTable(x, i, ...)
              x
          })

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $,DBIConnection-method
##' @export
setMethod("$", signature(x="DBIConnection"),
          function(x, name) dbReadTable(x, name))

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,ANY-method
##' @export
setReplaceMethod("$", signature(x="DBIConnection",value="ANY"),
                 function(x, name, value) {
                     dbWriteTable(x, name, value)
                     x
                 })

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,NULL-method
##' @export
setReplaceMethod("$", signature(x="DBIConnection", value="NULL"),
                 function(x, name, value) {
                     dbRemoveTable(x, name)
                     x
                 })

## setMethod("[", c("DBIConnection", "character", "missing"),
##           function(x, i, j, ...) {
##               lapply(i, function(table) dbReadTable(x, i),
##                      simplify=FALSE, USE.NAMES=TRUE)
##           })

## setMethod("[<-", c("DBIConnection", "character", "missing", "ANY"),
##           function(x, i, j, ..., value) {
##               ## Check?
##               for (table in i) {
##                   dbWriteTable(x, table, value, ...)
##               }
##           })

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


##' DBIDriver methods
##'
##' @name dbDisconnect
##' @docType methods
##' @rdname DBIDriver-methods
##' @aliases dbDisconnect,DBIDriver-method
##' @export
setMethod("dbDisconnect", "DBIDriver",
          function(conn, ...) {
              all(sapply(dbListConnections(conn),
                         dbDisconnect, ... = ...))
          })

##' Character methods
##'
##' @name dbDisconnect
##' @docType methods
##' @rdname character-methods
##' @aliases dbDisconnect,character-method
##' @export
setMethod("dbDisconnect", "character",
          function(conn) dbDisconnect(dbDriver(conn)))

