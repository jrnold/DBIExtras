##' @export
setGeneric("length", base::length)
##' @export
setGeneric("names", base::names)

##' @exportMethod [[
##' @exportMethod [
##' @exportMethod $


##' DBIConnection Generics
##'
##' @name [[
##' @aliases [[,DBIConnection-method,character-method,missing-method
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases [[,DBIConnection,character,missing-method
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
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "ANY"),
          function(x, i, j, ..., value) {
              dbWriteTable(x, i, value, ...)
              x
          })

##' @name [[
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases [[<-,DBIConnection,character,missing,NULL-method
setReplaceMethod("[[", c("DBIConnection", "character", "missing", "NULL"),
          function(x, i, j, ..., value) {
              dbRemoveTable(x, i, ...)
              x
          })

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $,DBIConnection-method
setMethod("$", signature(x="DBIConnection"),
          function(x, name) dbReadTable(x, name))

##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,ANY-method
setReplaceMethod("$", signature(x="DBIConnection",value="ANY"),
                 function(x, name, value) {
                     dbWriteTable(x, name, value)
                     x
                 })
##' @name $
##' @docType methods
##' @rdname DBIConnection-methods
##' @aliases $<-,DBIConnection,NULL-method
setReplaceMethod("$", signature(x="DBIConnection", value="NULL"),
                 function(x, name, value) {
                     dbRemoveTable(x, name)
                     x
                 })

################################33
##' @export
setMethod("[", c("DBIConnection", "character", "missing"),
          function(x, i, j, ...) {
              structure(lapply(i, function(table) dbReadTable(x, i)),
                        names=i)
              x
          })

##' @export
setReplaceMethod("[", c("DBIConnection", "character", "missing", "ANY"),
          function(x, i, j, ..., value) {
              for (idx in seq_along(i)) {
                  dbWriteTable(x, i[idx], value, ...)
              }
              x
          })

##' @export
setReplaceMethod("[", c("DBIConnection", "character", "missing", "data.frame"),
          function(x, i, j, ..., value) {
              for (idx in seq_along(i)) {
                  dbWriteTable(x, i[idx], value, ...)
              }
              x
          })

##' @export
setReplaceMethod("[", c("DBIConnection", "character", "missing", "list"),
          function(x, i, j, ..., value) {
              if (length(i) != length(value)) {
                  stop("length(i) != length(value)")
              }
              for (idx in seq_along(i)) {
                  dbWriteTable(x, i[idx], value[idx], ...)
              }
              x
          })

##' @export
setReplaceMethod("[", c("DBIConnection", "character", "missing", "NULL"),
          function(x, i, j, ..., value) {
              for (table in i) {
                  dbDeleteTable(x, table, ...)
              }
              x
          })

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

