##' @exportMethod length
NULL

##' @name length
##' @docType methods
##' @rdname DBIConnection-methods
##' @docType methods
##' @aliases length,DBIConnection-method
##' @export
setMethod("length", "DBIConnection",
          function(x) length(dbListTables(x)))
