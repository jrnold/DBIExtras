##' @exportMethod show
NULL

##' Methods for function \code{show}
##' 
##' @name show
##' @rdname DBIConnection-methods
##' @docType methods
##' @keywords methods
##' @aliases show-methods
##' @aliases show,DBIConnection-method
##' @aliases show,DBIConnection-method
NULL

setMethod("names", "DBIConnection",
          function(x) dbListTables(x))

setMethod("show", "DBIConnection",
          function(object) str(object))

