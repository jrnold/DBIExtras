##' @exportMethod dbClearResult
NULL


##' Methods for function \code{dbClearResult}
##'
##' @name dbClearResult-methods
##' @rdname dbClearResult-methods
##'
##' @aliases dbClearResult-method
##' @aliases dbClearResult,DBIConnection-method
##' @docType methods
##' @keywords methods
NULL

setMethod("dbClearResult", c(res="DBIConnection"),
          function(res) {
              for (i in dbListResults(res)) {
                  dbClearResults(i)
              }
          })

