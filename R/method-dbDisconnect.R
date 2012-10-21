##' Methods for function \code{dbDisconnect}
##'
##' @name dbDisconnect-methods
##' @rdname dbDisconnect-methods
##' @docType methods
##' @keywords methods
##' @aliases dbDisconnect-methods
##' @aliases dbDisconnect,DBIDriver-method
##' @aliases dbDisconnect,character-method
setMethod("dbDisconnect", "DBIDriver",
          function(conn, ...) {
              all(sapply(dbListConnections(conn),
                         dbDisconnect, ... = ...))
          })

setMethod("dbDisconnect", "character",
          function(conn) dbDisconnect(dbDriver(conn)))

