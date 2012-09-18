## Code taken from filehash
## Copyright 2012 Roger D. Peng
setGeneric("dbLoad", function(db, ...) standardGeneric("dbLoad"))
setGeneric("dbLazyLoad", function(db, ...) standardGeneric("dbLazyLoad"))

setMethod("dbLoad", "DBIConnection",
          function(db, env = parent.frame(2), tables = NULL, ...) {
              if(is.null(tables))
                  tables <- names(db)
              else if(!is.character(tables))
                  stop("'tables' should be a character vector")
              active <- sapply(tables, function(k) {
                  exists(k, env, inherits = FALSE)
              })
              if(any(active)) {
                  warning("tables with active/regular bindings ignored: ",
                          paste(sQuote(tables[active]), collapse = ", "))
                  tables <- tables[!active]
              }
              make.f <- function(k) {
                  key <- k
                  function(value) {
                      if(!missing(value)) {
                          db[[key]] <- value
                          invisible(value)
                      }
                      else {
                          obj <- db[[key]]
                          obj
                      }
                  }
              }
              for(k in tables)
                  makeActiveBinding(k, make.f(k), env)
              invisible(tables)
          })

setMethod("dbLazyLoad", "DBIConnection",
          function(db, env = parent.frame(2), tables = NULL, ...) {
              if(is.null(tables))
                  tables <- names(db)
              else if(!is.character(tables))
                  stop("'tables' should be a character vector")

              wrap <- function(x, env) {
                  key <- x
                  delayedAssign(x, db[[key]], environment(), env)
              }
              for(k in tables)
                  wrap(k, env)
              invisible(tables)
          })

## Load active bindings into an environment and return the environment
db2env <- function(con) {
    env <- new.env(hash = TRUE)
    dbLoad(con, env)
    env
}
