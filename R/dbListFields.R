#' extends dbListFields for RODBC objects
#' @inheritParams DBI::dbListFields
#' @param \dots pour compatibiltié avec la méthode générique
#' @seealso \code{\link[DBI]{dbListFields}}, \code{\link[RODBC]{sqlColumns}}
#' @aliases dbListFields,RODBC,character-method
dbListFields <- function(conn, name, ...) UseMethod('dbListFields')
#' @rdname dbListFields
setMethod('dbListFields', signature ('RODBC', 'character'),
	  function(conn, name, ...) unique(sqlColumns(conn, name)$COLUMN_NAME))

#' @rdname dbListFields
#' @aliases dbDisconnect
#' @aliases dbDisconnect,RODBC-method
dbDisconnect <- function(conn, ...) UseMethod('dbListFields')
#' @rdname dbListFields
setMethod('dbDisconnect', signature ('RODBC'),
	  function(conn, ...) odbcClose( conn ))
