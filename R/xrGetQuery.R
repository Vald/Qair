#' Recuperation du resultat d'une requete dans une base XR.
#' 
#' @section Details: Cette fonction permet d'envoyer une requête à une connection
#' sur une base XR et d'en récupérer le résultat.
#' Sous windows, le pilote utilisé est ODBC (nécessite d'avoir
#' installer le paquet \code{\link[RODBC]{RODBC}}), sous linux JDBC
#' (nécessite d'avoir installer le paquet \code{\link[RJDBC]{JDBC}}).
#' 
#' @param conn une connection à une base XR (cf \code{\link{xrConnect}})
#' @param query chaîne de caractères contenant la reqûete
#'
#' @return une data.frame avec le résultat de la requête ou 
#' 	une erreur si la requête n'a pas abouti.
#' 
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}}
xrGetQuery <- function (conn, query) {
	if(.Platform$OS.type=="windows") {
		sqlQuery(conn, query)
	} else if(.Platform$OS.type=="unix") {
		dbGetQuery(conn, query)
	} else stop ('Unrecognized platform oO.')

}

