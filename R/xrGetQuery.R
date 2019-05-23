#' Recuperation du resultat d'une requete dans une base XR.
#' 
#' @section Details: Cette fonction permet d'envoyer une requête à une connexion
#' sur une base XR et d'en récupérer le résultat.
#' 
#' @param conn une connexion à une base XR (cf \code{\link{xrConnect}})
#' @param query chaîne de caractères contenant la reqûete
#'
#' @return une data.frame avec le résultat de la requête ou 
#' 	une erreur si la requête n'a pas abouti.
#' 
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}}
#' @export
xrGetQuery <- function (conn, query) {

	url    <- URLencode(sprintf('%s%s', xrGetUrl(conn), query))
	result <- readLines(url, warn=FALSE)
	result <- fromJSON(result)

	# TODO: prevoir une mise en forme du resultat (on mettra probablement ici
	# toutes les correspondances)

	if('sites' %in% names(result)){
		result <- data.frame()
		if(conn[['version']] == 2){
		# TODO: il manque des champs cf correspondance commentées
			id IDENTIFIANT
			ref NSIT
			label ISIT
			class+area typologie
			latitude LATI
			longitude LONGI
			# NSIT_PUBLIC
			# NOM_COURT_SIT
			# AXE
			# CODE
			# LAMBERTX LAMBERTY --> par calcul ?
		}
	}else if(){
	}

	return( result )

}

#' Pre-formattage de l'URL de requete
#'
#' Pour un usage interne uniquement
#'
#' @param conn Un objet de type 'xr' (\code{\link{xrConnect}})
#' @return une chaîne de caractères correspondant à la base de l'URL à requêter
xrGetUrl <- function(conn){
	return(sprintf('http%s://%s:%s/dms-api/public/v1/',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']]))
}
