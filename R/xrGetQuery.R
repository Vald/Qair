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

#' Listes des champs (Qair v2 et v3) par type de requête
#'
#' @param 
#' @export
xrListFields <- function(name=c('sites')){
	name <- match.arg(name)
	if(name == 'sites'){
		# TODO: il manque des champs cf correspondance commentées
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NSIT', 'ISIT', 'typologie', 'LATI', 'LONGI',
					 'class', 'area'
					 # 'NSIT_PUBLIC', 'NOM_COURT_SIT', 'AXE', 'CODE', 'LAMBERTX', 
					 # 'LAMBERTY', 'CLASSE_SITE'
					 ),
			nv3  = c('id', 'ref', 'label', 'typologie', 'latitude', 'longitude',
					 'class', 'area'),
			type = c('character()', 'character()', 'character()', 'numeric()',
					 'numeric()', 'character()', 'character()', 'character()'
					 # 'character()', 'character()', 'character()', 'numeric()',
					 # 'numeric()', 'numeric()', 'numeric()'
					 )
		  ))
	#}else if(name == ''){
	}
}


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
	osaf   <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)

	# récupération de la requete brute

	url    <- URLencode(sprintf('%s%s', xrGetUrl(conn), query))
	result <- httr::GET(url, httr::config(ssl_verifypeer=FALSE, ssl_verifyhost=FALSE))
	result <- jsonlite::fromJSON(httr::content(result, 'text'))

	# traitement du cas générique ---------------------------------------------

	type   <- names(result)[[1]]
	result <- result[[type]]
	fields <- xrListFields(type)

	if (is.null(names(result))){
		f        <- lapply(fields[['type']], function(x) eval(parse(text=x)))
		names(f) <- fields[['nv3']]
		result   <- do.call(data.frame, f)
	}

	# traitement des cas specifiques ------------------------------------------

	if(type == 'sites'){

		# ajout de champs recalculés
		result[['typologie']] <- paste(result[['class']], result[['area']])

	#}else if(type == ''){
	}

	# gestion si compatibilité v2 et fin --------------------------------------

	if(conn[['version']] == 2)
		names(result) <- fields[['nv2']][match(names(result), fields[['nv3']])]

	options(stringsAsFactors = osaf)
	return( result )

}

