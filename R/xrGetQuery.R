#' Pre-formattage de l'URL de requete
#'
#' Pour un usage interne uniquement
#'
#' @param conn Un objet de type 'xr' (\code{\link{xrConnect}})
#' @param auth Booléen : l'url à récupérer doit-elle être celle d'authentification ?
#'  FALSE par défaut.
#' @return une chaîne de caractères correspondant à la base de l'URL à requêter
xrGetUrl <- function(conn, auth=FALSE){
	if(auth){
		if(conn[['port']] != 8443)
			stop("pour l'authentification, le port doit être 8443")
		return(sprintf('https://%s:8443/dms-api/authentification/login',
					   conn[['host']]))

	} else return(sprintf('http%s://%s:%s/dms-api/%s/v1/',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']], conn[['access']]))
}

#' Fonction interne pour coller deux jeux d'ids en fonction d'un collapse
#'
#' Pour un usage interne uniquement
#'
#' @param x Premier vecteur d'ids
#' @param y Second vecteur d'ids
#' @param collapse AND ou OR
#' @return les ids fusionner selon collapse
collapseIds <- function(ist, idsites, collapse=c('AND', 'OR')){
	collapse <- match.arg(collapse)
	if(is.null(idsites)){
		idsites <- ist
	}else if (collapse == 'AND'){
		idsites <- intersect(ist, idsites)
	}else
		idsites <- unique(c(ist, idsites))
	return(idsites)
}

#' Listes des champs (Qair v2 et v3) par type de requête
#'
#' @param 
#' @export
xrListFields <- function(name=c('sites')){
	name <- match.arg(name)
	if(name == 'sites'){
		# FIXME: il manque des champs cf correspondance commentées
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NSIT', 'ISIT', 'typologie', 'LATI', 'LONGI',
					 'area', 'type'),
			# 'AXE', 'CODE_POSTAL', 'labelCommune', 'D_CREATION', 'D_ARRET'
			# en plus 'NSIT_PUBLIC', 'NOM_COURT_SIT', 'LAMBERTX', 'LAMBERTY',
			# 'CLASSE_SITE', 'CODE', 'ISIT', 'ISIT_LONG'
			nv3  = c('id', 'ref', 'label', 'class', 'latitude', 'longitude',
					 'area', 'type'),
			#, 'street', 'postCode', 'labelCommune', 'startDate', 'stopDate'),
			type = c('character()', 'character()', 'character()', 'numeric()',
					 'numeric()', 'character()', 'character()', 'numeric()')
		  ))
	}else if(name == 'measures'){
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NOM_MES', 'id_site', 'UNITE', 'phy_name'),
			# 'phy_comp_code', 'NOPOL', 'campaigns', 'DERNIER_QH', 'D_VALIDATION',
			# 'D_VALIDATION_ENV', 'D_CREATION', 'D_ARRET'
			# en plus 'NSIT', 'NOM_COURT_SIT', 'CMET'
			nv3  = c('id', 'label', 'id_site', 'unite', 'phy_name'),
			# 'phy_comp_code', 'id_phy', 'campaigns','lastDataDate', 
			# 'techValidationDate', 'envValidationDate', 'startDate', 
			# 'stopDate'),
			type = c('character()', 'character()', 'character()', 'character()',
					 'character()')
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
#' @param resv3 Booléen : faut-il forcer le retour au format v3 ? Par défaut
#'  non, donc si la connexion à XR est en v2, les noms correspondront à la v2.
#'  Seul le résultat est affecté : si la connexion est en v2, les noms des
#'  champs de recherches doivent être spécifiés en v2. Son utilisation devrait
#'  être réservée à des fins de développement.
#'
#' @return une data.frame avec le résultat de la requête ou 
#' 	une erreur si la requête n'a pas abouti.
#' 
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}}
#' @export
xrGetQuery <- function (conn, query, resv3=FALSE) {
	osaf   <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)

	# récupération de la requete brute

	url    <- URLencode(sprintf('%s%s', xrGetUrl(conn), query))
	result <- httr::POST(url, httr::config(ssl_verifypeer=FALSE, ssl_verifyhost=FALSE))
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
		#aucun pour l'instant
	#}else if(type == ''){
	}

	# gestion si compatibilité v2 et fin --------------------------------------

	if(!resv3 & conn[['version']] == 2)
		names(result) <- fields[['nv2']][match(names(result), fields[['nv3']])]

	options(stringsAsFactors = osaf)
	return( result )

}

