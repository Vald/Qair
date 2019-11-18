#' Pre-formattage de l'URL de requete
#'
#' Pour un usage interne uniquement
#'
#' @param conn Un objet de type 'xr' (\code{\link{xrConnect}})
#' @param version booléen : faut-il retourner l'url pour récupérer la version ?
#'  FALSE par défaut.
#' @return une chaîne de caractères correspondant à la base de l'URL à requêter
xrGetUrl <- function(conn, version=FALSE){
	if(version)
	return(sprintf('http%s://%s:%s/dms-api/version/',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']])) else
	return(sprintf('http%s://%s:%s/dms-api/public/v1/',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']]))
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
	if(is.null(idsites) || length(idsites) == 0){
		idsites <- ist
	}else if (collapse == 'AND'){
		idsites <- intersect(ist, idsites)
	}else
		idsites <- unique(c(ist, idsites))
	return(idsites)
}

#' Listes des champs (Qair v2 et v3) par type de requête
#'
#' @param name nom du type de données pour lequel on veut la correspondance
#'  des noms de champs entre Qair v2 et l'API (Qair v3 donc)
#' @export
xrListFields <- function(name=c('sites' ,'measures', 'campaigns', 'physicals',
								'measure-groups', 'data',
								'equipments', 'trackMeasureEquipments')){
	name <- match.arg(name)
	if(name == 'sites'){
		# FIXME:VLAD modifier/intégrer les fonctions de incertR
		# FIXME:ISEO il manque des champs cf correspondance commentées
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NSIT', 'ISIT', 'typologie', 'LATI', 'LONGI',
					 'area', 'type', 'D_CREATION', 'D_ARRET', 'labelCommune', 
					 'AXE', 'CODE_POSTAL', 'ALTI'),
			# en plus 'NSIT_PUBLIC', 'NOM_COURT_SIT', 'ISIT_LONG',
			# 'LAMBERTX', 'LAMBERTY',
			# 'CLASSE_SITE', en fait non
			# 'CODE' euh non ... (Qu'est-ce ?)
			nv3  = c('id', 'ref', 'label', 'class', 'latitude', 'longitude',
					 'area', 'type', 'startDate', 'stopDate', 'labelCommune',
					 'street', 'postCode', 'elevation'),
			type = c('character()', 'character()', 'character()', 'numeric()',
					 'numeric()', 'character()', 'character()', 'numeric()',
					 'character()', 'character()', 'character()', 'character()',
					 'numeric()', 'numeric()')
		  ))
	}else if(name == 'measures'){
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NOM_MES', 'id_site', 'UNITE', 'phy_name',
					 'phy_comp_code', 'NOPOL', 'DERNIER_QH', 'D_VALIDATION',
					 'D_VALIDATION_ENV', 'D_CREATION', 'D_ARRET', 'campaigns'),
			# en plus 'NSIT', 'CMET', 'TYPE_ACQ'
			# 'NOM_COURT_SIT' sinon récupérable
			nv3  = c('id', 'label', 'id_site', 'unit', 'phy_name',
					 'phy_comp_code', 'idPhy', 'lastDataDate', 'techValidationDate',
					 'envValidationDate', 'startDate', 'stopDate', 'campaigns'),
			type = c('character()', 'character()', 'character()', 'character()',
					 'character()', 'character()', 'character()', 'character()',
					 'character()', 'character()', 'character()', 'character()',
					 'character()')
		  ))
	}else if(name == 'campaigns'){
		return(data.frame(
			nv2  = c('NOM_COURT_CM', 'DATEDEB', 'DATEFIN'),
			# en plus 'LIBELLE'
			nv3  = c('id', 'startDate', 'stopDate'),
			type = c('character()', 'character()', 'character()')
		  ))
	}else if(name == 'physicals'){
		return(data.frame(
			nv2  = c('NOPOL', 'CCHIM', 'NCON'),
			nv3  = c('id', 'chemicalSymbol', 'label'),
			type = c('character()', 'character()', 'character()')
		  ))
	}else if(name == 'measure-groups'){
		return(data.frame(
			nv2  = c('NOM_COURT_RES', 'NOM_RES', 'FLAG_RESEAURES'),
			nv3  = c('id', 'label', 'isNetworkGroup'),
			type = c('character()', 'character()', 'numeric()')
		  ))
	}else if(name %in% c('equipments', 'trackMeasureEquipments')){
		return(NULL)
#	}else if(name == ''){
#		return(data.frame(
#			nv2  = c(),
#			nv3  = c(),
#			type = c()
#		  ))
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

	# FIXME: test les timeout (cas ou trop de connexions simultanées, le serveur
	# ne répond pas --> ajouter un parametre nb tentatives)

	# récupération de la requete brute ----------------------------------------

	url    <- sprintf('%s%s', xrGetUrl(conn), query)
	if(sub('\\?.*$', '', query) == 'equipments') url <- URLencode(url)

	if(getOption('Xair.debug', FALSE)) message(url)
	result <- httr::GET(url, httr::config(ssl_verifypeer=FALSE, ssl_verifyhost=FALSE))
	result <- jsonlite::fromJSON(httr::content(result, 'text'))

	# traitement du cas générique ---------------------------------------------

	type   <- names(result)[[1]]
	result <- result[[type]]
	fields <- xrListFields(type)

	if(is.null(fields)) {
		options(stringsAsFactors = osaf)
		return( result )
	}

	# TODO:VLAD mettre des types POSXIct pour les dates et traiter le cas ici
	# TODO:VLAD mettre des types boolean pour le isNetworkGroup 
	if (is.null(names(result))){
		f        <- lapply(fields[['type']], function(x) eval(parse(text=x)))
		names(f) <- fields[['nv3']]
		result   <- do.call(data.frame, f)
	}

	# traitement des cas specifiques ------------------------------------------

	#if(type == 'sites'){
	#}else if(type == ''){
	#}

	# gestion si compatibilité v2 et fin --------------------------------------

	if(!resv3 & conn[['version']] == 2)
		names(result) <- fields[['nv2']][match(names(result), fields[['nv3']])]

	options(stringsAsFactors = osaf)
	return( result )

}

