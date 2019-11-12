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
								'measure-groups', 'data')){
	name <- match.arg(name)
	if(name == 'sites'){
		# FIXME:VLAD modifier/intégrer les fonctions de incertR
		# FIXME:ISEO il manque des champs cf correspondance commentées
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NSIT', 'ISIT', 'typologie', 'LATI', 'LONGI',
					 'area', 'type', 'D_CREATION', 'D_ARRET', 'labelCommune', 
					 'AXE', 'CODE_POSTAL'),
			# en plus 'NSIT_PUBLIC', 'NOM_COURT_SIT', 'LAMBERTX', 'LAMBERTY',
			# 'CLASSE_SITE', 'CODE', 'ISIT', 'ISIT_LONG'
			nv3  = c('id', 'ref', 'label', 'class', 'latitude', 'longitude',
					 'area', 'type', 'startDate', 'stopDate', 'labelCommune',
					 'street', 'postCode'),
			type = c('character()', 'character()', 'character()', 'numeric()',
					 'numeric()', 'character()', 'character()', 'numeric()',
					 'character()', 'character()', 'character()', 'character()',
					 'numeric()')
		  ))
	}else if(name == 'measures'){
		return(data.frame(
			nv2  = c('IDENTIFIANT', 'NOM_MES', 'id_site', 'UNITE', 'phy_name',
					 'phy_comp_code', 'NOPOL', 'DERNIER_QH', 'D_VALIDATION',
					 'D_VALIDATION_ENV', 'D_CREATION', 'D_ARRET', 'campaigns'),
			# en plus 'NSIT', 'NOM_COURT_SIT', 'CMET'
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
			# , 'CODE_COMPLEMENTAIRE', 'TYPE_AGR', 'FCON', 'ECHELLE_MIN',
			# 'ECHELLE_MAX', 'TIMESTAMP', 'CCHIM_COURT', 'LIB_ECHELLE',
			# 'ECHELLE2_MIN', 'ECHELLE2_MAX', 'LIB_ECHELLE2', 'ECHELLE3_MIN',
			# 'ECHELLE3_MAX', 'LIB_ECHELLE3', 'ECHELLE4_MIN', 'ECHELLE4_MAX',
			# 'LIB_ECHELLE4', 'ECHELLE5_MIN', 'ECHELLE5_MAX', 'LIB_ECHELLE5',
			# 'NO_CHRONO', 'SEUIL_30MIN', 'SEUIL_10MIN', 'SEUIL_24H', 'SEUIL_MAX',
			# 'SEUIL_MIN', 'SEUIL_60MIN'),
			nv3  = c('id', 'chemicalSymbol', 'label'),
			type = c('character()', 'character()', 'character()')
		  ))
	}else if(name == 'measure-groups'){
		return(data.frame(
			nv2  = c('NOM_COURT_RES', 'NOM_RES', 'FLAG_RESEAURES'),
			# , 'NRESSURV', 'R_CREATEUR', 'R_TYPE',
			# 'TYPEVALIDATION', 'TYPESUIVICAL', 'TYPESAISIEIMPORT', 'TYPESCRUTATION',
			# 'TYPEEXPLOITATION', 'TYPECONSULTATION', 'TYPEEXPORT', 'TYPEADEME',
			# 'TYPEALERTE', 'TYPEPERMANENT', 'TYPESPECIFIQUE1', 'TYPESPECIFIQUE2',
			# 'TYPESPECIFIQUE3', 'DIFFUSIONPUBLIQUE', 'NCARTE', 'NOM_COURT_ALERTE',
			# 'MAIL', 'STEP', 'D_CREATION', 'MOT_PASSE', 'TIMESTAMP',
			# 'FLAG_CONTROL_R', 'EVT_ALR_DIR', 'SORTIE_ALR_DIR', 'EVT_ALR_INTER',
			# 'SORTIE_ALR_INTER', 'EVT_ALR_FINALE', 'SORTIE_ALR_FINALE',
			# 'APP_DEP_SEUIL_MAX', 'DISP_DEP_SEUIL_MAX', 'SORTIE_SEUIL_MAX',
			# 'EVT_APP_ARRET_4H', 'EVT_DISP_ARRET_4H', 'SORTIE_ARRET_4H',
			# 'EVT_APP_ARRET_JOUR', 'EVT_DISP_ARRET_JOUR', 'SORTIE_ARRET_JOUR',
			# 'EVT_APP_ARRET_T2S', 'EVT_DISP_ARRET_T2S', 'SORTIE_ARRET_T2S',
			# 'FLAG_CONTROL_4H', 'FLAG_CONTROL_60H', 'FLAG_INVAL_JOUR', 'FLAG_INVAL_AN',
			# 'FLAG_CONTROL_MAX', 'SEUIL_ALR_INTER', 'FLAG_10H_UNAVAIL_CONT',
			# 'FLAG_60H_YEAR_UNAVAIL', 'FLAG_T2S', 'D_START_MONTH_DAY', 'D_START_CTRL',
			# 'EVT_APP_LAST_AVG_INV', 'EVT_DISP_LAST_AVG_INV', 'SORTIE_LAST_AVG_INV',
			# 'EVT_APP_STOP_FEED', 'EVT_DISP_STOP_FEED', 'SORTIE_STOP_FEED',
			# 'EVT_APP_60H_OVERRUN', 'EVT_DISP_60H_OVERRUN', 'SORTIE_60H_OVERRUN',
			# 'EVT_APP_60H_YEAR_UNAVAIL', 'EVT_DISP_60H_YEAR_UNAVAIL', 'SORTIE_60H_YEAR_UNAVAIL',
			# 'EVT_APP_10H_UNAVAIL_CONT', 'EVT_DISP_10H_UNAVAIL_CONT', 'SORTIE_10H_UNAVAIL_CONT',
			# 'EVT_APP_T2S_LAST', 'EVT_DISP_T2S_LAST', 'SORTIE_T2S_LAST',
			# 'EVT_APP_LIMIT_MAX_LAST', 'EVT_DISP_LIMIT_MAX_LAST', 'SORTIE_LIMIT_MAX_LAST',
			# 'EVT_APP_10_DAYS_INVALID', 'EVT_DISP_10_DAYS_INVALID', 'SORTIE_10_DAYS_INVALID',
			# 'EVT_DISP_DIR', 'EVT_DISP_INTER', 'EVT_DISP_FINALE', 'EVT_APP_4H_OVERRUN_CONT',
			# 'EVT_DISP_4H_OVERRUN_CONT', 'SORTIE_4H_OVERRUN_CONT'
			nv3  = c('id', 'label', 'isNetworkGroup'),
			type = c('character()', 'character()', 'numeric()')
		  ))
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
	if(getOption('Xair.debug', FALSE)) message(url)
	result <- httr::GET(url, httr::config(ssl_verifypeer=FALSE, ssl_verifyhost=FALSE))
	result <- jsonlite::fromJSON(httr::content(result, 'text'))

	# traitement du cas générique ---------------------------------------------

	type   <- names(result)[[1]]
	result <- result[[type]]
	fields <- xrListFields(type)

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

