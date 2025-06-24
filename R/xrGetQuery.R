#' Pre-formattage de l'URL de requete
#'
#' Pour un usage interne uniquement
#'
#' @param conn Un objet de type 'xr' (\code{\link{xrConnect}})
#' @param version booléen : faut-il retourner l'url pour récupérer la version ?
#'  FALSE par défaut.
#' @param authentification booléen : faut-il retourner l'url pour l'authentification ?
#'  FALSE par défaut.
#' @return une chaîne de caractères correspondant à la base de l'URL à requêter
xrGetUrl <- function(conn, version=FALSE, authentification=FALSE){
	if(authentification)
	return(sprintf('http%s://%s:%s/dms-api/authentification/login',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']])) else if(version)
	return(sprintf('http%s://%s:%s/dms-api/%s/version',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']],
				   if(conn[['logged']]) 'restricted' else 'public')) else
	return(sprintf('http%s://%s:%s/dms-api/%s/',
				   if(conn[['port']] == 8443) 's' else '',
				   conn[['host']], conn[['port']],
				   if(conn[['logged']]) 'restricted' else 'public'))
}

#' Fonction interne pour coller deux jeux d'ids en fonction d'un collapse
#'
#' Pour un usage interne uniquement
#'
#' @param ist Premier vecteur d'ids
#' @param idsites Second vecteur d'ids
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
								'equipments', 'trackMeasureEquipments',
								'aqiGroups', 'disclosedAQI')){
	name <- match.arg(name)
	if(name == 'sites'){
		# quand le json n'est pas un simple vecteur mais un dictionnaire en
		# cascade : utiliser '.' pour accéder à l'élément voulu (par exemple
		# environment:classTypeLabel)
		return(data.frame(
			nv2  = c('IDENTIFIANT','NOM_COURT_SIT','NSIT','ISIT',
					 'D_CREATION','D_ARRET',
					 'NSIT_PUBLIC','ISIT_LONG',
					 'address.commune.labelCommune','NINSEE', 
					 'LATI', 'LONGI', 'ALTI',
					 'AXE', 'CODE_POSTAL', 
					 'LAMBERTX', 'LAMBERTY',
					 'address.departement.labelDepartement','address.departement.id',
					 'DENSITE','NBRE_SOURCES',
					 'NB_VEHICULE','RAYON_ACTION',
					 'ZONE_IMPLANT','DER_LECT_QH',
					 'CLASSE_SITE','SITE_TYPE',
					 'TYPE_LIEU_ECHAN','TYPE_VOIE',
					 'typologie','environment.locationTypeLabel',
					 'environment.samplingPlaceTypeLabel','environment.laneTypeLabel',
					 'TYPE_SECTEUR','ZONE_ACTIVITE',
					 'sectors.typeSectorLabel','sectors.zoneOfActivityLabel'
					 ),
			nv3  = c('id','dbRowId','refSite','labelSite',
					 'startDate', 'stopDate',
					 'refSitePublic','labelSiteExtended',
					 'address.commune.labelCommune','address.commune.id', 
					 'address.latitude', 'address.longitude', 'address.altitude',
					 'address.street', 'address.postCode',
					 'address.mapCoordinateX', 'address.mapCoordinateY',
					 'address.department.labelDepartment','address.department.id',
					 'implantation.populationDensity','implantation.emissionSource',
					 'implantation.vehiclePerDay','implantation.radius',
					 'implantation.zoneDescription','implantation.lastCensusDate',
					 'environment.classType','environment.locationType',
					 'environment.samplingPlaceType','environment.laneType',
					 'environment.classTypeLabel','environment.locationTypeLabel',
					 'environment.samplingPlaceTypeLabel','environment.laneTypeLabel',
					 'sectors.typeSector','sectors.zoneOfActivity',
					 'sectors.typeSectorLabel','sectors.zoneOfActivityLabel'
					 ),
			type = c(rep('character()',4), 
					 'as.POSIXct(character())', 'as.POSIXct(character())',
					 'character()', 'character()',
					 'character()','numeric()',
					 'numeric()', 'numeric()',  'numeric()', 
					 'character()', 'character()',
					 'numeric()', 'numeric()',
					 'character()', 'character()',
					 'numeric()','numeric()',
					 'numeric()','numeric()',
					 'character()','as.POSIXct(character())',
					 'numeric()','numeric()',
					 'numeric()','numeric()',
					 'character()','character()',
					 'character()','character()',
					 'numeric()','numeric()',
					 'character()','character()'
					 )
		  ))
	}else if(name == 'measures'){
		return(data.frame(
			nv2  = c('IDENTIFIANT','NOM_COURT_MES','NOM_MES','TYPE_MESURE','TYPE_ACQ',
					 'compCode', 'codeP2',
					 'D_CREATION', 'D_ARRET','DERNIER_QH', 'D_VALIDATION',
					 'D_VALIDATION_ENV','D_ADVAL',
					 'id_site','NSIT','NSIT_PUBLIC','NOM_COURT_SIT',
					 'UNITE',
					 'CCHIM', 'NOPOL'),
			nv3  = c('id','dbRowId','labelMeas','measureType','acqType',
					 'compCode', 'codeP2',
					 'startDate', 'stopDate','lastDataDate','techValidationDate',
					 'envValidationDate','advalValidationDate',
					 'site.id','site.refSite','site.refSitePublic','site.dbRowId',
					 'unit.id',
					 'physical.tagPhy','physical.id'),
			type = c(rep('character()',5),
					 rep('character()',2),
					 rep('as.POSIXct(character())',6),
					 rep('character()',4),
					 'character()',
					 rep('character()',2))
		  ))
	}else if(name == 'campaigns'){
		return(data.frame(
			nv2  = c('NOM_COURT_CM','LIBELLE','COMMENTAIRE','DATEDEB','DATEFIN'),
			nv3  = c('id','label','comment','startDate','stopDate'),
			type = c(rep('character()',3),rep('as.POSIXct(character())',2))
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
			type = c('character()', 'character()', 'logical()')
		  ))
	}else if(name %in% c(
		'equipments','trackMeasureEquipments','aqiGroups', 'disclosedAQI')){
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

	# récupération de la requete brute ----------------------------------------

	url <- sprintf('%s%s', xrGetUrl(conn), query)
	url <- URLencode(url, repeated=TRUE)

	if(getOption('Xair.debug', FALSE)) message(url)

	nbattempt <- getOption('Xair.nbattempt', 100)
	result    <- list(status_code='notsent')
	i         <- 1
	while(result[['status_code']] != 200 && i <= nbattempt) {
		conf   <- httr::config(ssl_verifypeer=FALSE,ssl_verifyhost=FALSE)
		result <- try(httr::GET(url, conf), silent=TRUE)
		i      <- i+1

		if(inherits(result, 'try-error')) {
			if(getOption('Xair.debug', FALSE)) message('curl error, nouvel essai')
			result <- list(status_code='notsent')
		}
		if(result[['status_code']] != 200) Sys.sleep(0.1+sample(-50:50/1000, 1))
		if(getOption('Xair.debug', FALSE) && i > 2) message('attempt ', i)
	}

	result <- jsonlite::fromJSON(httr::content(result, 'text'))

	# traitement du cas générique ---------------------------------------------

	type   <- sub('^.*/', '', sub('\\?.*$', '', query))
	fields <- xrListFields(type)
	if(type == 'disclosedAQI') type <- 'calculatedIndex'
	result <- result[[type]]

	if(is.null(fields)) {
		options(stringsAsFactors = osaf)
		return( result )
	}

	if (is.null(names(result))){
		f        <- lapply(fields[['type']], function(x) eval(parse(text=x)))
		names(f) <- fields[['nv3']]
		result   <- do.call(data.frame, f)
	}

	# traitement des réponses quand il y a des champs complexes ---------------
	# (le json n'est pas juste un vecteur ...)
		# definition des noms des champs récursifs
		# affectation des valeurs aux champs en question
		# suppression des champs complexes

	flat <- function(x) {
		empty      <- sapply(x, is.data.frame) & sapply(x, length) == 0
		compounded <- !sapply(x, is.vector) & !sapply(x, inherits, 'POSIXct') & !empty
		res        <- x[!compounded & !empty]
		if(any(compounded)) {
			res <- c(list(res), lapply(x[compounded], flat))
			while(length(res) > 1) {
				names(res[[2]]) <- paste(names(res)[2], names(res[[2]]), sep='.')
				res[[1]] <- cbind(res[[1]], res[[2]])
				res[[2]] <- NULL
			}
			res <- res[[1]]
		}
		return(as.data.frame(res))
	}

	result <- flat(result)

	# traitement des cas specifiques ------------------------------------------

	#if(type == 'sites'){
	#}else if(type == ''){
	#}

	# gestion des dates -------------------------------------------------------

	champdates         <- fields[['nv3']][grep('POSIXct', fields[['type']])]
	champdates         <- intersect(champdates, names(result))
	result[champdates] <- lapply(result[champdates],
								 as.POSIXct, format='%Y-%m-%dT%H:%M:%SZ', 'UTC')

	# remplissage des colonnes absentes ---------------------------------------

	colabsentes <- setdiff(fields[['nv3']], names(result))
	result[colabsentes] <- NA
	result <- result[fields[['nv3']]]

	# gestion si compatibilité v2 et fin --------------------------------------

	if(!resv3 & conn[['version']] == 2)
		names(result) <- fields[['nv2']][
			match(names(result), gsub(':', '.', fields[['nv3']]))]

	options(stringsAsFactors = osaf)
	return( result )

}


#' Information sur la version d'XR (serveur et API)
#' 
#' @param conn une connexion à une base XR (cf \code{\link{xrConnect}})
#' @export
xrVersion <- function(conn) {
	url    <- xrGetUrl(conn, TRUE)

	if(getOption('Xair.debug', FALSE)) message(url)

	nbattempt <- getOption('Xair.nbattempt', 100)
	result    <- list(status_code='notsent')
	i         <- 1
	while(result[['status_code']] != 200 && i <= nbattempt) {
		conf   <- httr::config(ssl_verifypeer=FALSE,ssl_verifyhost=FALSE)
		result <- try(httr::GET(url, conf), silent=TRUE)
		i      <- i+1

		if(inherits(result, 'try-error')) {
			if(getOption('Xair.debug', FALSE)) message('curl error, nouvel essai')
			result <- list(status_code='notsent')
		}
		if(result[['status_code']] != 200) Sys.sleep(0.1+sample(-50:50/1000, 1))
		if(getOption('Xair.debug', FALSE) && i > 2) message('attempt ', i)
	}

	result <- jsonlite::fromJSON(httr::content(result, 'text'))
	return(result)
}
