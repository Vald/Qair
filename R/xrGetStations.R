#' Recuperation des stations de mesures definies dans une base XR
#'
#' La fonction permet de lister les stations existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param mesures chaînes de caractères correspondant aux mesures à rapatrier
#' 	(optionnel) (utilisé via la fonction \code{\link{xrGetMesures}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetMesures Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetMesures
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 'N2_VER', search.fields = 'IDENTIFIANT') ...}
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#' @param resv3 Booléen : faut-il forcer le retour au format v3 ? Par défaut
#'  non, donc si la connexion à XR est en v2, les noms correspondront à la v2.
#'  Seul le résultat est affecté : si la connexion est en v2, les noms des
#'  champs de recherches doivent être spécifiés en v2. Son utilisation devrait
#'  être réservée à des fins de développement.
#' @param validOnly (v3) La recherche doit-elle porter uniquement sur les
#'  stations ouvertes ?
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les stations trouvées.
#' @export
xrGetStations <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, reseaux = NULL, fields = NULL, mesures = NULL,
			  collapse=c('AND', 'OR'), exact=FALSE, resv3=FALSE, validOnly=FALSE){#,
			  #startDate=NULL, stopDate=NULL) {
	osaf     <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)
	collapse <- match.arg(collapse)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/sites?showDbRowIds=true&')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('sites')
	if(is.null(search.fields)){
		search.fields <- xrfields[['nv3']][1:2]
		message("Champs disponibles pour la recherche : ",
				paste(collapse=', ', xrfields[[nv]]),
				"\nPar défaut : ",
				paste(collapse=', ', xrfields[[nv]][1:2]))
	}else{
		search.fields <- as.character(match.arg(search.fields, xrfields[[nv]], TRUE))
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des sites sur la base de search.fields, de campagne et de reseaux 
	# puis recherche de mesure si != NULL et récup. de id site
	# puis refiltre si search.fields contient autre chose que juste id
	# --> agregation des différents tests en fonction de collapse...

	idsites <- NULL

	# recherche sur search.fields ---------------------------------------------
	# si on a que id ou dbRowId comme champ de recherche, on utilise directement
	# l'API d'XR, sinon on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern))
	if(all(search.fields %in% c('id', 'dbRowId'))){
		if(!exact)
			query <- paste0('%', pattern, '%', collapse=',') else
			query <- paste0(pattern, collapse=',')
		if('id' %in% search.fields){
			query   <- paste0(bquery, 'sites=', query)
			ist     <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idsites <- collapseIds(ist, idsites, 'OR')
		}
		if('dbRowId' %in% search.fields){
			query   <- paste0(bquery, 'dbRowIdOfSites=', query)
			ist     <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idsites <- collapseIds(ist, idsites, 'OR')
		}
	} else {
		# sinon recherche sur la base de toutes les stations

		# TODO: créer une fonction qui fait la recherche pour tous ...
		all.stations <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.stations[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.stations[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.stations[[sf]])
				}}
			ist     <- all.stations[['id']][unique(unlist(selection))]
			idsites <- collapseIds(ist, idsites, 'OR')
		}
	}

	# récupération d'idsite sur la base de mesures ----------------------------

	if (!is.null (mesures) ) {
		if( !is.list(mesures) )
			mesures <- xrGetMesures(conn, pattern = mesures, resv3=TRUE) else{
			mesures[['resv3']] <- TRUE
			mesures <- do.call(xrGetMesures, c(list(conn=conn), mesures))
			}
		ist     <- unique(mesures[['site.id']])
		idsites <- collapseIds(ist, idsites, collapse)
	}

	# récupération d'idcampaignes ---------------------------------------------

	if (!is.null (campagnes) ) {
		if( !is.list(campagnes) )
			campagnes <- xrGetCampagnes(conn, pattern = campagnes, resv3=TRUE) else{
			campagnes[['resv3']] <- TRUE
			campagnes <- do.call(xrGetCampagnes, c(list(conn=conn), campagnes))
			}
		query   <- paste0(campagnes[['id']], collapse=',')
		query   <- paste0(bquery, 'campaigns=', query)
		ist     <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idsites <- collapseIds(ist, idsites, collapse)
	}

	# récupération d'idgroups -------------------------------------------------

	if (!is.null (reseaux) ) {
		if( !is.list(reseaux) )
			reseaux <- xrGetReseaux(conn, pattern = reseaux, resv3=TRUE) else{
			reseaux[['resv3']] <- TRUE
			reseaux <- do.call(xrGetReseaux, c(list(conn=conn), reseaux))
			}
		query   <- paste0(reseaux[['id']], collapse=',')
		query   <- paste0(bquery, 'groups=', query)
		ist     <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idsites <- collapseIds(ist, idsites, collapse)
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(c(pattern, campagnes, reseaux, mesures)) & length(idsites) == 0)
		idsites <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	query <- sprintf('%svalidOnly=%s', query, if(validOnly) 'TRUE' else 'FALSE')
	if(!is.null(idsites))
		query <- sprintf('%s&sites=%s', query, paste(idsites, collapse=','))

	# TODO:  ajouter filtre stopDate, startDate

	stations <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(stations) <- xrfields[['nv2']][match(names(stations), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que stations ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(stations)))
		stations[setdiff(fields, names(stations))] <- NA

	options(stringsAsFactors = osaf)
	return(stations[fields])
}


