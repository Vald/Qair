#' Recuperation des mesures definies dans une base XR
#'
#' La fonction permet de lister les mesures existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @param validOnly (v3) La recherche doit-elle porter uniquement sur les
#'  mesures ouvertes ?
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les mesures trouvées.
xrGetMesures <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, reseaux = NULL, stations=NULL, polluants = NULL,
			  fields = NULL,
			  collapse = c('AND', 'OR'), exact=FALSE, resv3=FALSE, validOnly=FALSE){#,
			  #startDate=NULL, stopDate=NULL) {}
	collapse <- match.arg(collapse)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/measures?showDbRowIds=true&')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('measures')
	if(is.null(search.fields)){
		search.fields <- xrfields[['nv3']][1:2]
		message("Champs disponibles pour la recherche : ",
				paste(collapse=', ', xrfields[[nv]]),
				"\nPar défaut : ",
				paste(collapse=', ', xrfields[[nv]][1:2]),
				'\n\n')
	}else{
		search.fields <- match.arg(search.fields, xrfields[[nv]], TRUE)
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des mesures sur la base de search.fields, de campagne, de reseaux 
	# de stations et de polluants
	# puis refiltre si search.fields contient autre chose que juste id/NJOM_COUR_MEs
	# --> agregation des différents tests en fonction de collapse...

	idmesures <- NULL

	# recherche sur search.fields ---------------------------------------------
	# si on a que id ou NOM_COURT_MES??? comme champ de recherche, on utilise directement
	# l'API d'XR, sinon on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	# FIXME:ISEO vérifier au moment de l'jout de NOM_COURT_MES que la recherche en %
	# fonctionne. Sino on se reporte sur la version globale
	#if(all(search.fields %in% c('id', 'NOM_COURT_MES???'))){}
	if(!is.null(pattern))
	#if(all(search.fields %in% 'id')) {}
	if(FALSE){
		# recherche sur id / IDENTIFIANT / measures
		#  sur ? / NOM_COURT_MES / ?

		if(!exact)
			query <- paste0('%', pattern, '%', collapse=',') else
			query <- paste0(pattern, collapse=',')
		if('id' %in% search.fields){
			query     <- paste0(bquery, 'measures=', query)
			ist       <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idmesures <- collapseIds(ist, idmesures, 'OR')
		}
		if('NOM_COURT_MES???' %in% search.fields){
			# FIXME:ISEO à laisser là si champ de recherche spécifique dans API
			# sinon à supprimer et à laisser le cas suivant prendre la main
			query     <- paste0(bquery, 'NOM_COURT_MES???=', query)
			ist       <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idmesures <- collapseIds(ist, idmesures, 'OR')
		}
	} else {
		all.mesures <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.mesures[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.mesures[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.mesures[[sf]])
				}}
			ist       <- all.mesures[['id']][unique(unlist(selection))]
			idmesures <- collapseIds(ist, idmesures, 'OR')
		}
	}

	# recherche sur campagnes -------------------------------------------------

	if (!is.null (campagnes) ) {
		if( !is.list(campagnes) )
			campagnes <- xrGetCampagnes(conn, pattern = campagnes, resv3=TRUE) else{
			campagnes[['resv3']] <- TRUE
			campagnes <- do.call(xrGetCampagnes, c(list(conn=conn), campagnes))
			}
		query     <- paste0(campagnes[['id']], collapse=',')
		query     <- paste0(bquery, 'campaigns=', query)
		ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idmesures <- collapseIds(ist, idmesures, collapse)
	}

	# recherche sur reseaux ---------------------------------------------------

	if (!is.null (reseaux) ) {
		if( !is.list(reseaux) )
			reseaux <- xrGetReseaux(conn, pattern = reseaux, resv3=TRUE) else{
			reseaux[['resv3']] <- TRUE
			reseaux <- do.call(xrGetReseaux, c(list(conn=conn), reseaux))
			}
		query     <- paste0(reseaux[['id']], collapse=',')
		query     <- paste0(bquery, 'groups=', query)
		ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idmesures <- collapseIds(ist, idmesures, collapse)
	}

	# recherche sur stations --------------------------------------------------

	if (!is.null (stations) ) {
		if( !is.list(stations) )
			stations <- xrGetStations(conn, pattern = stations, resv3=TRUE) else{
			stations[['resv3']] <- TRUE
			stations <- do.call(xrGetStations, c(list(conn=conn), stations))
			}
		query     <- paste0(stations[['refSite']], collapse=',')
		query     <- paste0(bquery, 'refSites=', query)
		ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idmesures <- collapseIds(ist, idmesures, collapse)
	}

	# recherche sur polluants -------------------------------------------------

	if (!is.null (polluants) ) {
		if( !is.list(polluants) )
			polluants <- xrGetPolluants(conn, pattern = polluants, resv3=TRUE) else{
			polluants[['resv3']] <- TRUE
			polluants <- do.call(xrGetPolluants, c(list(conn=conn), polluants))
			}
		query     <- paste0(polluants[['id']], collapse=',')
		query     <- paste0(bquery, 'physicals=', query)
		ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
		idmesures <- collapseIds(ist, idmesures, collapse)
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(c(pattern, campagnes, reseaux, stations, polluants))
	   & length(idmesures) == 0) idmesures <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	query <- sprintf('%svalidOnly=%s', query, if(validOnly) 'TRUE' else 'FALSE')
	if(!is.null(idmesures))
		query <- sprintf('%s&measures=%s', query, paste(idmesures, collapse=','))

	# TODO:  ajouter filtre stopDate, startDate

	mesures <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(mesures) <- xrfields[['nv2']][match(names(mesures), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else 
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que mesures ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(mesures)))
		mesures[setdiff(fields, names(mesures))] <- NA

	return(mesures[fields])

}

