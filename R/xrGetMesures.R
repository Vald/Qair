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
			  collapse = c('AND', 'OR'), exact=FALSE, resv3=FALSE, validOnly=FALSE,
			  silent){#,
			  #startDate=NULL, stopDate=NULL) {}
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)
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
		if(!silent) message("Champs disponibles pour la recherche (mesures) : \n",
				paste(collapse=', ', xrfields[[nv]]),
				"\n\nPar défaut : ",
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
	aucunecorrespondance <- FALSE

	# recherche sur search.fields ---------------------------------------------
	# si on a que id ou dbRowId comme champ de recherche, on utilise directement
	# l'API d'XR, sinon on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern))
	if(all(search.fields %in% c('id', 'dbRowId'))) {
		if(!exact)
			ids <- paste0('%', pattern, '%', collapse=',') else
			ids <- paste0(pattern, collapse=',')
		if('id' %in% search.fields){
			query     <- paste0(bquery, 'measures=', ids)
			ist       <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idmesures <- collapseIds(ist, idmesures, 'OR')
		}
		if('dbRowId' %in% search.fields){
			query     <- paste0(bquery, 'dbRowIdOfMeasures=', ids)
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

	if (!is.null (campagnes) && !aucunecorrespondance) {
		if( !is.list(campagnes) )
			campagnes <- xrGetCampagnes(conn, pattern = campagnes,
										resv3=TRUE, silent=silent) else{
			campagnes[['resv3']] <- TRUE
			campagnes <- do.call(xrGetCampagnes,
								 c(list(conn=conn), campagnes, silent=silent))
		}
		if(nrow(campagnes) > 0) {
			query     <- paste0(campagnes[['id']], collapse=',')
			query     <- paste0(bquery, 'campaigns=', query)
			ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
			idmesures <- collapseIds(ist, idmesures, collapse)
		} else aucunecorrespondance  <- TRUE
	}

	# recherche sur reseaux ---------------------------------------------------

	if (!is.null (reseaux) && !aucunecorrespondance) {
		if( !is.list(reseaux) )
			reseaux <- xrGetReseaux(conn, pattern = reseaux,
									resv3=TRUE, silent=silent) else{
			reseaux[['resv3']] <- TRUE
			reseaux <- do.call(xrGetReseaux,
							   c(list(conn=conn), reseaux, silent=silent))
			}
		if(nrow(reseaux) > 0) {
			query     <- paste0(reseaux[['id']], collapse=',')
			query     <- paste0(bquery, 'groups=', query)
			ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
			idmesures <- collapseIds(ist, idmesures, collapse)
		} else aucunecorrespondance  <- TRUE
	}

	# recherche sur stations --------------------------------------------------

	if (!is.null (stations) && !aucunecorrespondance) {
		if( !is.list(stations) )
			stations <- xrGetStations(conn, pattern = stations,
									  resv3=TRUE, silent=silent) else{
			stations[['resv3']] <- TRUE
			stations <- do.call(xrGetStations,
								c(list(conn=conn), stations, silent=silent))
		}
		if(nrow(stations) > 0) {
			query     <- paste0(stations[['id']], collapse=',')
			query     <- paste0(bquery, 'sites=', query)
			ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
			idmesures <- collapseIds(ist, idmesures, collapse)
		} else aucunecorrespondance  <- TRUE
	}

	# recherche sur polluants -------------------------------------------------

	if (!is.null (polluants) && !aucunecorrespondance) {
		if( !is.list(polluants) )
			polluants <- xrGetPolluants(conn, pattern = polluants,
										resv3=TRUE, silent=silent) else{
			polluants[['resv3']] <- TRUE
			polluants <- do.call(xrGetPolluants,
								 c(list(conn=conn), polluants, silent=silent))
		}
		if(nrow(polluants) > 0) {
			query     <- paste0(polluants[['id']], collapse=',')
			query     <- paste0(bquery, 'physicals=', query)
			ist       <- unique(xrGetQuery(conn, query, resv3=TRUE)[['id']])
			idmesures <- collapseIds(ist, idmesures, collapse)
		} else aucunecorrespondance  <- TRUE
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(aucunecorrespondance ||
	   (!is.null(c(pattern, campagnes, reseaux, stations, polluants))
		& length(idmesures) == 0)) idmesures <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	query <- sprintf('%svalidOnly=%s', query, if(validOnly) 'TRUE' else 'FALSE')

	if(!is.null(idmesures)) {
		# permet de gérer les cas où trop d'id sont demandés (ce qui fait planter
		# le serveur ...)
		idmesures <- split(idmesures, ceiling(seq_along(idmesures)/500))

		queries   <- lapply(idmesures, function(idm)
							sprintf('%s&measures=%s', query, paste(idm, collapse=',')))

		mesures   <- lapply(queries, function(q)
							xrGetQuery(conn, q, resv3=TRUE))
		mesures   <- do.call(rbind, mesures)

	} else mesures <- xrGetQuery(conn, query, resv3=TRUE)

	# TODO:  ajouter filtre stopDate, startDate

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

