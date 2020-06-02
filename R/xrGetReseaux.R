#' Recuperation des reseaux de mesures definis dans une base XR
#'
#' La fonction permet de lister les réseaux existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les réseaux trouvés.
xrGetReseaux <- function(conn, pattern = NULL, search.fields = NULL,
						 fields = NULL, exact = FALSE , resv3 = FALSE) {

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v1/measure-groups?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('measure-groups')
	if(is.null(search.fields)){
		search.fields <- c('id', 'label')
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
	# récupération des sites sur la base de search.fields
	# puis recherche de mesure si != NULL et récup. de id site

	idreseaux <- NULL

	# recherche sur search.fields ---------------------------------------------
	# si on a que id  comme champ de recherche, on utilise directement
	# l'API d'XR, sinon on charge toutes les stations d'XR et ce
	# champ est traité comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern))
	#if(all(search.fields == 'id')){
	if(FALSE){
		# recherche sur id / NOM_COURT_RES / measureGroups
		# TODO:ISEO rechercher % sur ref ne marche pas donc pour l'instant on ne garde 
		# que l'approche 'global'

		if(!exact)
			query <- paste0('%', pattern, '%', collapse=',') else
			query <- paste0(pattern, collapse=',')
		if('id' %in% search.fields){
			query     <- paste0(bquery, 'measureGroups=', query)
			ist       <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idreseaux <- collapseIds(ist, idreseaux, 'OR')
		}
	} else {
		# sinon recherche sur la base de toutes les stations
		# Cette partie a été validée le 06/11/2019

		all.reseaux <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.reseaux[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.reseaux[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.reseaux[[sf]])
				}}
			ist       <- all.reseaux[['id']][unique(unlist(selection))]
			idreseaux <- collapseIds(ist, idreseaux, 'OR')
		}
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(pattern) & length(idreseaux) == 0)
		idreseaux <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	if(!is.null(idreseaux))
		query <- sprintf('%s&measureGroups=%s', query, paste(idreseaux, collapse=','))

	reseaux <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(reseaux) <- xrfields[['nv2']][match(names(reseaux), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que reseaux ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(reseaux)))
		reseaux[setdiff(fields, names(reseaux))] <- NA

	return(reseaux[fields])
}


