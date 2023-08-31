#' Recuperation des methodes de prelevement manuels definies dans une base XR
#'
#' La fonction permet de lister les méthodes de prélèvement existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetManualData}}, \code{\link{xrGetSitesPrelevement}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les méthodes trouvées.
xrGetMethodesPrelevement <- function(conn, pattern = NULL, search.fields = NULL,
									 fields = NULL, resv3 = FALSE, silent) {

	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/samplingMethodes?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('samplingMethodes')
	if(is.null(search.fields)) {
		search.fields <- xrfields[['nv3']][1:3]
		if(!silent) message("Champs disponibles pour la recherche (méthodes de prélèvements) : \n",
				paste(collapse=', ', xrfields[[nv]]),
				"\n\nPar défaut : ",
				paste(collapse=', ', xrfields[[nv]][1:3]),
				"\n\n")
	}else {
		search.fields <- match.arg(search.fields, xrfields[[nv]], TRUE)
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des mesures sur la base de search.fields
	# puis refiltre si search.fields contient autre chose que juste id/NJOM_COUR_MEs
	# --> agregation des différents tests en fonction de collapse...

	idmethodes <- NULL

	# recherche sur search.fields ---------------------------------------------
	# on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern)) {
		all.methodes <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.methodes[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.methodes[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.methodes[[sf]])
				}}
			ist         <- all.methodes[['samplingMethod']][unique(unlist(selection))]
			idmethodes <- collapseIds(ist, idmethodes, 'OR')
		}
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(pattern) & length(idmethodes) == 0)
		idmethodes <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	if(!is.null(idmethodes))
		query <- sprintf('%s&codeSamplingMethod=%s', query, paste(idmethodes, collapse=','))

	methodes <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(methodes) <- xrfields[['nv2']][match(names(methodes), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que methodes ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(methodes)))
		methodes[setdiff(fields, names(methodes))] <- NA

	return(methodes[fields])
}


