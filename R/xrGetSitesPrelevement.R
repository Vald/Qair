#' Recuperation des sites de prelevement manuels definis dans une base XR
#'
#' La fonction permet de lister les sites de prélèvement existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetManualData}}, \code{\link{xrGetMethodesPrelevement}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les sites trouvés.
xrGetSitesPrelevement <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, fields = NULL, 
			  exact=FALSE, resv3=FALSE, silent) {

	# FIXME: ajouter dans l'API campagne ?
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)
	osaf     <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/samplingSite?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('samplingSite')
	if(is.null(search.fields)){
		search.fields <- xrfields[['nv3']][3:4]
		if(!silent) message("Champs disponibles pour la recherche (sites de prélèvements) : \n",
				paste(collapse=', ', xrfields[[nv]]),
				"\n\nPar défaut : ",
				paste(collapse=', ', xrfields[[nv]][3:4]),
				"\n\n")
	}else{
		search.fields <- as.character(match.arg(search.fields, xrfields[[nv]], TRUE))
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des sites de prelevements sur la base de search.fields
	# --> agregation des différents tests en fonction de collapse...

	idsites <- NULL

	# recherche sur search.fields ---------------------------------------------
	# on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	all.sites <- xrGetQuery(conn, bquery, resv3=TRUE)

	if(!is.null(pattern)) {
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.sites[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.sites[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.sites[[sf]])
				}}
			ist     <- unique(unlist(selection))
			idsites <- collapseIds(ist, idsites, 'OR')
		}
	} else {
		idsites <- TRUE
	}

	sites <- all.sites[idsites,]

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(sites) <- xrfields[['nv2']][match(names(sites), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que sites ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(sites)))
		sites[setdiff(fields, names(sites))] <- NA

	options(stringsAsFactors = osaf)
	return(sites[fields])
}


