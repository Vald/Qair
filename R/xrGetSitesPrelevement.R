#' Recuperation des sites de prelevement manuels definis dans une base XR
#'
#' La fonction permet de lister les sites de prélèvement existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#' @param start Debut de la période pour laquelle les sites doivent être recherchés
#' @param end Fin de la période pour laquelle les sites doivent être recherchés
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetManualData}}, \code{\link{xrGetMethodesPrelevement}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les sites trouvés.
xrGetSitesPrelevement <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, fields = NULL, start = NULL, end = NULL, tz='UTC',
			  collapse = c('AND', 'OR'), exact=FALSE, resv3=FALSE, silent) {

	# FIXME: ajouter dans l'API campagne ?
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)
	osaf     <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)
	collapse <- match.arg(collapse)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/samplingSites?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('samplingSites')
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

	# start et end sont mis en forme pour la requete ----------------------

	dformat <- '%Y-%m-%dT%H:%M:%SZ'
	if(!is.null(start)) {
		if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
		if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
		start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))

		from   <- format (start, format = dformat, tz='UTC')
		bquery <- sprintf('%sfrom=%s&', bquery, from)
	}

	if(!is.null(end)) {
		if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)
		if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)
		end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

		to     <- format (end+POSIXctp('second'), format = dformat, tz='UTC')
		bquery <- sprintf('%sto=%s&', bquery, from)
	}

	# algo:
	# récupération des sites de prelevements sur la base de search.fields
	# puis refiltre si search.fields contient autre chose que juste id/NJOM_COUR_MEs
	# --> agregation des différents tests en fonction de collapse...

	idsites <- NULL

	# recherche sur search.fields ---------------------------------------------
	# on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern)) {
		all.sites <- xrGetQuery(conn, bquery, resv3=TRUE)
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
			ist     <- all.sites[['idSamplingSite']][unique(unlist(selection))]
			idsites <- collapseIds(ist, idsites, collapse)
		}
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(pattern) & length(idsites) == 0)
		idsites <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	if(!is.null(idsites))
		query <- sprintf('%s&idSamplingSite=%s', query, paste(idsites, collapse=','))

	sites <- xrGetQuery(conn, query, resv3=TRUE)

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


