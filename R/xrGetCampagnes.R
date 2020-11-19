#' Recuperation des campagnes de mesures definies dans une base XR
#'
#' La fonction permet de lister les campagnes existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les campagnes trouvées.
xrGetCampagnes <- function(conn, pattern = NULL, search.fields = NULL,
					start = NULL, end = NULL, fields = NULL, exact = FALSE,
					resv3 = FALSE, silent) {
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v1/campaigns?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('campaigns')
	if(is.null(search.fields)){
		search.fields <- xrfields[['nv3']][1:2]
		if(!silent) message("Champs disponibles pour la recherche : ",
				paste(collapse=', ', xrfields[[nv]]),
				"\n\nPar défaut : ",
				paste(collapse=', ', xrfields[[nv]][1:2]), 
				"\n\n")
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

	idcampagnes <- NULL

	# recherche sur search.fields ---------------------------------------------

	if(!is.null(pattern)) {
		# recherche sur id / NOM_COURT_CM / campaigns
		all.campagnes <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.campagnes[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.campagnes[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.campagnes[[sf]])
				}}
			ist       <- all.campagnes[['id']][unique(unlist(selection))]
			idcampagnes <- collapseIds(ist, idcampagnes, 'OR')
		}
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(c(pattern)) & length(idcampagnes) == 0)
		idcampagnes <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	if(!is.null(idcampagnes))
		query <- sprintf('%s&campaigns=%s', query, paste(idcampagnes, collapse=','))

	# TODO:  ajouter filtre stopDate, startDate / start - end dans le cadre de
	# cette fonction

	campagnes <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(campagnes) <- xrfields[['nv2']][match(names(campagnes), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que campagnes ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(campagnes)))
		campagnes[setdiff(fields, names(campagnes))] <- NA

	return(campagnes[fields])
}

