#' Recuperation des polluants definis dans une base XR
#'
#' La fonction permet de lister les polluants existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les polluants trouvés.
xrGetPolluants <- function(conn, pattern = NULL, search.fields = NULL,
			   fields = NULL, exact = FALSE, resv3 = FALSE, silent) {

	# Fonction validée le 06/11/2019

	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v1/physicals?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('physicals')
	if(is.null(search.fields)) {
		search.fields <- xrfields[['nv3']][1:3]
		if(!silent) message("Champs disponibles pour la recherche : ",
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

	idpolluants <- NULL

	# recherche sur search.fields ---------------------------------------------
	# on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern)) {
		all.polluants <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(!exact)
				selection <- sapply(pattern, grep, all.polluants[[sf]]) else {
				if(conn[['version']] == 2){
					selection <- match(pattern, all.polluants[[sf]])
				} else {
					selection <- gsub('\\%', '.*', pattern)
					selection <- gsub('\\?', '.', selection)
					selection <- paste0('^', selection, '$')
					selection <- sapply(selection, grep, all.polluants[[sf]])
				}}
			ist         <- all.polluants[['id']][unique(unlist(selection))]
			idpolluants <- collapseIds(ist, idpolluants, 'OR')
		}
	}

	# si des filtres ont été appliqués et que idsites est vide ----------------
	# la fonction retourne une data.frame vide
	# (on procède en donnant une valeur bidon à idsites)

	if(!is.null(pattern) & length(idpolluants) == 0)
		idpolluants <- 'AUCUNECORRESPONDANCE'

	# création et exécution de la requête -------------------------------------

	query <- bquery
	if(!is.null(idpolluants))
		query <- sprintf('%s&physicals=%s', query, paste(idpolluants, collapse=','))

	polluants <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour ------------------------------------------

	if(!resv3 & nv == 'nv2')
		names(polluants) <- xrfields[['nv2']][match(names(polluants), xrfields[['nv3']])]

	if (is.null(fields))
		fields <- xrfields[[ifelse(resv3, 'nv3', nv)]] else
		fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	# il est possible que dans la selection tous les champs ne soient pas renvoyés
	# et que polluants ne contient pas toutes les colonnes. Les manquantes sont 
	# ajoutées
	if(!all(fields %in% names(polluants)))
		polluants[setdiff(fields, names(polluants))] <- NA

	return(polluants[fields])
}


