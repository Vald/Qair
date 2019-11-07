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
			   fields = NULL, exact = FALSE, resv3 = FALSE) {

	# Fonction validée le 06/11/2019

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('physicals?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('physicals')
	if(is.null(search.fields))
		search.fields <- c('id', 'chemicalSymbol', 'label') else {
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
	# si on a que id  comme champ de recherche, on utilise directement
	# l'API d'XR, sinon on charge toutes les stations d'XR et ces deux
	# champs sont traités comme les autres (puisqu'on est de toute façon 
	# obligé de charger toutes les stations pour les autres champs)

	if(!is.null(pattern))
	#if(all(search.fields == 'id')) {}
	# TODO:ISEO recherche % ne marche pas sur physicals
	if(FALSE){
		# recherche sur id / NOPOL / physicals
		if(conn[['version']] == 2 & !exact)
			query <- paste0('%', pattern, '%', collapse=',') else
			query <- paste0(pattern, collapse=',')
		if('id' %in% search.fields){
			query       <- paste0(bquery, 'physicals=', query)
			ist         <- xrGetQuery(conn, query, resv3=TRUE)[['id']]
			idpolluants <- collapseIds(ist, idpolluants, 'OR')
		}
	} else {
		all.polluants <- xrGetQuery(conn, bquery, resv3=TRUE)
		for (sf in search.fields){
			if(conn[['version']] == 2){
				if(exact)
					selection <- match(pattern, all.polluants[[sf]]) else
					selection <- sapply(pattern, grep, all.polluants[[sf]])
			} else {
				selection <- gsub('\\%', '.*', pattern)
				selection <- gsub('\\?', '.', selection)
				selection <- paste0('^', selection, '$')
				selection <- sapply(selection, grep, all.polluants[[sf]])
			}
			ist         <- all.polluants[['id']][unique(unlist(selection))]
			idpolluants <- collapseIds(ist, idpolluants, 'OR')
		}
	}

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


