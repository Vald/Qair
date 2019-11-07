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
					resv3 = FALSE) {

	# Fonction validée le 06/11/2019
	if(!is.null(search.fields))
		warning("'search.fields' est obsolète, sa valeur n'est pas prise en compte")

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('campaigns?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	idcampagnes <- NULL
	if(!is.null(pattern)) {
		# recherche sur id / NOM_COURT_CM / campaigns
		if(conn[['version']] == 2 & !exact)
			query <- paste0('%', pattern, '%', collapse=',') else
			query <- paste0(pattern, collapse=',')

		query     <- paste0(bquery, 'campaigns=', query)
		campagnes <- xrGetQuery(conn, query, resv3=TRUE)
		# FIXME:ISEO intégrer dans les champs de recherche le LIBELLE et 
		# son futur remplaçant --> réactiver le search.fields
	} else campagnes <- xrGetQuery(conn, bquery, resv3=TRUE)

	# TODO:  ajouter filtre stopDate, startDate / start - end dans le cadre de
	# cette fonction

	# selection des champs de retour ------------------------------------------

	xrfields <- xrListFields ('campaigns')
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


