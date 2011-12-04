#' Recuperation des reseaux de mesures definis dans une base XR
#'
#' La fonction permet de lister les réseaux existants dans la 
#' base XR référencée par la connection \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les réseaux trouvés.
xrGetReseaux <- function(conn, pattern=NULL, search.fields=c('NOM_COURT_RES', 'NOM_RES'),
			   fields = NULL) {
	if (is.null (fields) ) fields <- dbListFields (conn, 'RESEAUDEF')

	query <- sprintf ('SELECT %s FROM RESEAUDEF', paste ('RESEAUDEF', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)


	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = ' AND ') )

	reseaux <- xrGetQuery (conn, query)
	return (reseaux)
}


