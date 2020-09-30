#' Recuperation des methodes de prelevement manuels definies dans une base XR
#'
#' La fonction permet de lister les méthodes de prélèvement existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetManualData}}, \code{\link{xrGetSitesPrelevement}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les méthodes trouvées.
xrGetMethodesPrelevement <- function(conn, pattern = NULL, search.fields = c('LIBELLE'),
			  fields = NULL, collapse = c('AND', 'OR'), exact=FALSE) {
	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- '*'
	
	query <- sprintf ('SELECT %s FROM', paste ('METH_PRELEVEMENT', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'METH_PRELEVEMENT'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
					pattern, search.fields,
					type=ifelse(exact, 'IN', 'LIKE'))

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	methodes <- unique (xrGetQueryBD (conn, query) )
	methodes
}


