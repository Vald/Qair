#' Recuperation des polluants definis dans une base XR
#'
#' La fonction permet de lister les polluants existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les polluants trouvés.
xrGetPolluants <- function(conn, pattern=NULL, search.fields=c('NOPOL', 'CCHIM', 'NCON'),
			   fields = NULL, exact=FALSE) {
	if (is.null (fields) )
		fields <- dbListFields (conn, 'NOM_MESURE', schema='RSDBA')

	query <- sprintf ('SELECT %s FROM NOM_MESURE', paste ('NOM_MESURE', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
					pattern, search.fields,
					type=ifelse(exact, 'IN', 'LIKE'))

	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = ' AND ') )

	xrGetQuery (conn, query)
}


