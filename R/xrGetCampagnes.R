#' Recuperation des campagnes de mesures definies dans une base XR
#'
#' La fonction permet de lister les campagnes existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' Il est possible de préciser les champs de recherche,
#' ou des dates entre lesquelles la station a fonctionné.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les campagnes trouvées.
xrGetCampagnes <- function(conn, pattern=NULL,
			   search.fields=c('NOM_COURT_CM', 'LIBELLE'),
			   start=NULL, end=NULL, fields = NULL, exact=FALSE) {
	if (is.null (fields) )
		fields <- dbListFields (conn, 'CAMPMES', schema='RSDBA')

	query <- sprintf ('SELECT %s FROM CAMPMES', paste ('CAMPMES', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
					pattern, search.fields,
					type=ifelse(exact, 'IN', 'LIKE'))

	if (!is.null (start) ) {
		if (length (start) > 1) warning ("Only the first 'start' value is used.")
		start <- start[1]
		start <- try (as.POSIXct(start) )
		if (inherits (start, 'try-error') ) {
			warning ("'start' can not be coerced to a date. Not used.")
		} else {
			q$start <- sprintf ("DATEDEB >= TO_DATE('%s', 'YY-MM-DD')", format (start, '%y-%m-%d') )
		}
	}
	
	if (!is.null (end) ) {
		if (length (end) > 1) warning ("Only the first 'end' value is used.")
		end <- end[1]
		end <- try (as.POSIXct(end) )
		if (inherits (end, 'try-error') ) {
			warning ("'end' can not be coerced to a date. Not used.")
		} else {
			q$end <- sprintf ("DATEFIN <= TO_DATE('%s', 'YY-MM-DD')", format (end, '%y-%m-%d') )
		}
	}

	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = ' AND ') )

	xrGetQuery (conn, query)
}


