#' Recuperation des mesures definies dans une base XR
#'
#' La fonction permet de lister les mesures existantes dans la 
#' base XR référencée par la connection \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les mesures trouvées.
xrGetMesures <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NOM_COURT_MES'),
			  campagnes = NULL, reseaux = NULL, stations=NULL, polluants = NULL,
			  fields = NULL, collapse = c('AND', 'OR')) {

	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	fields.tmp <- dbListFields (conn, 'MESURE', schema='RSDBA')
	search.fields <- intersect (search.fields, fields.tmp)
	if (is.null (fields) ) fields <- fields.tmp

	query <- sprintf ('SELECT %s FROM', paste ('MESURE', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'MESURE'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern,
						   paste ('MESURE', search.fields, sep='.'))

	if (!is.null (reseaux) ) {
		q$reseaux <- unique (xrGetReseaux (conn, pattern = reseaux)$NOM_COURT_RES)
		if (length(q$reseaux) == 0) q$reseaux <- NULL else {
			q$tables <- c(q$tables, 'RESEAUMES')
			q$reseaux <- sprintf(
				'MESURE.NOM_COURT_MES=RESEAUMES.NOM_COURT_MES AND RESEAUMES.NOM_COURT_RES IN (%s)',
				paste ("'", q$reseaux, "'", sep = '', collapse = ", ") )
		}
	}

	if (!is.null (stations) | !is.null (campagnes) | !is.null(reseaux) ) {
		q$stations <- unique (xrGetStations (conn, collapse = gsub (' ', '', collapse),
						     pattern = stations,
						     reseaux=reseaux, campagnes=campagnes)$NOM_COURT_SIT)
		if (length(q$stations) == 0) q$stations <- NULL else {
			q$tables <- c(q$tables, 'STATION')
			q$stations <- sprintf(
				'MESURE.NOM_COURT_SIT=STATION.NOM_COURT_SIT AND STATION.NOM_COURT_SIT IN (%s)',
				paste ("'", q$stations, "'", sep = '', collapse = ", ") )
		}
	}

	if (!is.null (polluants) ) {
		q$polluants <- unique (xrGetPolluants (conn, pattern = polluants)$NOPOL)

		if (length(q$polluants) == 0) q$polluants <- NULL else {
			q$tables <- c(q$tables, 'NOM_MESURE')
			q$polluants <- sprintf(
				'MESURE.NOPOL=NOM_MESURE.NOPOL AND NOM_MESURE.NOPOL IN (%s)',
				paste ("'", q$polluants, "'", sep = '', collapse = ", ") )
		}
	}

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	unique (xrGetQuery (conn, query) )
}


