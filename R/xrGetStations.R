#' Recuperation des stations de mesures definies dans une base XR
#'
#' La fonction permet de lister les stations existantes dans la 
#' base XR référencée par la connection \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param mesures chaînes de caractères correspondant aux mesures à rappatrier
#' 	(optionnel) (utilisé via la fonction \code{\link{xrGetMesures}}).
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les stations trouvées.
xrGetStations <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NOM_COURT_SIT'),
			  campagnes = NULL, reseaux = NULL, fields = NULL, mesures = NULL, collapse = c('AND', 'OR')) {

	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- '*'#dbListFields (conn, 'STATION')
	
	query <- sprintf ('SELECT %s FROM', paste ('STATION', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'STATION'

	if (!is.null (mesures) )
		pattern <- c(pattern, unique (xrGetMesures (conn, pattern = mesures)$NOM_COURT_SIT) )

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

	if (!is.null (campagnes) ) {
		q$campagnes <- unique (xrGetCampagnes (conn, pattern = campagnes)$NOM_COURT_CM)
		if (length(q$campagnes) == 0) q$campagnes <- NULL else {
			q$tables <- c(q$tables, 'CAMPMES_STATION')
			q$campagnes <- sprintf(
				'STATION.NOM_COURT_SIT=CAMPMES_STATION.NOM_COURT_SIT AND CAMPMES_STATION.NOM_COURT_CM IN (%s)',
				paste ("'", q$campagnes, "'", sep = '', collapse = ", ") )
		}
	}

	if (!is.null (reseaux) ) {
		q$reseaux <- unique (xrGetReseaux (conn, pattern = reseaux)$NOM_COURT_RES)
		if (length(q$reseaux) == 0) q$reseaux <- NULL else {
			q$tables <- c(q$tables, 'RESEAUSTA')
			q$reseaux <- sprintf(
				'STATION.NOM_COURT_SIT=RESEAUSTA.NOM_COURT_SIT AND RESEAUSTA.NOM_COURT_RES IN (%s)',
				paste ("'", q$reseaux, "'", sep = '', collapse = ", ") )
		}
	}

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	stations <- unique (xrGetQuery (conn, query) )

	temp <- xrGetQuery (conn, "SELECT CLE, LIBELLE FROM LISTE_META_DONNEES WHERE CODE_ID_LISTE='CL_SITE'")
	names (temp)[2] <- 'typologie'
	merge (stations, temp, by.x='CLASSE_SITE', by.y='CLE', all.x=TRUE, all.y=FALSE)
}


