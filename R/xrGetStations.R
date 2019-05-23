#' Recuperation des stations de mesures definies dans une base XR
#'
#' La fonction permet de lister les stations existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param mesures chaînes de caractères correspondant aux mesures à rapatrier
#' 	(optionnel) (utilisé via la fonction \code{\link{xrGetMesures}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetMesures Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetMesures
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 'N2_VER', search.fields = 'IDENTIFIANT') ...}
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les stations trouvées.
#' @export
xrGetStations <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NOM_COURT_SIT'),
			  campagnes = NULL, reseaux = NULL, fields = NULL, mesures = NULL,
			  exact=FALSE) {

	query <- sprintf('%ssites?', xrGetUrl(conn))

	fields.tmp <- dbListFields (conn, 'STATION', schema='RSDBA')
	search.fields <- intersect (search.fields, fields.tmp)
	if (is.null (fields) ) fields <- '*'#dbListFields (conn, 'STATION')
	if( fields != '*' & !'CLASSE_SITE' %in% fields)
		fields <- c(fields, 'CLASSE_SITE')
	
	query <- sprintf ('SELECT %s FROM', paste ('STATION', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'STATION'

	if (!is.null (mesures) ) {
		if( !is.list(mesures) )
			q$mesures <- unique (xrGetMesures (conn, pattern = mesures)$NOM_COURT_SIT) else
			q$mesures <- unique(do.call(xrGetMesures, c(list(conn=conn), mesures))$NOM_COURT_SIT)
		q$mesures <- match.pattern.fields (q$mesures, 'STATION.NOM_COURT_SIT', 'IN')
	}

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
					pattern, sprintf('STATION.%s', search.fields),
					type=ifelse(exact, 'IN', 'LIKE'))

	if (!is.null (campagnes) ) {
		if( !is.list(campagnes) )
			q$campagnes <- unique (xrGetCampagnes (conn, pattern = campagnes)$NOM_COURT_CM) else
			q$campagnes <- unique(do.call(xrGetCampagnes, c(list(conn=conn), campagnes))$NOM_COURT_CM)
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

	# TODO: typologie ?
	#temp <- xrGetQuery (conn, "SELECT CLE, LIBELLE FROM LISTE_META_DONNEES WHERE CODE_ID_LISTE='CL_SITE'")
	#names (temp)[2] <- 'typologie'
	return(xrGetQuery(conn, query))
}


