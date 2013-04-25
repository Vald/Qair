#' Recuperation des sites de prelevement manuels definis dans une base XR
#'
#' La fonction permet de lister les sites de prélèvement existants dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#' @param SP booléen indiquant si le résultat doit être au format
#'	SpatialPointsDataFrame. proj4string est alors utilisé pour 
#' 	préciser le référentiel spatial choisi. Les coordonnées utilisées
#' 	sont les champs 'LAMBERTX' et LAMBERTY' de XR.
#' @param proj4string indique les paramètres de projections. Par défaut :
#' CRS('+init=epsg:2154') (lambert 93). Pour du lambert II étendu, indiquer :
#' CRS('+init=epsg:27572')
#'
#' @seealso \code{\link{xrGetManualData}}, \code{\link{xrGetMethodesPrelevement}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les sites trouvés.
xrGetSitesPrelevement <- function(conn, pattern = NULL,
		  search.fields = c('IDSITEP', 'LIBELLE'),
		  campagnes = NULL, fields = NULL,
		  collapse = c('AND', 'OR'), exact=FALSE,
		  SP=FALSE, proj4string=CRS('+init=epsg:2154')) {

	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- '*'
	
	query <- sprintf ('SELECT %s FROM',
		  paste ('SITE_PRELEVEMENT', fields,
			 sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'SITE_PRELEVEMENT'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
					pattern, search.fields,
					type=ifelse(exact, 'IN', 'LIKE'))

	if (!is.null (campagnes) ) {
		if( !is.list(campagnes) )
			q$campagnes <- unique (xrGetCampagnes (conn, pattern = campagnes)$NOM_COURT_CM) else
			q$campagnes <- unique(do.call(xrGetCampagnes, c(list(conn=conn), campagnes))$NOM_COURT_CM)

		if (length(q$campagnes) == 0) q$campagnes <- NULL else {
			q$tables <- c(q$tables, 'CAMPMES_SITE_P')
			q$campagnes <- sprintf(
				'SITE_PRELEVEMENT.NSIT=CAMPMES_SITE_P.NSIT AND CAMPMES_SITE_P.NOM_COURT_CM IN (%s)',
				paste ("'", q$campagnes, "'",
				       sep = '', collapse = ", ") )
		}
	}

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )

	q$tables <- NULL

	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query,
				  paste (q, collapse = collapse) )

	sites <- unique (xrGetQuery (conn, query) )

	if( SP ) {
		sites$LAMBERTX[is.na(sites$LAMBERTX)] <- 0
		sites$LAMBERTY[is.na(sites$LAMBERTY)] <- 0

		sites <- SpatialPointsDataFrame(
			sites[c('LAMBERTX', 'LAMBERTY')], sites,
			proj4string=proj4string)
	}

	return( sites )
}


