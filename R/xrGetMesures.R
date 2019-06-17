#' Recuperation des mesures definies dans une base XR
#'
#' La fonction permet de lister les mesures existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#' @param resv3 Booléen : faut-il forcer le retour au format v3 ? Par défaut
#'  non, donc si la connexion à XR est en v2, les noms correspondront à la v2.
#'  Seul le résultat est affecté : si la connexion est en v2, les noms des
#'  champs de recherches doivent être spécifiés en v2. Son utilisation devrait
#'  être réservée à des fins de développement.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les mesures trouvées.
xrGetMesures <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, reseaux = NULL, stations=NULL, polluants = NULL,
			  fields = NULL, collapse = c('AND', 'OR'), exact=FALSE, resv3=FALSE) {
	collapse <- match.arg(collapse)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('measures?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('measures')

c('IDENTIFIANT', 'NOM_COURT_MES')
	fields.tmp <- dbListFields (conn, 'MESURE', schema='RSDBA')
	search.fields <- intersect (search.fields, fields.tmp)
	if (is.null (fields) ) fields <- fields.tmp

	query <- sprintf ('SELECT %s FROM', paste ('MESURE', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'MESURE'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (
			pattern,
			paste ('MESURE', search.fields, sep='.'),
			type=ifelse(exact, 'IN', 'LIKE'))

	if (!is.null (reseaux) ) {
		if( !is.list(reseaux) )
			q$reseaux <- unique (xrGetReseaux (conn, pattern = reseaux)$NOM_COURT_RES) else
			q$reseaux <- unique(do.call(xrGetReseaux, c(list(conn=conn), reseaux))$NOM_COURT_RES)

		if (length(q$reseaux) == 0) q$reseaux <- NULL else {
			q$tables <- c(q$tables, 'RESEAUMES')
			q$reseaux <- sprintf(
				'MESURE.NOM_COURT_MES=RESEAUMES.NOM_COURT_MES AND RESEAUMES.NOM_COURT_RES IN (%s)',
				paste ("'", q$reseaux, "'", sep = '', collapse = ", ") )
		}
	}

	if (!is.null (stations) | !is.null (campagnes) | !is.null(reseaux) ) {
		if(!is.list(stations)) {
			q$stations <- unique (xrGetStations (
						     conn, collapse = gsub (' ', '', collapse),
						     pattern = stations,
						     reseaux=reseaux, campagnes=campagnes)$NOM_COURT_SIT)
		} else {
			stations$reseaux <- unique(c(stations$reseaux, reseaux))
			stations$campagnes <- unique(c(stations$campagnes, campagnes))
			stations$collapse <- gsub(' ', '', collapse)
			q$stations <- unique(do.call(xrGetStations, c(list(conn=conn), stations))$NOM_COURT_SIT)
		}
		if (length(q$stations) == 0) q$stations <- NULL else {
			q$tables <- c(q$tables, 'STATION')
			q$stations <- sprintf(
				'MESURE.NOM_COURT_SIT=STATION.NOM_COURT_SIT AND STATION.NOM_COURT_SIT IN (%s)',
				paste ("'", q$stations, "'", sep = '', collapse = ", ") )
		}
	}

	if (!is.null (polluants) ) {
		if( !is.list(polluants) )
			polluants <- xrGetPolluants (conn, polluants) else
			polluants <- do.call(xrGetPolluants, c(list(conn=conn), polluants))
		q$polluants <- unique(polluants$NOPOL)

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

	res <- xrGetQuery (conn, query)
	res$FMUL <- as.numeric(res$FMUL)
	unique( res )
}


