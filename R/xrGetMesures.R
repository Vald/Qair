#' Recuperation des mesures definies dans une base XR
#'
#' La fonction permet de lister les mesures existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetStations
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les mesures trouvées.
xrGetMesures <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, reseaux = NULL, stations=NULL, polluants = NULL,
			  fields = NULL,
			  collapse = c('AND', 'OR'), exact=FALSE, resv3=FALSE, validOnly=FALSE){#,
			  #startDate=NULL, stopDate=NULL) {
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
	if(is.null(search.fields)){
		search.fields <- c('id')#FIXME: mettre le remplacant de NOM_COURT_MES en plus
	}else{
		search.fields <- match.arg(search.fields, xrfields[[nv]], TRUE)
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des mesures sur la base de search.fields, de campagne, de reseaux 
	# de stations et de polluants
	# puis refiltre si search.fields contient autre chose que juste id/NJOM_COUR_MEs
	# --> agregation des différents tests en fonction de collapse...

	# FIXME:1 comme la recherche % et ? ne marche pas sur id, on prend toutes
	# les mesures et on cherche dedans
	all.mesures <- xrGetQuery(conn, bquery, resv3=TRUE)

	# recherche sur id si id in search.fields / IDENTIFIANT / measures
	# FIXME:1 en attendant que la recherche % et ? fonctionne 'id' et 'ref'
	# sont gérés comme les autres champs.
	# if('id' in search.fields)
		#if(conn[['version']] == 2 & !exact)
		#	query <- paste0('%', pattern, '%', collapse=',') else
		#	query <- paste0(pattern, collapse=',')

	idmesures <- NULL
	for (sf in search.fields){
		if(conn[['version']] == 2){
			if(exact)
				selection <- match(pattern, all.mesures[[sf]]) else
				selection <- sapply(pattern, grep, all.mesures[[sf]])
		} else {
			selection <- gsub('\\%', '.*', pattern)
			selection <- gsub('\\?', '.', selection)
			selection <- paste0('^', selection, '$')
			selection <- sapply(selection, grep, all.mesures[[sf]])
		}
		ist       <- all.mesures[['id']][unique(unlist(selection))]
		idmesures <- collapseIds(ist, idmesures, collapse)
	}

 
	######################## OLD

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

	###############" FIN de OLD

	# création et exécution de la requête

	query <- bquery
	query <- sprintf('%svalidOnly=%s', query, if(validOnly) 'TRUE' else 'FALSE')
	if(!is.null(idmesures))
		query <- sprintf('%s&measures=%s', query, paste(idmesures, collapse=','))

	# FIXME:  ajouter filtre stopDate, startDate

	mesures <- xrGetQuery(conn, query, resv3=TRUE)

	# selection des champs de retour

	if(!resv3 & nv == 'nv2')
		names(mesures) <- xrfields[['nv2']][match(names(mesures), xrfields[['nv3']])]

	if (is.null(fields)) fields <- xrfields[[ifelse(resv3, 'nv3', nv)]]
	fields <- intersect(fields, xrfields[[ifelse(resv3, 'nv3', nv)]])

	return(mesures[fields])

}


