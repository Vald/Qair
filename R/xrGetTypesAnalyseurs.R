#' Pour chaque mesure, liste les installations des analyseurs.
#'
#' @param conn Connexion à la base XR.
#' @param x Une data.frame telle que retournée par
#'  \code{\link[Qair]{xrGetMesures}} avec au moins les colonnes 'NOPOL' et
#'  'IDENTIFIANT' et/ou 'NOM_COURT_MES' (v2) ou 'physical.id', et 'id' et/ou 'dbRowId'.
#'  La fonction sera plus lente s'il n'y a qu'une seule des deux colonnes
#'  NOM_COURT_MES(dbRowId) et IDENTIFIANT(id).
#' @param debut POSIXct indiquant la date de début de la période
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @param fin POSIXct indiquant la date de fin de la période
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @inheritParams xrGetStations
#' @return Retourne un  TimeInstantDataFrame dont chaque élément contient,
#'  pour une des mesures demandées, la liste des analyseurs utilisés et la
#'  date d'installation de l'analyseur.
#' @export
xrGetTypesAnalyseurs <- function(conn, x, debut, fin, resv3=FALSE) {
	nv     <- paste0('nv', conn[['version']])


	if(nv == 'nv2') {
		names(x)[names(x) == 'NOPOL'] <- 'physical.id'
		names(x)[names(x) == 'IDENTIFIANT'] <- 'id'
		names(x)[names(x) == 'NOM_COURT_MES'] <- 'dbRowId'
		id      <- 'IDENTIFIANT'
		dbRowId <- 'NOM_COURT_MES'
	} else {
		id      <- 'id'
		dbRowId <- 'dbRowId'
	}

	if(!'id' %in% names(x))
		x <- xrGetMesures(conn, x[['dbRowId']], exact=TRUE, resv3=TRUE, search.fields=dbRowId)
	if(!'dbRowId' %in% names(x))
		x <- xrGetMesures(conn, x[['id']], exact=TRUE, resv3=TRUE, search.fields=id)

	# traitement des dates de debut et de fin ---------------------------------
	# si debut et fin ne sont pas en POSIXct, conversion

	if( inherits(debut, 'POSIXlt') ) debut <- as.POSIXct(debut)
	if( inherits(fin, 'POSIXlt') ) fin <- as.POSIXct(fin)

	if( !inherits(debut, 'POSIXct') ) debut <- as.POSIXct(debut, tz='UTC')
	if( !inherits(fin, 'POSIXct') ) fin <- as.POSIXct(fin, tz='UTC')

	debut <- as.POSIXct(as.POSIXlt(debut, tz='UTC'))
	fin <- as.POSIXct(as.POSIXlt(fin, tz='UTC'))

	# start et end sont mis en forme pour la requete ----------------------

	dformat <- '%Y-%m-%dT%H:%M:%SZ'
	from    <- format (debut, format = dformat, tz='UTC')
	to      <- format (fin+POSIXctp('second'), format = dformat, tz='UTC')

	# récupération de la liste des analyseurs par mesure ----------------------

	query <- paste0('v1/trackMeasureEquipments?measure=',
					paste(x[['id']], collapse=','))
	analyseurs <- xrGetQuery(conn, query, resv3=TRUE)
	analyseurs <- as.list(analyseurs)

	# création de la structure de retour --------------------------------------

	res <- lapply(analyseurs[['trackEquipments']], function(a) {
		a <- unique(a[c('startDate', 'endDate', 'model')])
		a <- a[(!is.na(a[['startDate']]) & is.na(a[['endDate']])) |
			   !a[['startDate']]==a[['endDate']],]

		a[['startDate']] <- strptime(a[['startDate']], '%Y-%m-%dT%H:%M:%SZ', 'UTC')
		a[['startDate']] <- trunc(a[['startDate']], 'hour')
		a[['startDate']] <- as.POSIXct(a[['startDate']])
		a <- a[order(a[['startDate']]),]
		a <- a[a[['startDate']] <= fin,]

		d <- (which(a[['startDate']] > debut)[1] - 1)
		a <- a[ifelse(is.na(d), nrow(a), d):nrow(a),]

		return(TimeInstantDataFrame(a[['startDate']], data=a['model']))
	})

	names(res) <- x[['dbRowId']][match(analyseurs[['id_measure']],x[['id']])]

	if(!resv3 & nv == 'nv2') res <- lapply(res, 'names<-', 'MODELE')

	return (res)
}

