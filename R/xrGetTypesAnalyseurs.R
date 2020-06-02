#' Pour chaque mesure, liste les installations des analyseurs.
#'
#' @param conn Connexion à la base XR.
#' @param x Une data.frame telle que retournée par
#'  \code{\link[Qair]{xrGetMesures}} avec au moins les colonnes 'NOPOL',
#'  'NOM_COURT_MES' (v2) et/ou 'idPhy', 'FIXME:ISEO remplacant de NOM_COURT_MES'.
#' @param debut POSIXct indiquant la date de début de la période
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @param fin POSIXct indiquant la date de fin de la période
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @inheritParams xrGetStations
#' @return Retourne un  TimeInstantDataFrame dont chaque élément contient,
#'  pour une des mesures demandées, la liste des analyseurs utilisés et la
#'  date d'installation de l'analyseur.
xrGetTypesAnalyseurs <- function(conn, x, debut, fin, resv3=FALSE) {
	nv     <- paste0('nv', conn[['version']])

	if(nv == 2) {
		names(x)[names(x) == 'NOPOL'] <- 'idPhy'
		names(x)[names(x) == 'NOM_COURT_MES'] <- 'FIXME:ISEO'
	}

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
					paste(x[['id']], collapse=','), # FIXME:ISEO
					'&from=', from, '&to=', to)
	analyseurs <- xrGetQuery(conn, query, resv3=TRUE)
	analyseurs <- as.list(analyseurs)

	# récupération des infos sur les analyseurs trouvés -----------------------

	ids <- lapply(analyseurs[['trackEquipments']], '[[', 'id_equipment')
	ids <- unlist(ids)
	query <- paste0('equipments?idEquipment=',
					paste(ids, collapse=','),  # FIXME:ISEO
					'&withDetail=1')
	analyseursl <- xrGetQuery(conn, query, resv3=TRUE)
	analyseursl <- data.frame(
		id    = analyseursl[['id']],
		idPhy = sapply(analyseursl[['physicals']], '[[', 'id'))

	# réduction de la première liste en se basant sur les polluants -----------
	# trouvés dans la seconde (de façon à ce que ça colle avec 
	# la colonne idPhy)

	# TODO: faire un match entre les deux

	# Pour chaque mesure, création d'une data.frame 


	# FIXME:ISEO inconsistance entre id retourné par trackMeasureEquipment et equipment
	# l'id récupéré par trackMachin est in entier au lieu d'une chaîne de caractères

	# création de la structure de retour --------------------------------------

	analyseurs <- lapply(analyseurs, function(x)
						 TimeInstantDataFrame(c(debut, x$date[-1]), data=x))

	if(!resv3 & nv == 'nv2') analyseurs <- lapply(analyseurs, 'names<-', 'MODELE')

	return (analyseurs)
}

