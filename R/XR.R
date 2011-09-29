xrGetQuery <- function (conn, query) {
	if(.Platform$OS.type=="windows") {
		sqlQuery(conn, query)
	} else if(.Platform$OS.type=="unix") {
		dbGetQuery(conn, query)
	} else stop ('Unrecognized platform oO.')

}

xrConnect <- function(dsn=NULL, uid=NULL, pwd=NULL, host=NULL, ojdbc=NULL) {
	# host et ojdbc sont à spécifier uniquement dans le cas de l'utilisation de RJDBC

	# definition de la base de donnees (dsn)
	if(!is.null(dsn)) {
		options(Xair.dsn=dsn)
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='unix') {
		cat('nom de la base de donnees (DataSourceName) :\n')
		options(Xair.dsn=scan(what='character', nlines=1))
		cat('\n')
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='windows') {
		cat('nom de la base de donnees (DataSourceName) :\n',
		    names(odbcDataSources('system')), '\n')
		options(Xair.dsn=scan(what='character', nlines=1));cat('\n')
	}

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption('Xair.uid'))) {
		cat('identifiant pour la connection :\n')
		options(Xair.uid=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption('Xair.pwd'))) {
		cat('mot de passe pour la connection :\n')
		options(Xair.pwd=scan(what='character', nlines=1))
		cat('\n')
	}
	# definition de l'hote (unix like system)
	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption('Xair.host')) & .Platform$OS.type=='unix') {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}

	# connection a la base
	if(.Platform$OS.type=='windows') {
		conxair <- try (odbcConnect (getOption('Xair.dsn'),
					     uid = getOption('Xair.uid'),
					     pwd = getOption('Xair.pwd'),
					     case = 'nochange', believeNRows = TRUE) )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connection a la base Xair.')
	} else if(.Platform$OS.type=='unix') {
		if(!is.null(options()$Xair.ojdbc.file)) {
			drv <- JDBC('oracle.jdbc.OracleDriver', options()$Xair.ojdbc.file)
		} else {
			cat('Veuillez entrer le chemin vers le fichier java ojdbc*.jar\nAfin de ne pas avoir a renseigner ce chemin a chaque session\nvous pouvez definir la variable options(Xair.ojdbc.file = ...) dans le fichier ~/.Rprofile\n')
			drv <- scan(nmax = 1, what = 'character')
		}
		conxair <- try (dbConnect (drv,
					   paste ('jdbc:oracle:thin:@', getOption('Xair.host'),
						  ':1521:', getOption('Xair.dsn'), sep=''),
					   getOption('Xair.uid'),
					   getOption('Xair.pwd'),
					   identifer.quote='\'') )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connection a la base Xair.')
	} else {
		stop('platforme non reconnue')
	}
	return (conxair)
}

# conn <- xrConnect()

match.pattern.fields <- function (pattern, search.fields) {
	sprintf ('(%s)',
		 paste ('(', rep (search.fields, each = length(pattern) ),
			" LIKE '%", pattern, "%')",
			sep = '', collapse = ' OR ') )

}

xrGetCampagnes <- function(conn, pattern=NULL, search.fields=c('NOM_COURT_CM', 'LIBELLE'), start=NULL, end=NULL,
			   fields = NULL) {
	if (is.null (fields) ) fields <- dbListFields (conn, 'CAMPMES')

	query <- sprintf ('SELECT %s FROM CAMPMES', paste ('CAMPMES', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

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

xrGetReseaux <- function(conn, pattern=NULL, search.fields=c('NOM_COURT_RES', 'NOM_RES'),
			   fields = NULL) {
	if (is.null (fields) ) fields <- dbListFields (conn, 'RESEAUDEF')

	query <- sprintf ('SELECT %s FROM RESEAUDEF', paste ('RESEAUDEF', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)


	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = ' AND ') )

	reseaux <- xrGetQuery (conn, query)
	return (reseaux)
}

xrGetStations <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NSIT', 'NOM_COURT_SIT'),
			  campagnes = NULL, reseaux = NULL, fields = NULL, collapse = c('AND', 'OR')) {

	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- dbListFields (conn, 'STATION')

	query <- sprintf ('SELECT %s FROM', paste ('STATION', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'STATION'

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

	unique (xrGetQuery (conn, query) )
}

xrGetPolluants <- function(conn, pattern=NULL, search.fields=c('NOPOL', 'CCHIM', 'NCON'),
			   fields = NULL) {
	if (is.null (fields) ) fields <- dbListFields (conn, 'NOM_MESURE')

	query <- sprintf ('SELECT %s FROM NOM_MESURE', paste ('NOM_MESURE', fields, sep='.', collapse=', ') )
	q <- list()

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = ' AND ') )

	xrGetQuery (conn, query)
}

xrGetMesures <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NOM_COURT_MES'),
			  campagnes = NULL, reseaux = NULL, stations=NULL, polluants = NULL,
			  fields = NULL, collapse = c('AND', 'OR')) {

	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- dbListFields (conn, 'MESURE')

	query <- sprintf ('SELECT %s FROM', paste ('MESURE', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'MESURE'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

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

	print (q)

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	unique (xrGetQuery (conn, query) )
}

# à déplacer dans un fichier zzz.R
library (lubridate)
qh <- new_period (minutes=15)
h <- new_period (hours=1)
#---------------------------------

# pour le test
library (RJDBC)
conn <- xrConnect()
pattern <- c('VER', 'AYT')
polluants <- 'NO2'
reseaux <- campagnes <- stations <- NULL
validated <- TRUE
start <- as.POSIXct('2010-01-01', tz='UTC')
end <- as.POSIXct('2010-12-31', tz='UTC')
period <- 'h'
collapse <- 'AND'
search.fields <- 'IDENTIFIANT'
valid.states <- c("A", "R", "O", "W")
what <- 'both'

xrGetData <- function (conn, pattern=NULL, start, end,
		       period = c('qh', 'h', 'd', 'm', 'y'),
		       validated = TRUE, valid.states = c("A", "R", "O", "W"), what = c('value', 'state', 'both'),
		       search.fields='IDENTIFIANT', campagnes = NULL, reseaux = NULL, stations = NULL, polluants = NULL,
		       collapse = c('AND', 'OR'), XR6 = TRUE) {
	# period est un periode au sens lubridate
	# start et end doivent être des POSIXt (pour la prise en compte des timezones).
	period <- match.arg (period)
	what <- match.arg (what)

	# recuperation des noms de mesures qu'il faut 
	q <- list ()
	mesures <- xrGetMesures(conn, pattern, search.fields,
				campagnes, reseaux, stations, polluants,
				'IDENTIFIANT', collapse)
	q$mesures <- paste ("'", mesures$IDENTIFIANT, "'", sep='', collapse=', ')

	# données brutes ou pas ? Et quelle table aller taper ?
	if(validated)
		q$table <- switch (period,
				 qh = 'JOURNALIER', h = 'JOURNALIER', j = 'JOURNALIER',
				 m = 'MOIS', y = 'MOIS') else {
		if (period %in% c('m', 'y') )
			warning ("Il n'y a pas de valeurs brutes pour les mois et les années (period in 'm', 'y').")
		q$table <-  switch (period,
				 qh = 'JOURNALIER', h = 'JOURNALIER', j = 'JOURNALIER',
				 m = 'MOIS', y = 'MOIS')
	}

	# quels champs faut-il rappatrier ? (dépend de la table déterminée juste avant)
	#	Les champs de données et la date, et le nom_court_mes
	q$fields <- dbListFields (conn, q$table)
	q$fields <- lapply (c('DATE', switch (period, qh = 'Q', h = 'H', j = 'J', m = 'M', y = 'A')),
			  grep, dbListFields (conn, q$table), value=TRUE)
	q$fields <- unique (unlist (q$fields) )
	q$fields <- c('NOM_COURT_MES', setdiff (q$fields, 'Q_ETATB') )
	q$fields <- q$fields[!grepl ('PV_', q$fields)]

	q$date <- grep ('DATE', q$fields, value=TRUE)
	
	q$fields[grep ('DATE', q$fields)] <- sprintf ("TO_CHAR(%s, 'YYYY-MM-DD')", q$fields[grep ('DATE', q$fields)])
	q$fields <- paste (q$fields, collapse=', ')

	# start et end sont mis en forme pour la requete
	q$start <- format (start, format = '%Y-%m-%d', tz='UTC')
	q$start <- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$start)
	q$end	<- format (end, format = '%Y-%m-%d', tz='UTC')
	q$end	<- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$end)

	# la requete à proprement parler. C'est presque décevant :)
	query <- sprintf ('SELECT %s FROM %s WHERE NOM_COURT_MES IN (%s) AND %s BETWEEN %s AND %s',
			  q$fields, q$table, q$mesures, q$date, q$start, q$end)

	data <- xrGetQuery (conn, query)
	names (data)[2] <- 'DATE'

	# mise en forme des données
	period <- eval (parse (text = period) )	# period est évaluée au sens lubridate

	data <- split (data, data$NOM_COURT_MES)

	mef.mesure <- function(data, period, valid.states, what) {
		mesure <- unique (data[[grep ('NOM_COURT_MES', names (data))]])
		data <- data[-grep ('NOM_COURT_MES', names (data))]
		
		etats <- data[[grep ('ETAT', names (data))]]
		data <- data[-grep ('ETAT', names (data))]
		etats <- unlist (strsplit (etats, ''))

		dates <- unique (data[[grep ('DATE', names (data))]])
		data <- data[-grep ('DATE', names (data))]
		dates <- data.frame (
			start=rep (as.POSIXct(dates, tz='UTC'), each=length(data) ) + period * 0:(length(data)-1),
			end=rep (as.POSIXct(dates, tz='UTC'), each=length(data) ) + period * 1:(length(data)))

		if (what %in% c('value', 'both') ) {
			values <- c( t(data) )
			values[!etats %in% valid.states] <- NA
			values <- data.frame (values)
			names (values) <- mesure
		}
		if (what == 'state') {
			values <- data.frame (etats)
			names (values) <- mesure
		} else if (what == 'both') {
			values <- data.frame (values, etats)
			names (values) <- paste (mesure, c('value', 'state'), sep= '.')
		}
		return (data.frame (dates, values) )
	}
	
	result <- start + period * 1:((end-start)/as.duration (as.interval (period, start) ) )
	result <- data.frame (start = result-period, end = result)
	data <- lapply (data, mef.mesure, period=period, valid.states=valid.states, what=what)
	while (length (data) > 0) {
		result <- merge (result, data[[1]], all.x=TRUE, all.y=FALSE, by=c('start', 'end') )
		data[[1]] <- NULL
	}




}






