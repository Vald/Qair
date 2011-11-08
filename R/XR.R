xrGetQuery <- function (conn, query) {
	if(.Platform$OS.type=="windows") {
		sqlQuery(conn, query)
	} else if(.Platform$OS.type=="unix") {
		dbGetQuery(conn, query)
	} else stop ('Unrecognized platform oO.')

}

xrConnect <- function(dsn=NULL, uid=NULL, pwd=NULL, host=NULL, ojdbc=NULL) {
	# host et ojdbc sont a specifier uniquement dans le cas de l'utilisation de RJDBC

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

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	unique (xrGetQuery (conn, query) )
}

	mef.mesure <- function(data, identifiant, period, valid.states, what, XR6, fmul) {
		mesure <- identifiant
		ncm <- unique (data[[grep ('NOM_COURT_MES', names (data))]])
		data <- data[setdiff (names (data), c('NOM_COURT_MES') )]
		
		etats <- data[[grep ('ETAT', names (data))]]
		data <- data[-grep ('ETAT', names (data))]
		etats <- unlist (strsplit (etats, ''))

		dates <- unique (data[[grep ('DATE', names (data))]])
		data <- data[-grep ('DATE', names (data))]
		tmp <- paste (rep (dates, each=length(data) ), names (data) )
		tmp <- c(paste (as.character(as.POSIXct(dates[1]) - 
					     switch (period, Q_M01=d, H_M01=d, J_M01=d, M_M01=m, A_M01=m)),
				names (data)[length(data)] ),
			 tmp)
		dates <- data.frame (start=tmp[-length(tmp)], end=tmp[-1])

		if (what %in% c('value', 'both') ) {
			values <- c( t(data) )
			values[!etats %in% valid.states] <- NA
			values <- data.frame (values)
			if (!XR6) values <- values*10^fmul
			names (values) <- mesure
		}
		if (what == 'state') {
			values <- data.frame (etats)
			names (values) <- mesure
		} else if (what == 'both') {
			values <- data.frame (values, etats)
			names (values) <- paste (mesure, c('value', 'state'), sep= '.')
		}
		values <- data.frame (dates, values)
		attributes(values)$NOM_COURT_MES <- ncm
		return (values)
	}


xrGetContinuousData <- function (conn, pattern=NULL, start, end,
		       period = c('h', 'qh', 'd', 'm', 'y'),
		       validated = TRUE, valid.states = c("A", "R", "O", "W"), what = c('value', 'state', 'both'),
		       search.fields='IDENTIFIANT', campagnes = NULL, reseaux = NULL, stations = NULL, polluants = NULL,
		       collapse = c('AND', 'OR'), XR6 = TRUE) {
	# period est un periode au sens lubridate
	# start et end doivent être des POSIXt (pour la prise en compte des timezones).
	period <- match.arg (period)
	what <- match.arg (what)

	# pour permettre eventuellement d'entrer des chaines de caracteres en start en end
	#	on fait un petit cast
	if (!is.POSIXt (start) ) start <- as.POSIXct (start, tz = 'UTC')
	if (!is.POSIXt (end) ) end <- as.POSIXct (end, tz = 'UTC')

	# recuperation des noms de mesures qu'il faut 
	q <- list ()
	mesures <- xrGetMesures(conn, pattern, search.fields,
				campagnes, reseaux, stations, polluants, collapse=collapse)
	q$mesures <- paste ("'", mesures$NOM_COURT_MES, "'", sep='', collapse=', ')

	# donnees brutes ou pas ? Et quelle table aller taper ?
	if(validated)
		q$table <- switch (period,
				 qh = 'JOURNALIER', h = 'JOURNALIER', d = 'JOURNALIER',
				 m = 'MOIS', y = 'MOIS') else {
		if (period %in% c('m', 'y') )
			warning ("Il n'y a pas de valeurs brutes pour les mois et les annees (period in 'm', 'y').")
		q$table <-  switch (period,
				 qh = 'BRUTE', h = 'BRUTE', d = 'BRUTE',
				 m = 'MOIS', y = 'MOIS')
	}

	# quels champs faut-il rappatrier ? (depend de la table determinee juste avant)
	#	Les champs de donnees et la date, et le nom_court_mes
	q$fields.l <- dbListFields (conn, q$table)
	q$fields.l <- lapply (c('DATE', switch (period, qh = 'Q_', h = 'H_', d = 'J_', m = 'M_', y = 'A_')),
			  grep, dbListFields (conn, q$table), value=TRUE)
	q$fields.l <- unique (unlist (q$fields.l) )
	q$fields.l <- c('NOM_COURT_MES', setdiff (q$fields.l, 'Q_ETATB') )
	q$fields.l <- unique (unlist (q$fields.l) )
	q$fields.l <- q$fields.l[!grepl ('PV_', q$fields.l)]
	q$fields <- q$felds.l

	q$date <- grep ('DATE', q$fields, value=TRUE)
	
	q$fields[grep ('DATE', q$fields)] <- sprintf ("TO_CHAR(%s, 'YYYY-MM-DD')", q$fields[grep ('DATE', q$fields)])
	q$fields <- paste (q$fields, collapse=', ')

	# start et end sont mis en forme pour la requete
	q$start <- format (start, format = '%Y-%m-%d', tz='UTC')
	q$start <- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$start)
	q$end	<- format (end, format = '%Y-%m-%d', tz='UTC')
	q$end	<- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$end)

	# la requete a proprement parler. C'est presque decevant tellement ça devient lisible :)
	query <- sprintf ('SELECT %s FROM %s WHERE NOM_COURT_MES IN (%s) AND %s BETWEEN %s AND %s',
			  q$fields, q$table, q$mesures, q$date, q$start, q$end)

	data <- xrGetQuery (conn, query)
	names (data)[2] <- 'DATE'

	# mise en forme des donnees
	q$period <- eval (parse (text = period) )	# period est evaluee au sens lubridate
	q$periodb <- switch (period, qh = 'Q_M01',	# period est evaluee  de façon bidon pour mef.
			    h = 'H_M01', d = 'J_M01', m = 'M_M01', y = 'A_M01')

	data <- split (data, data$NOM_COURT_MES)

	m <- match (names (data), mesures$NOM_COURT_MES)
	data <- mapply (mef.mesure, data, mesures$IDENTIFIANT[m], fmul=mesures$FMUL[m], SIMPLIFY = FALSE,
			MoreArgs = list (period=q$periodb, valid.states=valid.states, what=what, XR6=XR6))
	ncm <- unlist (lapply (data, attr, 'NOM_COURT_MES') )
	
	tmp <- seq (as.POSIXct(format (start, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		    as.POSIXct(format (end, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		    switch (period, qh='day', h='day', j='day', 'year') )
	tmp <- format (tmp, '%Y-%m-%d')
	tmp2 <- grep (switch (period, qh = 'Q_M', h = 'H_M', d = 'J_M', m = 'M_M', y = 'A_M'), 
		      q$fields.l, value=TRUE)
	tmp <- paste (rep (tmp, each=length(tmp2) ), tmp2)
	tmp <- c(paste (as.character(as.POSIXct(tmp[1]) -
				     switch (period, qh = d, h = d, d = d, m = m, y = m)),
			tmp2[length(tmp2)]),
		 tmp)
	result <- data.frame (start=tmp[-length(tmp)], end=tmp[-1])
	while (length (data) > 0) {
		result <- merge (result, data[[1]], all.x=TRUE, all.y=FALSE, by=c('start', 'end') )
		data[[1]] <- NULL
	}
	result$start <- result$end <- NULL
	rm (data)

	for (i in setdiff(mesures$NOM_COURT_MES, names(result) ) )
		result[[i]] <- NA
	result <- result[mesures$NOM_COURT_MES]

	# recuperation des infos
	q$stations <- xrGetStations (conn, pattern=mesures$NOM_COURT_SIT, search.fields='NOM_COURT_SIT')
	q$stations <- q$stations[match(mesures$NOM_COURT_SIT, q$stations$NOM_COURT_SIT),]
	q$polluants <- xrGetPolluants (conn, pattern=mesures$NOPOL, search.fields='NOPOL')
	q$polluants <- q$polluants[match(mesures$NOPOL, q$polluants$NOPOL),]
	
	q$attr.mesures <- cbind (
		q$stations[, c('LAMBERTX', 'LAMBERTY', 'NSIT', 'NINSEE', 'ISIT', 'NOM_COURT_SIT', 'IDENTIFIANT')],
		q$polluants[, c('CCHIM', 'NCON', 'NOPOL')])
	row.names (q$attr.mesures) <- attributes(result)$NOM_COURT_MES
	
	for (i in 1:length(result) )
		if (!any (is.na (q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')]) ) )
		attr(result[[i]], 'station') <- SpatialPointsDataFrame (
			q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')],
			q$attr.mesures[i,setdiff(names(q$attr.mesures), c('LAMBERTX', 'LAMBERTY'))],
			proj4string = CRS('+init=epsg:27572') )

		to <- as.POSIXct(format (end, format = '%Y-%m-%d', tz='UTC'), tz='UTC')
		np.arg <- list(1)
		names(np.arg) <- switch (period, qh = 'day', h = 'day', d = 'day', m = 'month', y = 'month')
		to <- to + do.call (new_period, np.arg)
		#                 to <- to + switch (period, qh = d, h = d, d = d, m = m, y = m)

	dates <- seq (as.POSIXct(format (start, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		      to,
		      switch (period, qh='15 mins', h='hour', d='day', m='month', y='year') )

	result <- new('TimeIntervalDataFrame', start=dates[-length(dates)], end=dates[-1], timezone='UTC',
		      data=result)

	return (result)
}

## pour s'inspirer quand on fera du TS*DataFrame
# print.TimeIntervalDataFrame <- function (x, ...) {
#         to.print <- data.frame (attributes(x)$dates, unclass (x), stringsAsFactors=FALSE)
#         to.print <- data.frame (lapply (to.print, function(x) ifelse (is.na(x), 'NA', as.character(x) ) ), stringsAsFactors=FALSE)
# 
#         to.print.tmp <- attributes(x)$mesures
#         to.print.tmp <- data.frame (start='', end=c('', names(to.print.tmp)),
#                                     t(data.frame (rep ('', nrow(to.print.tmp) ), to.print.tmp@data, stringsAsFactors=FALSE) ),
#                                     stringsAsFactors=FALSE)
#         rownames (to.print.tmp) <- NULL
#         to.print <- rbind (to.print, to.print.tmp, stringsAsFactors=FALSE)
# 
#         to.print.tmp <- coordinates(attributes(x)$mesures)
#         to.print.tmp <- data.frame (start = '',
#                                     end = c('', colnames(to.print.tmp) ),
#                                     t(data.frame (rep('', nrow(to.print.tmp) ), to.print.tmp) ),
#                                     stringsAsFactors=FALSE)
#         rownames (to.print.tmp) <- NULL
#         colnames (to.print.tmp) <- names (to.print)
#         to.print <- rbind (to.print, to.print.tmp, stringsAsFactors=FALSE)
# 
#         print (to.print)
# }


Xair2R <- function (polluants, dated, datef, dt = c("qh", "heure", "jour", "mois", "an"),
		    merging, codeV=c("A", "R", "O", "W"),
		    dsn=NULL, uid=NULL, pwd=NULL, brute=FALSE,
		    reseaux=NULL, stations=NULL, campagnes=NULL,
		    host=NULL, keep.state=FALSE, XR6=TRUE)
{
	warning ("La fonction Xair2R est obsolete. il est preferable d'utiliser la fonction xrGetContinuousData.")

	conn <- xrConnect(dsn=dsn, uid=uid, pwd=pwd, host=host)
	result <- xrGetContinuousData (conn, pattern = polluants, start = dated, end = datef,
			     period = switch (dt, qh='qh', heure='h', jour='d', mois='m', an='y'),
			     validated = !brute, valid.states = codeV, what = ifelse(keep.state, 'both', 'value'),
			     campagnes = campagnes, reseaux = reseaux, stations = stations, XR6 = XR6)
	dbDisconnect (conn)
	return (result)
}

# library (maptools)
# library (rgdal)
# 
# conn <- xrConnect()
# test <- xrGetContinuousData (conn, c('N2_VER', 'N2_VAU'), '2010-01-01', '2011-02-02', period='m')
# dbDisconnect (conn)

# library (Qair)
# a deplacer dans un fichier zzz.R
# library (lubridate)
# library (RJDBC)
# library (maptools)
# library (timetools)
qh <- new_period (minutes=15)
h <- new_period (hours=1)
#---------------------------------

