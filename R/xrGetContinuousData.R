#' Recuperation de mesure continue dans une base XR
#'
#' La fonction permet de récupérer une ou plusieurs séries 
#' de données stockée(s) dans une base XR entre 2 dates données
#' au format voulu (heure, mois, etc.). Les données sont retournées
#' au format \code{\link[timetools]{TimeIntervalDataFrame}} défini dans le 
#' paquet \code{\link[timetools]{timetools}}.
#'
#' L'extraction des séries de données peut être réalisée en recherchant
#' une chaîne de caractères (définie par \code{pattern}) sur 
#' un ou plusieurs champs de la table \code{MESURE} (la fonction 
#' \code{\link{xrGetMesures}} est alors utilisée). Les champs peuvent
#' être précisés par l'argument \code{search.fields}.
#'
#' Il est également possible de récupérer plusieurs séries de données
#' à partir des réseaux, campagnes, stations ou polluants définis dans XR.
#' Il suffit pour
#' cela d'indiquer une ou plusieurs chaînes dans les arguments
#' \code{campagnes}, \code{reseaux}, \code{stations} ou \code{polluants}.
#' Ces arguments sont alors utilisés dans les fonctions \code{xrGet*}
#' adéquates, soit respectivement \code{\link{xrGetCampagnes}}, \code{\link{xrGetReseaux}},
#'  \code{\link{xrGetStations}} et  \code{\link{xrGetPolluants}}.
#'
#' Si plusieurs de ces arguments sont renseignés, la conjonction entre les
#' différents critères est précisée par l'argument \code{collapse}.
#'
#'
#' @param conn une connection valide telle que retournée par \code{\link{xrConnect}}.
#' @param pattern chaînes de caractères utilisées pour la recherche (cf \sQuote{Details}).
#' @param start date initiale de la période à rappatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param end date de fin de la période à rappatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param period un des éléments suivant ('h', 'qh', 'd', 'm', 'y') précisant la période
#' 	des données à rappatrier.
#' @param validated booléen. Si TRUE, les données sont récupérées dans la table normale
#'	de stockage de XR ; si FALSE, les données sont récupérées dans la table \code{BRUTE}.
#' @param valid.states liste des codes états d'XR à considérer comme valide. Par défaut
#'	les données dont le code état est'A', 'R', 'O' ou 'W' sont considérées comme valides.
#'	les autres sont remplacées par NA.
#' @param what un des éléments suivant ('value', 'state', 'both'). value : seules
#'	les valeurs sont récupérées ; state : seuls les codes états sont récupérées ;
#'	both : les deux sont rappatriés et un suffixe est ajouté au nom des séries pour
#' 	les différencier.
#' @param search.fields champ de la table dans lesquels \code{pattern} doit être
#' 	recherché.
#' @param campagnes chaînes de caractères correspondant aux campagnes à rappatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetCampagnes}}).
#' @param reseaux chaînes de caractères correspondant aux réseaux à rappatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetReseaux}}).
#' @param stations chaînes de caractères correspondant aux stations à rappatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetStations}}).
#' @param polluants chaînes de caractères correspondant aux polluants à rappatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetPolluants}}).
#' @param collapse conjonction à appliquer entre les différents critères de recherche
#'	indiqués.
#' @param XR6 TRUE si la version de XR est supérieure ou égale à 6, FALSE sinon.
#'
#' @return un objet de classe \code{TimeIntervalDataFrame}
#' 	contenant les données demandées.
#'
#' @seealso \code{\link{xrGetMesures}}, \code{\link{xrGetCampagnes}}, \code{\link{xrGetReseaux}},
#'  \code{\link{xrGetStations}}, \code{\link{xrGetPolluants}}, \code{\link[timetools]{TimeIntervalDataFrame}}

xrGetContinuousData <- function (conn, pattern=NULL, start, end,
		       period = c('h', 'qh', 'd', 'm', 'y'),
		       validated = TRUE, valid.states = c("A", "R", "O", "W"), what = c('value', 'state', 'both'),
		       search.fields='IDENTIFIANT', campagnes = NULL, reseaux = NULL, stations = NULL, polluants = NULL,
		       collapse = c('AND', 'OR'), XR6 = TRUE) {
	# period est un periode au sens lubridate
	# start et end doivent être des POSIXt (pour la prise en compte des timezones).
	period <- match.arg (period)
	what <- match.arg (what)
	collapse <- match.arg (collapse)

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
	ncm <- unlist (lapply (data, attr, 'NOM_COURT_MES') ) # ? a quoi ca sert ?
	for (i in names(data) ) names (data[[i]])[3] <- i
	
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
	
	#         for (i in 1:length(result) )
	#                 if (!any (is.na (q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')]) ) )
	#                 attr(result[[i]], 'station') <- SpatialPointsDataFrame (
	#                         q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')],
	#                         q$attr.mesures[i,setdiff(names(q$attr.mesures), c('LAMBERTX', 'LAMBERTY'))],
	#                         proj4string = CRS('+init=epsg:27572') )

		to <- as.POSIXct(format (end, format = '%Y-%m-%d', tz='UTC'), tz='UTC')
		np.arg <- list(1)
		names(np.arg) <- switch (period, qh = 'day', h = 'day', d = 'day', m = 'month', y = 'month')
		to <- to + do.call (new_period, np.arg)
		#                 to <- to + switch (period, qh = d, h = d, d = d, m = m, y = m)

	dates <- seq (as.POSIXct(format (start, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		      to,
		      switch (period, qh='15 mins', h='hour', d='day', m='month', y='year') )

	names (result) <- sprintf ('%s%s', ifelse (!is.na(as.numeric(substr(names(result), 1, 1))), 'X', ''), names(result) )
	result <- new('TimeIntervalDataFrame', start=dates[-length(dates)], end=dates[-1], timezone='UTC',
		      data=result)

	return (result)
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

