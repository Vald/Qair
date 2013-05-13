#' Recuperation de mesure continue dans une base XR
#'
#' La fonction permet de récupérer une ou plusieurs séries 
#' de données stockée(s) dans une base XR entre 2 dates données
#' au format voulu (heure, mois, etc.). Les données sont retournées
#' au format \code{\link[timetools]{TimeIntervalDataFrame-class}} défini dans le 
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
#' @param conn une connexion valide telle que retournée par \code{\link{xrConnect}}.
#' @param pattern chaînes de caractères utilisées pour la recherche (cf \sQuote{Details}).
#' @param start date initiale de la période à rapatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param end date de fin de la période à rapatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param period un des éléments suivant ('h', 'qh', 'd', 'm', 'y') précisant la période
#' 	des données à rapatrier.
#' @param validated booléen. Si TRUE, les données sont récupérées dans la table normale
#'	de stockage de XR ; si FALSE, les données sont récupérées dans la table \code{BRUTE}.
#' @param valid.states liste des codes états d'XR à considérer comme valide. Par défaut
#'	les données dont le code état est'A', 'R', 'O', 'W' ou 'P' sont considérées comme valides.
#'	les autres sont remplacées par NA.
#' @param what un des éléments suivant ('value', 'state', 'both'). value : seules
#'	les valeurs sont récupérées ; state : seuls les codes états sont récupérées ;
#'	both : les deux sont rapatriés et un suffixe est ajouté au nom des séries pour
#' 	les différencier.
#' @param search.fields champ de la table dans lesquels \code{pattern} doit être
#' 	recherché.
#' @param campagnes chaînes de caractères correspondant aux campagnes à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetCampagnes}}).
#' @param reseaux chaînes de caractères correspondant aux réseaux à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetReseaux}}).
#' @param stations chaînes de caractères correspondant aux stations à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetStations}}).
#' @param polluants chaînes de caractères correspondant aux polluants à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetPolluants}}).
#' @param collapse conjonction à appliquer entre les différents critères de recherche
#'	indiqués.
#' @param XR6 TRUE si la version de XR est supérieure ou égale à 6, FALSE sinon.
#' @param tz timezone dans laquelle les données doivent être retournées. Si les dates
#' 	debut et fin sont des chaînes de caractères, tz est utilisé pour les définir.
#' @param cursor détermine le format de sortie des données : NULL pour un 
#' 	TimeIntervalDataFrame (par défaut) une valeur entre 0 et 1 pour un 
#' 	TimeInstantDataFrame (1 pour respecter les conventions XR).
#' @param exact booléen indiquant si les mesures à rapatrier doivent être 
#' 	exactement identiques à \sQuote{pattern} ou si \sQuote{pattern} doit 
#' 	être utilisé dans une expression régulière.
#'
#' @return un objet de classe \code{\link[timetools]{TimeIntervalDataFrame-class}}
#' 	contenant les données demandées.
#'
#' @seealso \code{\link{xrGetMesures}}, \code{\link{xrGetCampagnes}}, \code{\link{xrGetReseaux}},
#'  \code{\link{xrGetStations}}, \code{\link{xrGetPolluants}}, \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'	\code{\link{xrGetManualData}}

xrGetContinuousData <- function (conn, pattern=NULL, start, end,
		       period = c('h', 'qh', 'd', 'm', 'y'),
		       validated = TRUE, valid.states = c("A", "R", "O", "W", "P"), what = c('value', 'state', 'both'),
		       search.fields, campagnes = NULL, reseaux = NULL, stations = NULL, polluants = NULL,
		       collapse = c('AND', 'OR'), XR6 = TRUE,
		       tz='UTC', cursor=NULL, exact=FALSE) {
	# period est un periode au sens lubridate
	# start et end doivent être des POSIXt (pour la prise en compte des timezones).
	period <- match.arg (period)
	what <- match.arg (what)
	collapse <- match.arg (collapse)

	# champ de recherche par défaut selonc que l'on est en V6 ou non
	if (XR6) {
		id.field <- 'IDENTIFIANT'
		if (missing (search.fields))
			search.fields <- 'IDENTIFIANT'
	} else {
		id.field <- 'NOM_COURT_MES'
		if (missing (search.fields))
			search.fields <- 'NOM_COURT_MES'
	}

	# pour permettre eventuellement d'entrer des chaines de caracteres en start en end
	#	on fait un petit cast
	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	real.start <- start
	real.end <- end

	#start <- floor_date (start, switch(period, qh='day', h='day', d='day', m='year', y='year') )
	start <- if (period %in% c('qh', 'h', 'd'))
			 as.POSIXct(format(start, '%Y-%m-%d'), attributes(start)$tzone) else
			 as.POSIXct(sprintf('%s-01-01', format(start, '%Y')), attributes(start)$tzone)
	#end <- ceiling_date (end, switch(period, qh='day', h='day', d='day', m='year', y='year') )
	end <- if (period %in% c('qh', 'h', 'd')) 
		       as.POSIXct(format(end, '%Y-%m-%d'), attributes(end)$tzone) + POSIXctp(unit='day') else 
		       as.POSIXct(sprintf('%s-01-01', format(end, '%Y')), attributes(end)$tzone) + POSIXctp(unit='year')

	# recuperation des noms de mesures qu'il faut 
	q <- list ()
	mesures <- xrGetMesures(conn=conn, pattern=pattern, search.fields=search.fields,
				campagnes=campagnes, reseaux=reseaux, stations=stations,
				polluants=polluants, collapse=collapse, exact=exact)
	q$mesures <- paste ("'", mesures$NOM_COURT_MES, "'", sep='', collapse=', ')

	# donnees brutes ou pas ? Et quelle table aller taper ?
	if(validated) {
		q$table <- switch (period,
				 qh = 'JOURNALIER', h = 'JOURNALIER', d = 'JOURNALIER',
				 m = 'MOIS', y = 'MOIS')
	} else {
		if (period %in% c('m', 'y') )
			warning ("Il n'y a pas de valeurs brutes pour les mois et les annees (period in 'm', 'y').")
		q$table <-  switch (period,
				 qh = 'BRUTE', h = 'BRUTE', d = 'BRUTE',
				 m = 'MOIS', y = 'MOIS')
	}

	# quels champs faut-il rapatrier ? (depend de la table determinee juste avant)
	#	Les champs de donnees et la date, et le nom_court_mes
	q$fields.l <- dbListFields (conn, q$table, schema='RSDBA')
	q$fields.l <- lapply (c('DATE', switch (period, qh = 'Q_', h = 'H_', d = 'J_', m = 'M_', y = 'A_')),
			  grep, dbListFields (conn, q$table, schema='RSDBA'), value=TRUE)
	q$fields.l <- unique (unlist (q$fields.l) )
	q$fields.l <- c('NOM_COURT_MES', setdiff (q$fields.l, 'Q_ETATB') )
	q$fields.l <- unique (unlist (q$fields.l) )
	q$fields.l <- q$fields.l[!grepl ('PV_', q$fields.l)]
	q$fields <- q$fields.l

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
	#q$period <- eval (parse (text = period) )	# period est evaluee au sens lubridate
	q$periodb <- switch (period, qh = 'Q_M01',	# period est evaluee  de façon bidon pour mef.
			    h = 'H_M01', d = 'J_M01', m = 'M_M01', y = 'A_M01')

	data <- split (data, data$NOM_COURT_MES)

	m <- match (names (data), mesures$NOM_COURT_MES)
	data <- mapply (mef.mesure, data, mesures[[id.field]][m], fmul=mesures$FMUL[m], SIMPLIFY = FALSE,
			MoreArgs = list (period=q$periodb, valid.states=valid.states, what=what, XR6=XR6))
	# ncm <- unlist (lapply (data, attr, 'NOM_COURT_MES') ) # ? a quoi ca sert ? À rien ?
	for (i in names(data) ) names (data[[i]])[3] <- i
	
	tmp <- seq (as.POSIXct(format (start, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		    as.POSIXct(format (end, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		    switch (period, qh='day', h='day', d='day', 'year') )
	tmp <- format (tmp, '%Y-%m-%d')
	tmp2 <- grep (switch (period, qh = 'Q_M', h = 'H_M', d = 'J_M', m = 'M_M', y = 'A_M'), 
		      q$fields.l, value=TRUE)
	tmp <- paste (rep (tmp, each=length(tmp2) ), tmp2)
	if (period %in% c('qh', 'h') )
		tmp <- c(paste (as.character(as.POSIXct(tmp[1]) -
				     switch (period,
					     qh = POSIXctp(unit='day'),
					     h = POSIXctp(unit='day'))),
			tmp2[length(tmp2)]),
		 tmp) else if (period %in% c('d', 'm', 'y') )
		tmp <- c(tmp,
			 paste (as.character(as.POSIXct(tmp[length(tmp)]) +
				     switch (period,
					     d = POSIXctp(unit='day'),
					     m = POSIXctp(unit='month'),
					     y = POSIXctp(unit='year'))),
			tmp2[1]))
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
	names (result) <- mesures[[id.field]][match(names(result), mesures$NOM_COURT_MES)]

	# recuperation des infos
	q$stations <- xrGetStations (conn, pattern=mesures$NOM_COURT_SIT, search.fields='NOM_COURT_SIT')
	q$stations <- q$stations[match(mesures$NOM_COURT_SIT, q$stations$NOM_COURT_SIT),]
	q$polluants <- xrGetPolluants (conn, pattern=mesures$NOPOL, search.fields='NOPOL')
	q$polluants <- q$polluants[match(mesures$NOPOL, q$polluants$NOPOL),]
	
	q$attr.mesures <- cbind (
		q$stations[, intersect(names(q$stations), c('LAMBERTX', 'LAMBERTY', 'NSIT_PUBLIC', 'NINSEE', 'ISIT', 'NOM_COURT_SIT', 'IDENTIFIANT'))],
		q$polluants[, c('CCHIM', 'NCON', 'NOPOL')])
	row.names (q$attr.mesures) <- attributes(result)$NOM_COURT_MES
	
	#         for (i in 1:length(result) )
	#                 if (!any (is.na (q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')]) ) )
	#                 attr(result[[i]], 'station') <- SpatialPointsDataFrame (
	#                         q$attr.mesures[i,c('LAMBERTX', 'LAMBERTY')],
	#                         q$attr.mesures[i,setdiff(names(q$attr.mesures), c('LAMBERTX', 'LAMBERTY'))],
	#                         proj4string = CRS('+init=epsg:27572') )

		to <- as.POSIXct(format (end, format = '%Y-%m-%d', tz='UTC'), tz='UTC')
		#np.arg <- list(1)
		np.arg <- switch (period, qh = 'day', h = 'day', d = 'day', m = 'year', y = 'year')
		to <- to + POSIXctp(unit=np.arg)#do.call (new_period, np.arg)

	dates <- seq (as.POSIXct(format (start, format = '%Y-%m-%d', tz='UTC'), tz='UTC'),
		      to,
		      switch (period, qh='15 mins', h='hour', d='day', m='month', y='year') )

	names (result) <- sprintf ('%s%s', ifelse (substr(names(result), 1, 1) %in% as.character(0:9),
						   'X', ''), names(result) )
	result <- new('TimeIntervalDataFrame', start=dates[-length(dates)], end=dates[-1], timezone='UTC',
		      data=result)
	
	result <- result[start(result) >= real.start & end(result) <= real.end,]
	if( !is.null(cursor) )
		result <- as.TimeInstantDataFrame(result, cursor)
	timezone(result) <- tz
	return (result)
}


mef.mesure <- function(data, identifiant, period, valid.states, what, XR6, fmul) {
	mesure <- identifiant
	ncm <- unique (data[[grep ('NOM_COURT_MES', names (data))]])
	data <- data[setdiff (names (data), c('NOM_COURT_MES') )]

	# attention, l'ordre de récupération/affectation de chaque 'type' de 
	# données est primordial. Sinon la mise en forme n'est pas correcte.
	
	# extraction des dates à partir de data, détermination de l'ordre
	# temporel des données et ré-ordonnage des dates
	dates <- unique (data[[grep ('DATE', names (data))]])
	data <- data[-grep ('DATE', names (data))]

	ordre <- order(dates)

	dates <- dates[ordre]

	# extraction des etats à partir de data et ré-ordonnage temporel
	# à partir de l'ordre déterminé ci-dessus
	etats <- data[[grep ('ETAT', names (data))]]
	data <- data[-grep ('ETAT', names (data))]

	etats <- etats[ordre]
	etats <- unlist (strsplit (etats, ''))

	# ordonnage temporel de data

	data <- data[ordre,,drop=FALSE]

	tmp <- paste (rep (dates, each=length(data) ), names (data) )
	if (period %in% c('Q_M01', 'H_M01') )
		tmp <- c(paste (as.character(as.POSIXct(dates[1]) -
					     switch (period,
						     Q_M01 = POSIXctp(unit='day'),
						     H_M01 = POSIXctp(unit='day'))),
				names(data)[length(data)]),
			 tmp) else if (period %in% c('J_M01', 'M_M01', 'A_M01') )
		tmp <- c(tmp,
			 paste (as.character(as.POSIXct(dates[length(dates)]) +
					     switch (period,
						     J_M01 = POSIXctp(unit='day'),
						     M_M01 = POSIXctp(unit='month'),
						     A_M01 = POSIXctp(unit='year'))),
				names(data)[1]) )
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

