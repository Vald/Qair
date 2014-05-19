#' Fonction pour recuperer des donnees scan de XR
#'
#' @inheritParams xrGetContinuousData
#'
#' @return un objet de classe \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'   contenant les données demandées.
#'
#' @seealso \code{\link{xrGetContinuousData}},
#'	\code{\link{xrGetSitesPrelevement}}, \code{\link{xrGetMethodesPrelevement}},
#'	 \code{\link{xrGetCampagnes}}, \code{\link{xrGetPolluants}}
#' 	\code{\link[timetools]{TimeIntervalDataFrame-class}}

xrGetScan <- function (conn, pattern=NULL, start, end,
		    search.fields, campagnes = NULL, reseaux = NULL,
		    stations = NULL, polluants = NULL,
		    collapse = c('AND', 'OR'), XR6 = TRUE,
		    tz='UTC', cursor=NULL, exact=FALSE) {
	# period est un periode au sens lubridate
	# start et end doivent être des POSIXt (pour la prise en compte des
	# timezones).
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

	# pour permettre eventuellement d'entrer des chaines de caracteres en
	# start en end on fait un petit cast
	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	# recuperation des noms de mesures qu'il faut 
	q <- list ()
	mesures <- xrGetMesures(conn=conn, pattern=pattern,
				search.fields=search.fields,
				campagnes=campagnes, reseaux=reseaux,
				stations=stations, polluants=polluants,
				collapse=collapse, exact=exact)
	q$mesures <- paste ("'", mesures$NOM_COURT_MES, "'", sep='', collapse=', ')

	champs <- paste("S_M", sprintf("%02i", 1:60), sep="", collapse=", ")
	query <- paste("SELECT NOM_COURT_MES, TO_CHAR(S_DATE, 'YYYY-MM-DD HH24:MI'), ",
		       champs, ", NOMBRE_SCAN ",
		       "FROM SCAN WHERE NOM_COURT_MES IN (", q$mesures, ") AND ",
		       "S_DATE BETWEEN TO_DATE('", start, "', 'YY-MM-DD') AND ",
		       "TO_DATE('", end, "', 'YY-MM-DD')", sep="")

	donnees <- xrGetQuery (conn, query)

	# conversion (au cas où)
	a.convertir <- grep('S_M', names(donnees))
	donnees[a.convertir] <- lapply(donnees[a.convertir], as.numeric)

	# mise en forme des donnees
	names(donnees)[1:2] <- c("nom_court_mes", "date")
	donnees[1:2] <- lapply(donnees[1:2], as.character)
	donnees <- split(donnees[-1], donnees$nom_court_mes)
	donnees <- lapply(donnees, as.list)

	for( i in names(donnees) ) {
		temp <- as.POSIXct(donnees[[i]]$date, tz='UTC')
		donnees[[i]]$NOMBRE_SCAN <- c(as.numeric(temp[-1] - temp[-length(temp)])*6, 60)
		donnees[[i]]$NOMBRE_SCAN <- ifelse(donnees[[i]]$NOMBRE_SCAN > 60, 60, donnees[[i]]$NOMBRE_SCAN)

		selection <- unlist(mapply("+",
					   mapply(":", 1, donnees[[i]]$NOMBRE_SCAN, SIMPLIFY=FALSE),
					   (1:length(donnees[[i]]$NOMBRE_SCAN)-1)*60,
					   SIMPLIFY=FALSE))

		donnees[[i]]$mes <- c(t(as.data.frame(donnees[[i]][setdiff(names(donnees[[i]]), c("date", "NOMBRE_SCAN"))])))[selection]

		donnees[[i]]$date <- rep(as.POSIXct(donnees[[i]]$date, tz='UTC'), each=60)
		donnees[[i]]$date <- donnees[[i]]$date + rep(0:59*10, length.out=length(donnees[[i]]$date))
		donnees[[i]]$date <- donnees[[i]]$date[selection]
	}

	temp <- lapply(lapply(donnees, "[", c("date", "mes")), as.data.frame)
	donnees <- data.frame(date=temp[[1]]$date, temp[[1]]$mes)
	names(donnees)[2] <- names(temp)[1]
	if(length(temp)>1) {
		for(i in 2:length(temp)) {
			donnees <- merge(donnees, data.frame(date=temp[[i]]$date, temp[[i]]$mes), by="date", all=TRUE)#}
			names(donnees)[i+1] <- names(temp)[i]
		}
	}

	donnees <- donnees[donnees$date >= start & donnees$date <= end, ]

	dates <- donnees$date
	result <- donnees[setdiff(names(donnees), 'date')]

	result <- new('TimeIntervalDataFrame',
		      start=dates,
		      end=c(dates[-1], dates[length(dates)]+10*POSIXctp('second')),
		      timezone='UTC',
		      data=result)

	if( !is.null(cursor) )
		result <- as.TimeInstantDataFrame(result, cursor)

	timezone(result) <- tz
	return (result)
}

