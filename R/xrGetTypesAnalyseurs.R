#' Pour chaque mesure, liste les installations des analyseurs.
#'
#' @param conn Connexion à la base XR.
#' @param x Une data.frame telle que retournée par
#'  \code{\link[Qair]{xrGetMesures}} avec au moins les colonnes 'NOPOL', 
#'  'NOM_COURT_MES'.
#' @param debut POSIXct indiquant la date de début de la période 
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @param fin POSIXct indiquant la date de fin de la période 
#'  pour laquelle on souhaite avoir la liste des analyseurs.
#' @return Retourne un  TimeInstantDataFrame dont chaque élément contient,
#'  pour une des mesures demandées, la liste des analyseurs utilisés et la
#'  date d'installation de l'analyseur.
xrGetTypesAnalyseurs <- function(conn, x, debut, fin) {
	requetes <- paste("select POLLUANT_APPAREIL.NOPOL,
		SUIVI_MESURE_APPAREIL.NOM_COURT_MES,NTYPAPP, TYPAPPAREIL.MODELE,
		SUIVI_MESURE_APPAREIL.DATE_CHANGEMENT,SUIVI_MESURE_APPAREIL.ETAT
		from SUIVI_MESURE_APPAREIL
		join APPAREIL using( NO_APPAREIL )
		join POLLUANT_APPAREIL using( NTYPAPP )
		join TYPAPPAREIL using ( NTYPAPP )
		where POLLUANT_APPAREIL.NOPOL='", x$NOPOL, "'
		and SUIVI_MESURE_APPAREIL.ETAT=1
		and SUIVI_MESURE_APPAREIL.NOM_COURT_MES='", x$NOM_COURT_MES, "'",
		sep='')

	analyseurs <- lapply(requetes, xrGetQuery, conn=conn)
	names( analyseurs ) <- x$NOM_COURT_MES

	for(i in 1:length(analyseurs)) {
		names(analyseurs[[i]])[
			names(analyseurs[[i]]) == 'DATE_CHANGEMENT'] <- 'date'

		analyseurs[[i]]$date <- as.POSIXct(analyseurs[[i]]$date, 'UTC')
		analyseurs[[i]]$date <- trunc(analyseurs[[i]]$date, 'hour')
		analyseurs[[i]]$date <- as.POSIXct(analyseurs[[i]]$date)
	}

	analyseurs <- lapply(analyseurs, function(x) x[x$date < fin,])
	analyseurs <- lapply(analyseurs, function(x) x[order(x$date),])
	analyseurs <- lapply(analyseurs, function(x) {
		d <- (which(x$date>debut)[1]-1)
		x <- x[ifelse(is.na(d), nrow(x), d):nrow(x),]
		})

	lapply(analyseurs, function(x)
		TimeInstantDataFrame(x$date, data=x['MODELE']))
}

