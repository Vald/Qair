
tidf <- function(debut, fin) {
	tidf <- RegularTimeIntervalDataFrame(debut, fin+POSIXctp('day'), 'day')
	tidf$SO2 <- tidf$PM10 <- tidf$O3 <- tidf$NO2 <- tidf$indice <- NA
	return (tidf)
}

indicesXR2Rlong <- function(xr, debut, fin, agglos, type='C') {
	debut	<- as.POSIXct(debut, 'UTC')-POSIXctp(2, 'day')
	fin		<- as.POSIXct(fin, 'UTC')

	# récupération de l'indice complet ----------------------------------------

	i <- try(indicesXR2R(xr, agglos, debut, fin, TRUE, 'agglo'), silent=TRUE)
	if (type == 'P') iP <-
		 try(indicesXR2R(xr, agglos, debut, fin, TRUE, 'agglo', type='P'), silent=TRUE)

	# création d'une structure par défaut si pas d'indice complet -------------

	if (inherits(i, 'try-error'))
		i <- lapply(agglos, function(x) tidf(debut, fin))

	# récupération des autres valeurs d'indices -------------------------------
	# et création de quelques identifiants

	si <- xrGetQuery(xr, 
		sprintf("SELECT NOM_AGGLO, J_DATE, C_IND_PRV_J, C_POL_PRV_J, C_IND_PRV_J1,
				P_IND_DIFFUSE, P_IND_PRV_J, P_POL_PRV_J, P_IND_PRV_J1
			FROM RESULTAT_INDICE JOIN
			GROUPE_ATMO USING (NOM_COURT_GRP)
			WHERE NOM_AGGLO IN('%s') AND
			J_DATE BETWEEN TO_DATE ('%s', 'YYYY-MM-DD') AND
			TO_DATE ('%s', 'YYYY-MM-DD')",
		paste(agglos, collapse="', '"), debut, fin))

	si$date <- strftime(si$J_DATE, '%Y-%m-%d')
	si$nom  <- si$NOM_AGGLO

	# mise en forme de la table d'indice complet

	i <- mapply(function(n, i) {
			if (!fin %in% start(i))
				i <- rbind(i, tidf(end(i)[nrow(i)], fin))

			i[['nom']] <- n
			i[['date']] <- strftime(start(i), '%Y-%m-%d')
			return (i)
		}, names(i), i)

	# fusion

	i <- do.call(rbind, lapply(i, as.data.frame, include.dates=TRUE))
	i <- merge(i, si, all.x=TRUE)

	# si indice partiel demandé

	if (type == 'P'){
		iP <- do.call(rbind, lapply(iP, as.data.frame, include.dates=TRUE))
		iP$date <- strftime(iP$start, '%Y-%m-%d')
		iP$nom  <- sub('\\..$', '', rownames(iP), perl=TRUE)

		l.fin	<- which(i$start==fin)
		lp.fin	<- which(iP$start==fin)

		i[l.fin, 'indice']		<- i[l.fin,'P_IND_DIFFUSE']
		i[l.fin, 'C_IND_PRV_J'] <- i[l.fin,'P_IND_PRV_J']
		i[l.fin, 'C_POL_PRV_J'] <- i[l.fin,'P_POL_PRV_J']
		i[l.fin, 'C_IND_PRV_J1']<- i[l.fin,'P_IND_PRV_J1']


		newVals <- iP[lp.fin,c('nom', 'NO2', 'O3', 'PM10', 'SO2')]
		newVals <- newVals[match(newVals$nom, i$nom[l.fin]),]
		i[l.fin,c('NO2', 'O3', 'PM10', 'SO2')] <- newVals[-1]
	}
	# TODO: on fabrique le même tableau.
	# une fois qu'on l'a, on garde la suite de l'algo (en commentant éventuellement,
	# et en remplaçant les $ par [[

	# création d'identifiants temporels

	i$idjourmeme <- paste0(i$nom, i$date)
	i$idjoursuiv <- strptime(i$date, '%Y-%m-%d', 'UTC')
	i$idjoursuiv <- as.POSIXct(i$idjoursuiv)+POSIXctp('day')
	i$idjoursuiv <- paste0(i$nom, strftime(i$idjoursuiv, '%Y-%m-%d'))

	# affectation des bonnes valeurs aux colonnes

	i$IJ1 <- i$indice[match(i$idjoursuiv, i$idjourmeme)]
	i$IJ1 <- ifelse(is.na(i$IJ1),
					i$C_IND_PRV_J[match(i$idjoursuiv, i$idjourmeme)],
					i$IJ1)
	i$IJ1 <- ifelse(is.na(i$IJ1),
					i$C_IND_PRV_J1,
					i$IJ1)

	i$PJ <- ifelse(is.na(i$indice),
				   i$C_POL_PRV_J,
				   apply(i[c('SO2', 'NO2', 'O3', 'PM10')], 1,
						 function(x) {
							 m <- which(x == max(x, na.rm=TRUE))
							 paste(c('01', '03', '08', '24')[m], collapse=',')
						 }))

	i$indice <- ifelse(is.na(i$indice), i$C_IND_PRV_J, i$indice)

	i$commentaire <- NA

	i$date <- as.POSIXct(i$date, 'UTC')

	i[c('date','nom','indice','SO2','NO2','O3','PM10','IJ1','PJ','commentaire')]
}

