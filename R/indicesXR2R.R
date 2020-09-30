#' Recuperation des indices de qualite de l'air
#'
#' La fonction permet de rapatrier les indices de
#' la qualité de l'air stocké dans une base XR. Pour 
#' connaître la liste des réseaux d'indices definis au 1er janvier 2020.
#' FIXME: ISEO --> en attendant que v2/aqiGroup fonctionne
#'
#' @param agglos chaîne de caractères indiquant les 
#' 	les réseaux d'indices pour lesquels les donnée
#' 	doivent être rapatriées (invoquer \code{indices2XR()}
#' 	sans arguments pour connaître les valeurs possibles).
#' @inheritParams xrGetContinuousData
#' @inheritParams xrGetMesures
#' @param detail booleen indiquant si le détails des indices doit être rapatrié
#' également. Autrement dit si les sous-indices doivent également être récupérés
#' dans la base XR. Si TRUE, le paramètre \sQuote{format} permet de préciser
#' l'agencement des données.
#' @param format utilisé uniquement si \sQuote{detail} est à TRUE. Trois formats
#' sont à l'heure actuelle disponible : 
#' \describe{
#' \item{type}{une liste de TimeIntervalDataFrame est retournée. Le premier
#' élément de la liste contient un TimeIntervalDataFrame avec les indices pour
#' chaque agglo, le deuxième un TimeIntervalDataFrame avec les sous-indice du
#' premier polluant, le troisième avec les sous-indices du deuxième polluant,
#' etc.}
#' \item{agglo}{Une liste de TimeIntervalDataFrame est retournée. Chaque élément
#' de la liste contient un TimeIntervalDataFrame avec tous les indices et 
#' sous-indices pour une seule agglo.}
#' \item{aucun}{Un simple TimeIntervalDataFrame est retourné. Les noms des
#' colonnes sont formés sur le schéma AGGLO.POLLUANT}
#' }
#' @param type Quels indices récupérer : Complets ou Partiels.
#'
#' @seealso \code{\link{xrConnect}}, \code{\link{plot.indice}},
#' \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'
#' @return Si detail est FALSE, un objet de classe
#' \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' 
#' si detail est TRUE, une liste de 
#' \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}.
#' cf le \sQuote{format} pour le détail.
#'
#' contenant les indices pour la période demandée.
#'
'indicesXR2R' <-
function (conn, agglos, start, end, detail=FALSE,
	  format=c('type', 'agglo', 'aucun'), type=c('C', 'P'), resv3=FALSE) {
	# FIXME:ISEO possibilité d'écrire l'indice ?
	# FIXME:ISEO sites de prelevements
	# FIXME:ISEO methodes de prelevements
	# FIXME:ISEO donnees manuelles

	format <- match.arg(format)
	type   <- match.arg(type)

	# récupération de la version avec laquelle on bosse et initialisation de --
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- 'v2/disclosedAQI?'

	nagglo <- ifelse(!resv3 & nv == 'nv2','NOM_AGGLO','areaName')
	nopols <- c('01', '03' ,'08', '24')
	cchims <- c('SO2', 'NO2', 'O3', 'PM10')

	# liste des agglos disponibles --------------------------------------------
	# FIXME:ISEO à corriger chez ISEO (résultat vide)
	#agglos <- xrGetQuery(conn, 'v2/aqiGroups')
	# en attendant : attention solution partielle, les groupes pour lesquels pas 
	# d'indice sur la periode demandée n'apparaitront pas
	lagglos <- xrGetQuery(conn,
		'v2/disclosedAQI?from=2020-01-01T00:00:00Z&to=2020-01-02T00:00:00Z')
	lagglos <- lagglos[['group']]
	lagglos <- lagglos[c('idAqiGroup','idAqiIndex','idMeasureGroup','idCommune','areaName')]
	lagglos <- unique(lagglos)
	lagglos <- lagglos[order(lagglos[['idAqiIndex']],lagglos[['idMeasureGroup']]),]
	names(lagglos)[names(lagglos)=='areaName'] <- nagglo

	if(missing(agglos)) return(lagglos[nagglo])

	# idAqiGroups demandés
	iag <- lagglos[['idAqiGroup']][match(agglos, lagglos[[nagglo]])]

	# start et end sont mis en forme pour la requete ----------------------
	# si debut et fin ne sont pas en POSIXct, conversion

	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz='UTC')
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz='UTC')

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	dformat <- '%Y-%m-%dT%H:%M:%SZ'
	from    <- format (start, format = dformat, tz='UTC')
	to      <- format (end+POSIXctp('second'), format = dformat, tz='UTC')


	# creation de la requête et exécution -------------------------------------

	query <- sprintf('%sidAqiGroups=%s&withSubIndexes=%s&from=%s&to=%s',
		bquery, paste(iag, collapse=','), ifelse(detail,'true','false'), from, to)

	indices <- xrGetQuery(conn, query)

	indices[[nagglo]] <- lagglos[[nagglo]][
		match(indices[['idAqiGroup']], lagglos[['idAqiGroup']])]

	# selection des complets ou partiels et sous-indices ----------------------

	indtmp <- cbind(
		indices[c('date', nagglo)],
		indices[[ifelse(type=='C', 'full', 'anticipated')]][['disclose']])

	if(detail)
		indices <- cbind(
			indtmp,
			indices[[ifelse(type=='C', 'full', 'anticipated')]][['subIndexes']]) else
		indices <- indtmp

	# traitement des sous-indices (si demandés) -------------------------------

	indices[['date']] <- strptime(indices[['date']], dformat, 'UTC')
	indices[['date']] <- as.POSIXct(indices[['date']])

	if(detail) {

		sindices <- lapply(1:length(nopols), function(i) {
			si <- cbind(indices[c('date', nagglo)],
						NOPOL         =cchims[i],
						disclosedValue=indices[[nopols[i]]][['subIndexDisclosedValue']])
			return(si)
		})
		sindices <-do.call(rbind, sindices)

		indices[['NOPOL']] <- 'indice'
		indices <- indices[c('date', nagglo, 'NOPOL', 'disclosedValue')]
		indices <- rbind(indices, sindices)
	}

	# définition d'un fonction interne qui transforme un tableau --------------
	# brute d'indice en un TimeIntervalDataFrame qui va bien

	mef <- function(indices, group.by) {
		if (nrow(indices) == 0) {
			indices <- TimeIntervalDataFrame(character(0), character(0), 'UTC')
		} else {
			indices <- split(indices, indices[[group.by]])
			indices <- lapply(names(indices), function(n) {
				i <- names(indices[[n]]) == 'disclosedValue'
				names(indices[[n]])[i] <- n
				return(indices[[n]][c('date', n)])
			})

			while(length(indices) > 1) {
				indices[[1]] <- merge(indices[[1]], indices[[2]], all=TRUE)
				indices[[2]] <- NULL
			}
			indices <- indices[[1]]

			indices <- TimeIntervalDataFrame(
				indices[['date']], indices[['date']]+POSIXctp('day'), 'UTC',
				indices[setdiff(names(indices), 'date')])
		}
		return(indices)
	}

	# mise à NA des éventuels 0 -----------------------------------------------

	indices[['disclosedValue']][indices[['disclosedValue']]==0] <- NA

	# application de la fonction mef ------------------------------------------

	if(detail) {

		if(format == 'type') {
			indices <- lapply(split(indices, indices[['NOPOL']]), mef, nagglo)

		} else if(format == 'agglo') {
			indices <- lapply(split(indices, indices[[nagglo]]), mef, 'NOPOL')

		} else if(format == 'aucun') {
			indices[[nagglo]] <- paste(indices[[nagglo]], indices[['NOPOL']], sep='.')
			indices <- mef(indices, nagglo)
		}
	} else {
		indices <- mef(indices, nagglo)
	}

	return (indices)
}

#' Affichage circulaire des indices de qualité de l'air
#'
#' fonction permettant de représenter des indices de qualité
#' de l'air de manière rigoulotte :)
#'
#' @param x un objet de la calsse
#' \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' contenant les indices à représenter
#' @param y valeur unique (numérique, caractère) référençant
#' 	la colonne à représenter
#' @param cex character expansion, cf \code{\link[graphics]{par}}
#'
#' @seealso \code{\link{indicesXR2R}}
plot.indice <- function (x, y, cex=1.7) {
	commentaires <- factor(
		c(	'Tres bon', 'Tres bon', 'Bon', 'Bon', 'Moyen',
			'Mediocre', 'Mediocre', 'Mauvais', 'Mauvais', 'Tres mauvais'),
		c(	'Tres bon', 'Bon', 'Moyen', 'Mediocre', 'Mauvais',
			'Tres mauvais'),,TRUE)
	indices <- x[[y]]

	plot (	NA, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5),
		axes=FALSE, ann=FALSE, asp=1)
	temp <- data.frame (nb = tapply (
					indices,
					commentaires[indices],
					length) )
	temp$pc <- temp$nb/sum (!is.na (indices) )*100
	cols <- colorRampPalette(c(hsv(0.33, , 0.768), "yellow", "orange", "red"))(6)
	names(cols) <- levels(commentaires)
	
	theta <- 0
	for (j in 1:nrow (temp) ) {
		if (is.na (temp$nb[j]) ) next
		theta <- (seq(360*theta/100, 360*(theta + temp$pc[j])/100))*pi/180
		x.coord <- c(sin(theta), 0.25*sin(theta[length(theta):1]))
		y.coord <- c(cos(theta), 0.25*cos(theta[length(theta):1]))
		polygon(x.coord, y.coord, col=cols[rownames(temp)[j]], density=-1)

		theta <- sum(temp$pc[1:j])
	}
	tempI <- abs (sapply (
			lapply (rownames (temp), nchar),
			'-',
			max(sapply(rownames(temp), nchar) ) ) )
	par(font=2)
	text(-2, cex=cex, seq(1.5, 0.8, length=6), rownames(temp), col=cols, pos=4)
	text(-0.7, cex=cex, seq(1.5, 0.8, length=6), paste(ifelse(is.na(temp$nb), 0, temp$nb), ' jour', ifelse(!is.na(temp$nb) & temp$nb > 1, 's', ''), sep=''), col=cols, pos=2)
	par(font=3)
	text (	-2, cex=cex/1.7*1.4, -1.3, pos=4,
		sprintf (
			"Nombre de jours sans indice : %i",
			sum (is.na (indices) ) ) )
	box(col='white')
}

