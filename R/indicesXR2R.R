#' Recuperation des indices de qualite de l'air
#'
#' La fonction permet de rapatrier les indices de
#' la qualité de l'air stocké dans une base XR. Pour 
#' connaître la liste des réseaux d'indices definis,
#' il suffit d'invoquer la fonction sans arguments.
#'
#' @param agglos chaîne de caractères indiquant les 
#' 	les réseaux d'indices pour lesquels les donnée
#' 	doivent être rapatriées (invoquer \code{indices2XR()}
#' 	sans arguments pour connaître les valeurs possibles).
#' @inheritParams xrGetContinuousData
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
#' @author Fabrice Caïni, Vladislav Navel
'indicesXR2R' <-
function (conn, agglos, start, end, detail=FALSE,
	  format=c('type', 'agglo', 'aucun')) {
	if (missing (agglos) ) {
		query <- 'SELECT NOM_AGGLO FROM GROUPE_ATMO'
		return (xrGetQuery (conn, query))
	}

	format <- match.arg(format)

	query <- sprintf (
		"SELECT NOM_AGGLO, J_DATE, C_IND_DIFFUSE
			FROM	RESULTAT_INDICE JOIN
				GROUPE_ATMO USING (NOM_COURT_GRP)
			WHERE	NOM_AGGLO IN('%s') AND
				J_DATE BETWEEN
			       		TO_DATE ('%s', 'YYYY-MM-DD') AND
					TO_DATE ('%s', 'YYYY-MM-DD')",
		paste (agglos, collapse="', '"), start, end)

	indices <- xrGetQuery (conn, query)
	# forçage
	indices$C_IND_DIFFUSE <- as.numeric( indices$C_IND_DIFFUSE )

	if( detail ) {
		indices$NOPOL <- 'indice'

		query <- sprintf("SELECT NOM_AGGLO, J_DATE, C_SS_INDICE_DIFF, NOPOL
			FROM	RESULTAT_SS_INDICE JOIN
					GROUPE_ATMO USING (NOM_COURT_GRP)
				WHERE	NOM_AGGLO IN('%s') AND
					J_DATE BETWEEN
						TO_DATE ('%s', 'YYYY-MM-DD') AND
						TO_DATE ('%s', 'YYYY-MM-DD')", 
			paste(agglos, collapse = "', '"), start, end)

		ss.indices <- xrGetQuery(conn, query)
		# forçage
		ss.indices$C_SS_INDICE_DIFF <-
			as.numeric( ss.indices$C_SS_INDICE_DIFF )
		names(ss.indices)[names(ss.indices) == 'C_SS_INDICE_DIFF'] <-
			'C_IND_DIFFUSE'

		pols <- xrGetPolluants(conn, unique(ss.indices$NOPOL),
			      search.fields='NOPOL')[c('NOPOL', 'CCHIM')]

		ss.indices$NOPOL <- pols$CCHIM[match(ss.indices$NOPOL, pols$NOPOL)]

		indices <- merge(indices, ss.indices, all=TRUE)
	}

	indices$NOM_AGGLO <- as.character (indices$NOM_AGGLO)
	indices$J_DATE <- substr (as.character (indices$J_DATE), 1, 10)
	names (indices)[names (indices) == 'J_DATE'] <- 'date'

	indices$C_IND_DIFFUSE[indices$C_IND_DIFFUSE == 0] <- NA

	# définition d'un fonction interne qui transformet un tableau
	# brute d'indice (directement extrait d'XR et un TimeIntervalDataFrame
	# qui va bien
	mef <- function(indices, group.by) {
		temp <- split(indices[c("date", "C_IND_DIFFUSE")],
			      indices[[group.by]])
	
		indices <- temp[[1]]
		names (indices)[2] <- names (temp)[1]
		if (length (temp) > 1)
		for (i in 2:length(temp)) {
			names (temp[[i]])[2] <- names (temp)[i]
			indices <- merge (indices, temp[[i]],
					  all=TRUE, by='date')
		}
		rm(temp)
		indices$date <- as.POSIXct(indices$date, 'UTC')

		indices <- new ('TimeIntervalDataFrame',
			start=indices$date, end=indices$date+d,
			timezone='UTC',
			data=indices[setdiff(names(indices), 'date')])
		return( indices )
	}

	# application de la fonction mef

	if( detail ) {
		if( format == 'type' ) {
			indices <- lapply(split( indices, indices$NOPOL ),
					  mef, 'NOM_AGGLO')
		} else if( format == 'agglo' ) {
			indices <- lapply(split( indices, indices$NOM_AGGLO ),
					  mef, 'NOPOL')
		} else if( format == 'aucun' ) {
			indices$NOM_AGGLO <-
				paste(indices$NOM_AGGLO, indices$NOPOL, sep='.')
			indices <- mef(indices, 'NOM_AGGLO')
		}
	} else {
		indices <- mef(indices, 'NOM_AGGLO')
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

