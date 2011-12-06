#' Recuperation des indices de qualite de l'air
#'
#' La fonction permet de rappatrier les indices de
#' la qualité de l'air stocké dans une base XR. Pour 
#' connaître la liste des réseaux d'indices definis,
#' il suffit d'invoquer la fonction sans arguments.
#'
#' @param agglos chaîne de caractères indiquant les 
#' 	les réseaux d'indices pour lesquels les donnée
#' 	doivent être rappatriées (invoquer \code{indices2XR()}
#' 	sans arguments pour connaître les valeurs possibles).
#' @inheritParams xrGetContinuousData
#'
#' @seealso \code{\link{xrConnect()}}, \code{\link{plot.indice}},
#' \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'
#' @return un objet de classe
#' \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' contenant les indices pour la période demandée.
'indicesXR2R' <-
function (conn, agglos, start, end) {
	if (missing (agglos) ) {
		query <- 'SELECT NOM_AGGLO FROM GROUPE_ATMO'
		return (xrGetQuery (conn, query))
	}

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

	indices$NOM_AGGLO <- as.character (indices$NOM_AGGLO)
	indices$J_DATE <- substr (as.character (indices$J_DATE), 1, 10)
	names (indices)[names (indices) == 'J_DATE'] <- 'date'

	indices$C_IND_DIFFUSE[indices$C_IND_DIFFUSE == 0] <- NA
	temp <- split(indices[c("date", "C_IND_DIFFUSE")], indices$NOM_AGGLO)
	
	indices <- temp[[1]]
	names (indices)[2] <- names (temp)[1]
	if (length (temp) > 1)
	for (i in 2:length(temp)) {
		names (temp[[i]])[2] <- names (temp)[i]
		indices <- merge (indices, temp[[i]], all=TRUE, by='date')
	}
	rm(temp)
	indices$date <- as.POSIXct(indices$date, 'UTC')

	indices <- new ('TimeIntervalDataFrame',
			start=indices$date, end=indices$date+d,
			timezone='UTC', data=indices[setdiff(names(indices), 'date')])

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
#' @inheritParams graphics::par
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

