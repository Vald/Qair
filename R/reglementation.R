# representatif <- function (x, pc.min=0.75, na.consecutif.max=720) {
#         return (mean (!is.na(x))>=pc.min & !any (slide (is.na(x), na.consecutif.max+1, 1, 1) == 1, na.rm=TRUE) )
# }

#' Test la validite d'un vecteur par rapport a nombre de NA consecutifs
#'
#' Le vecteur est considéré valide si le nombre de NA consécutifs 
#' qui le constitue est inférieur ou égal à \code{na.consecutif.max}.
#' Si le vecteur est valide, la valeur retournée est TRUE, FALSE sinon.
#'
#' @param x vecteur à tester
#' @param na.consecutif.max entier représentant le nombre maximal
#' 	de valeurs NA consécutive autorisé
#' @return TRUE or FALSE see \sQuote{Description}. 
check.na.consecutifs <- function (x, na.consecutif.max=720)
	return (!any (slide (is.na(x), na.consecutif.max+1, 1, 1) == 1, na.rm=TRUE) )

validation.prepare <- function (x, to, ...) {
	other.args <- list(...)
	if (!'check.720' %in% names (other.args) ) return (FALSE)
	if (!other.args$check.720) return (FALSE)

	is.periodic <- try (period (x) )
	if (!inherits (is.periodic, 'try-error') )
		if (is.periodic == 'hour' & to == 'year') {
			test.validation <- x
			test.validation[T] <- data.frame (lapply (data.frame (x), check.na.consecutifs) )
			test.validation <- changeTimeIntervalSupport (test.validation, 'year', 0, all, na.rm=TRUE)
		}
	return (test.validation)
}

#' Calcule un aot
#'
#' Fonction permettant de calculer des AOT (Accumulated Ozone exposure
#' over a Threshold of 40 Parts Per Billion).
#'
#' Cette fonction réalise le calcul 'brutal' d'un AOT : le vecteur qui
#' lui est donné en argument doit déjà avoir été traité pour ne contenir
#' que les valeurs aux dates et heures souhaitées. Elle calcule uniquement
#' la somme des dépassements du seuil indiqué, sous réserve que le nombre
#' de données valides dans le vecteur soit supérieur au pourcentage
#' indiqué par rep.min.
#'
#' @param x vecteur sur lequel le calcul doit être appliqué.
#' @param seuil valeur du seuil de l'AOT.
#' @param rep.min représentativité minimale pour réaliser le calcul.
#'	Valeur entre [0-1].
aot <- function(x, seuil, rep.min)
	if (mean(!is.na(x)) >= rep.min) {
		sum(ifelse(!is.na(x) & x > seuil, x - seuil, 0), na.rm = TRUE)/mean(!is.na(x))
	} else { NA }

aot40maiJuillet <- function (x, seuil, ...) {
	timezone (x) <- 'UTC'

	test <- validation.prepare (x, to='year', ...)	# 720 heures

	x <- x[month (start(x)) %in% 5:7 & hour (end(x)) %in% 8:19,]
	x <- changeTimeIntervalSupport (x, 'year', min.coverage=0, aot, seuil=seuil$seuil, rep.min=seuil$rep.b.comparaison)
	
	if (!is.logical (test) )			# 720 heures
		for (i in names(x))			# 720 heures
			x[[i]][!test[[i]]] <- NA	# 720 heures
	
	x[T] <- round.a (data.frame (x), seuil$precision)
	return (x)
}

aot40maiJuillet5ans <- function (x, seuil, ...) {
	x <- aot40maiJuillet (x, seuil)
	annees <- unique (year (start (x) ) )
	start <- as.POSIXct(sprintf ('%i-01-01', annees-4), timezone (x) )
	end <- as.POSIXct(sprintf ('%i-01-01', annees+1), timezone (x) )
	new.x <- new ('TimeIntervalDataFrame', start=start, end=end, timezone=timezone (x),
		      data=data.frame (bidon=1:length(start) ) )
	new.x <- project (x, new.x, FUN='mean', na.rm=TRUE, split.from=TRUE, merge.from=TRUE, min.coverage=3/5)
	new.x$bidon <- NULL

	if (!is.null (seuil$precision) )
		new.x[T] <- round.a (data.frame (new.x), seuil$precision)
	return (new.x)	
}

margevlPM25 <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2011:2013, 2015), seuil=29:25) )
	if (!detail) {
		if (use.marges)
			val.seuil <- (29:25)[findInterval(year(start(x)), c(1900, 2011:2013, 2015))] else 
			val.seuil <- 25
		x[T] <- data.frame (x) > val.seuil
	}
	return (x)
}

margevlNO2h <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2001:2010), seuil=30:20*10) )
	if (use.marges)
		s <- (30:20*10)[findInterval(year(start(x)), c(1900, 2001:2010))] else 
		s <- 200
	x[T] <- data.frame (x) > s
	
	test <- validation.prepare (x, to='year', ...)	# 720 heures

	x <- changeTimeIntervalSupport (x, 'year', seuil$rep.comparaison, sum, na.rm=TRUE)

	if (!is.logical (test) )			# 720 heures
		for (i in names(x))			# 720 heures
			x[[i]][!test[[i]]] <- NA	# 720 heures

	if (!detail)
		x[T] <- data.frame (x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
	return (x)
}

margevlNO2y <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2001:2010), seuil=30:20*2) )
	if (!detail) {
		if (use.marges)
			val.seuil <- (30:20*2)[findInterval(year(start(x)), c(1900, 2001:2010))] else 
			val.seuil <- 40
		x[T] <- data.frame (x) > val.seuil
	}
	return (x)
}

margevlC6H6y <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2006:2010), seuil=10:5) )
	if (!detail) {
		if (use.marges)
			val.seuil <- (10:5)[findInterval(year(start(x)), c(1900, 2006:2010))] else 
			val.seuil <- 5
		x[T] <- data.frame (x) > val.seuil
	}
	return (x)
}

depsur8h <- function (x, seuil, ...) {
	x[T] <- sapply (data.frame(x), slide, 8, 1, 0.75, 'mean')
	x <- changeTimeIntervalSupport (x, 'day', seuil$rep.b.comparaison, max, na.rm=TRUE)
	x[T] <- round.a (data.frame (x), seuil$precision)
	return (x)
}

protecVegeFroidSO2 <- function (x, seuil, ...) {
	annees <- unique (year (start (x) ) )
	annees <- c(annees[1]-1, annees, annees[length(annees)]+1)
	start <- as.POSIXct(sprintf ('%i-10-01', annees[-length(annees)]), timezone (x) )
	end <- as.POSIXct(sprintf ('%i-04-01', annees[-1]), timezone (x) )
	new.x <- new ('TimeIntervalDataFrame', start=start, end=end, timezone=timezone (x),
		      data=data.frame (bidon=1:length(start) ) )
	new.x <- project (x, new.x, FUN='mean', na.rm=TRUE, split.from=FALSE, min.coverage=seuil$rep.b.comparaison)
	new.x$bidon <- NULL

	if (!is.null (seuil$precision) )
		new.x[T] <- round.a (data.frame (new.x), seuil$precision)

	return (new.x)
}

sur3ans <- function (x, seuil, detail, ...) {
	# validation des données
	test <- validation.prepare (x, to='year', ...) # 720 heures

	x <- changeTimeIntervalSupport (x, 'month', 0.9,
					function (x, seuil) sum (x>seuil, na.rm=TRUE), seuil=seuil$seuil)
	valid.x <- changeTimeIntervalSupport (x[month(start(x)) %in% 4:9,], 'year', 0, function (x) sum (!is.na(x)) > 4)
	x <- changeTimeIntervalSupport (x, 'year', 0, sum, na.rm=TRUE)
	x <- x[start (x) >= min (start (valid.x)) & end (x) <= max (end (valid.x)),]
	for (i in names(x))
		x[[i]][!valid.x[[i]]] <- NA

	if (!is.logical (test) )			# 720 heures
		for (i in names(x))			# 720 heures
			x[[i]][!test[[i]]] <- NA	# 720 heures

	# preparation du support de retour
	annees <- unique (year (start (x) ) )
	start <- as.POSIXct(sprintf ('%i-01-01', annees-2), timezone (x) )
	end <- as.POSIXct(sprintf ('%i-01-01', annees+1), timezone (x) )
	new.x <- new ('TimeIntervalDataFrame', start=start, end=end, timezone=timezone (x),
		      data=data.frame (bidon=1:length(start) ) )
	# calculs
	new.x <- project (x, new.x, FUN='mean', na.rm=TRUE, split.from=TRUE, merge.from=TRUE, min.coverage=1/3)
	new.x$bidon <- NULL

	if (!is.null (seuil$precision) )
		new.x[T] <- round.a (data.frame (new.x), seuil$precision)
	if (!detail)
		new.x[T] <- data.frame (new.x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
	return (new.x)
}

alerte400NO2 <- function(x, seuil, detail, ...) {
	representativite <- seuil$rep.comparaison
	val <- seuil$seuil
	start <- start (x)[1:(nrow(x)-2)]
	end <- end (x)[3:nrow(x)]
	new.x <- new ('TimeIntervalDataFrame', start=start, end=end,
		      data=data.frame (bidon=1:length (start)), timezone=timezone (x) )
	new.x <- project (x, new.x, function (x, val) sum (x>val, na.rm=TRUE), val=val,
			  split.from=TRUE, min.coverage=representativite)
	new.x$bidon <- NULL
	if (!detail)
		new.x[T] <- data.frame (new.x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
	return (new.x)
}

preparation.base <- function (x, seuil, base=c('calcul', 'comparaison'), check.720=TRUE, ...) {
	base <- match.arg (base)
	representativite <- seuil[[sprintf ('rep.b.%s', base)]]
	base <- seuil[[sprintf ('base.%s', base)]]
	if (is.null (base) ) {
	} else if (is.function (base) ) {
		x <- do.call (base, c(list(x), list(seuil), ...) )
	} else if (is.character (base) | is.period (base) ) {
		test <- validation.prepare (x, to=base, ...)	# 720 heures

		x <- changeTimeIntervalSupport (x, base, representativite)

		if (!is.logical (test) )			# 720 heures
			for (i in names(x))			# 720 heures
				x[[i]][!test[[i]]] <- NA	# 720 heures
	} else stop ('Préparation des données impossible.')

	if (!is.null (seuil$precision) )
		x[T] <- round.a (data.frame (x), seuil$precision)

	return (x)
}

comparaison <- function (x, seuil, detail, check.720=TRUE, ...) { 
	if (is.null (seuil$comparaison) ) {
		if (!detail)
			x[T] <- data.frame (x) > seuil$seuil
	} else if (is.function (seuil$comparaison) ) {
		x <- do.call (seuil$comparaison, c(list(x), list(seuil), list(detail), ...) )
	} else if (is.character (seuil$comparaison) | is.period (seuil$comparaison) ) {
		test <- validation.prepare (x, to=seuil$comparaison, ...)	# 720 heures

		rep.comparaison <- if (is.null(seuil$rep.comparaison)) 0.75 else seuil$rep.comparaison
		x <- changeTimeIntervalSupport (x, seuil$comparaison, rep.comparaison,
						function (x, seuil) sum (x>seuil, na.rm=TRUE), seuil=seuil$seuil)
		
		if (!is.logical (test) )			# 720 heures
			for (i in names(x))			# 720 heures
				x[[i]][!test[[i]]] <- NA	# 720 heures
		
		if (!detail)
			x[T] <- data.frame (x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
	} else stop ('Impossible de calculer un résultat.')

	return (x)
}

validation.reglementaire <- function (x, seuil, etapes=c('preparation.calcul', 'preparation.comparaison'),
				      resultat=c('detail', 'comparaison'), check.720=TRUE, ...) {
	etapes <- match.arg (etapes, several.ok=TRUE)
	resultat <- match.arg (resultat, several.ok=TRUE)
	param.to.change <- list (...)
	param.to.change <- param.to.change[names (param.to.change) %in% names (seuil)]
	if (length (param.to.change) > 0)
	    seuil[names (param.to.change)] <- param.to.change
	if ('preparation.calcul' %in% etapes)
		x <- preparation.base (x, seuil, 'calcul', check.720)
	if ('preparation.comparaison' %in% etapes)
		x <- preparation.base (x, seuil, 'comparaison', check.720)
	if ('detail' %in% resultat) {
		det <- comparaison (x, seuil, detail=TRUE, check.720)
		if (!'comparaison' %in% resultat)
			x <- det
	} else if ('comparaison' %in% resultat) {
		comp <- comparaison (x, seuil, detail=FALSE, check.720)
		if ('detail' %in% resultat)
			x <- c(list (det), list(comp) ) else
			x <- comp

	}
	
	return (x)
}

