print.seuil <- function (x, ...) print (format (x, ...) )
format.seuil <- function (x, ...) {
	if (!is.null (x$description) ) return (x$description)

	get.unite <- function(x) {
		if (is.period (x) )
			x <- names(x)[x > 0][1]
		c('seconde', 'minute', 'heure', 'jour', 'mois', 'an')[
					match(x, c('second', 'minute', 'hour', 'day', 'month', 'year'))]
	}
	get.val <- function (x) x[x>0][1]
	sprintf ('%s : %s pour la protection de %s. %s %s en moyenne%s à ne pas dépasser%s.',
		 x$cchim, x$type, x$protection,
		 as.character(x$seuil), x$unite,
		 ifelse (!is.null (x$base.comparaison) & is.character (x$base.comparaison),
			 sprintf (' sur 1 %s', get.unite(x$base.comparaison) ),
			 '??'),
		 ifelse (!is.null (x$comparaison) & is.character (x$comparaison) & !is.null(x$nb.max),
				 sprintf (' plus de %s fois tous les %ss', x$nb.max, get.unite(x$comparaison)), ifelse (
		 	 !is.null (x$comparaison) & is.character (x$comparaison) & is.null(x$nb.max),
			 	sprintf (' sur 1 %s', get.unite(x$comparaison)), ifelse (
			 !is.null (x$comparaison) & is.period (x$comparaison) & !is.null(x$nb.max),
			 	sprintf (' plus de %s fois tous les %i %ss', x$nb.max, get.val(x$comparaison), get.unite(x$comparaison)), ifelse (
			 !is.null (x$comparaison) & is.period (x$comparaison) & is.null(x$nb.max),
			 	sprintf (' sur %i %s', get.val(x$comparaison), get.unite(x$comparaison)),
			 '')))))
}

representatif <- function (x, pc.min=0.75, na.consecutif.max=720) {
	return (mean (!is.na(x))>=pc.min & !any (slide (is.na(x), na.consecutif.max+1, 1, 1) == 1, na.rm=TRUE) )
}

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

aot <- function(x, seuil, rep.min)
	if (mean(!is.na(x)) >= rep.min) {
		sum(ifelse(!is.na(x) & x > seuil, x - seuil, 0), na.rm = TRUE)/mean(!is.na(x))
	} else { NA }

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

aot40maiJuillet <- function (x, seuil, ...) {
	timezone (x) <- 'UTC'

	test <- validation.prepare (x, to='year', ...)	# 720 heures

	x <- x[month (start(x)) %in% 5:7 & hour (end(x)) %in% 8:19,]
	x <- changeTimeIntervalSupport (x, 'year', min.coverage=0, aot, seuil=80, rep.min=seuil$rep.b.comparaison)
	
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

typologies <- c('industriel', 'trafic', 'urbain', 'périurbain', 'rural régional', 'rural national')

.seuils <- list (list (polluant='03', cchim='NO2', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', cchim='NO2', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', cchim='NO2', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      #                       comparaison=alerte400NO2, rep.comparaison=1, precision=0, nb.max=2,
		      comparaison=new_period(hour=3), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=400, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', cchim='NO2', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', cchim='NO2', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=margevlNO2h, rep.comparaison=0.9, precision=0, nb.max=18,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01'),
		      description="NO2 : valeur limite pour la protection de la santé humaine. 200 microg/m3 en moyenne sur 1 heure à ne pas dépasser plus de 18 fois tous les ans."),
		list (polluant='03', cchim='NO2', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlNO2y, precision=0, rep.comparaison=0.9,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01')),
		list (polluant='12', cchim='NOx', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=30, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', cchim='PM10', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=30, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', cchim='PM10', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', cchim='PM10', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=80, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', cchim='PM10', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=35,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', cchim='PM10', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', cchim='PM2.5', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=10, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', cchim='PM2.5', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', cchim='PM2.5', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlPM25, rep.comparaison=0.9, precision=0,
		      seuil=25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2015-01-01') ),
		list (polluant='19', cchim='Plomb', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='AA', cchim='Plomb', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='19', cchim='Plomb', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.5, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='AA', cchim='Plomb', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.5, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', cchim='SO2', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', cchim='SO2', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=300, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', cchim='SO2', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=new_period (hour=3), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=500, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', cchim='SO2', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=24,
		      seuil=350, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2005-01-01')),
		list (polluant='01', cchim='SO2', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=3,
		      seuil=125, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2005-01-01')),
		list (polluant='01', cchim='SO2', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', cchim='SO2', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=protecVegeFroidSO2, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010',
		      description="SO2 : niveau critique pour la protection de la végétation. 20 microg/m3 en moyenne sur les mois d'octobre à mars à ne pas dépasser."),
		list (polluant='08', cchim='O3', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0,
		      seuil=120, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010',
		      description="O3 : objectif de qualité pour la protection de la santé humaine. 120 microg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser sur 1 an."),
		list (polluant='08', cchim='O3', type='objectif de qualité', protection='la végétation', sites=c('rural régional', 'rural national', 'périurbain'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=aot40maiJuillet, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=6000, unite='microg/m3.h', reference='Décret 2010-1250 du 21 octobre 2010',
		      description="O3 : objectif de qualité pour la protection de la végétation. 6000 microg/m3.h en AOT40 de mai à juillet à ne pas dépasser."),
		list (polluant='08', cchim='O3', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison=sur3ans, precision=0, nb.max=25,
		      seuil=120, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01'),
		      description="O3 : valeur cible pour la protection de la santé humaine. 120 microg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser plus de 25 fois tous les ans."),
		list (polluant='08', cchim='O3', type='valeur cible', protection='la végétation', sites=c('rural régional', 'rural national', 'périurbain'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=aot40maiJuillet5ans, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=18000, unite='microg/m3.h', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01'),
		      description="O3 : valeur cible pour la protection de la végétation. 18000 microg/m3.h en AOT40 de mai à juillet à ne pas dépasser en moyenne sur 5 ans."),
		list (polluant='08', cchim='O3', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=180, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', cchim='O3', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=240, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', cchim='O3', type="seuil d'alerte", protection="la mise en oeuvre progressive de mesures d'urgence", sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=new_period(hour=3), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=240, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', cchim='O3', type="seuil d'alerte", protection="la mise en oeuvre progressive de mesures d'urgence", sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=new_period(hour=3), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=300, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', cchim='O3', type="seuil d'alerte", protection="la mise en oeuvre progressive de mesures d'urgence", sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=360, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='04', cchim='CO', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0,
		      seuil=10, unite='mg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2005-01-01'),
		      description="CO : valeur limite pour la protection de la santé humaine. 10 mg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser sur 1 an."),
		list (polluant='V4', cchim='C6H6', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='V4', cchim='C6H6', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlC6H6y, precision=1,
		      seuil=5, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='80', cchim='Arsenic', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=6, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AH', cchim='Arsenic', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=6, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='82', cchim='Cadmium', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=5, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AJ', cchim='Cadmium', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=5, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='87', cchim='Nickel', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AC', cchim='Nickel', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='P6', cchim='B(a)P', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=1, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),

		list (polluant='01', cchim='SO2', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=3,
		      seuil=50, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='01', cchim='SO2', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=3,
		      seuil=75, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='01', cchim='SO2', type="seuil d'évaluation inférieur", protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=8, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='01', cchim='SO2', type="seuil d'évaluation supérieur", protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=12, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='03', cchim='NO2', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=18,
		      seuil=100, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='03', cchim='NO2', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=18,
		      seuil=140, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='03', cchim='NO2', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=26, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='03', cchim='NO2', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=32, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='12', cchim='NOx', type="seuil d'évaluation inférieur", protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=19.5, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='12', cchim='NOx', type="seuil d'évaluation supérieur", protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=24, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='24', cchim='PM10', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=35,
		      seuil=25, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='24', cchim='PM10', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='24', cchim='PM10', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=35,
		      seuil=35, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='24', cchim='PM10', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=28, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='39', cchim='PM2.5', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlPM25, rep.comparaison=0.9, precision=0,
		      seuil=12, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='39', cchim='PM2.5', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlPM25, rep.comparaison=0.9, precision=0,
		      seuil=17, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='19', cchim='Plomb', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='19', cchim='Plomb', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.35, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='AA', cchim='Plomb', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='AA', cchim='Plomb', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.35, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='V4', cchim='C6H6', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='V4', cchim='C6H6', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=3.5, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='04', cchim='CO', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0,
		      seuil=5, unite='mg/m3', reference='Directive 2008/50/CE du 21 mai 2008',
		      description="CO : seuil d'évaluation inférieur pour la protection de la santé humaine. 5 mg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser sur 1 an."),
		list (polluant='04', cchim='CO', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0,
		      seuil=7, unite='mg/m3', reference='Directive 2008/50/CE du 21 mai 2008',
		      description="CO : seuil d'évaluation supérieur pour la protection de la santé humaine. 7 mg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser sur 1 an."),
		list (polluant='80', cchim='Arsenic', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2.4, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='80', cchim='Arsenic', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=3.6, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AH', cchim='Arsenic', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2.4, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AH', cchim='Arsenic', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=3.6, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='82', cchim='Cadmium', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='82', cchim='Cadmium', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=3, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AJ', cchim='Cadmium', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=2, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AJ', cchim='Cadmium', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=3, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='87', cchim='Nickel', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=10, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='87', cchim='Nickel', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=14, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AC', cchim='Nickel', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=10, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AC', cchim='Nickel', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=14, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='P6', cchim='B(a)P', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=0.4, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='P6', cchim='B(a)P', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=1,
		      seuil=0.6, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004')
		)


seuils <- function (type=c("objectif de qualité", "seuil d'information et de recommandation", "seuil d'alerte", "valeur limite", "niveau critique", "valeur cible", "seuil d'évaluation inférieur", "seuil d'évaluation supérieur"),
		    protection=c("la santé humaine", "la végétation", "la mise en oeuvre progressive de mesures d'urgence"),
		    polluant=c("03", "12", "24", "39", "19", "AA", "01", "08", "04", "V4", "80", "AH", "82", "AJ", "87", "AC", "P6"),
		    cchim=c("NO2", "NOx", "PM10", "PM2.5", "Plomb", "SO2", "O3", "CO", "C6H6", "Arsenic", "Cadmium", "Nickel", "B(a)P"),
		    sites=typologies) {
	type <- match.arg (type, several.ok=TRUE)
	protection <- match.arg (protection, several.ok=TRUE)
	polluant <- match.arg (polluant, several.ok=TRUE)
	cchim <- match.arg (cchim, several.ok=TRUE)
	sites <- match.arg (sites, several.ok=TRUE)
	
	res <- .seuils
	res <- res[sapply(res, '[[','type') %in% type]
	res <- res[sapply(res, '[[','protection') %in% protection]
	res <- res[sapply(res, '[[','polluant') %in% polluant]
	res <- res[sapply(res, '[[','cchim') %in% cchim]
	res <- res[sapply (lapply(res, '[[','sites'), function(x, y) any (x %in% y), sites)]

	for (i in 1:length (res) ) class (res[[i]]) <- c('seuil', 'list')

	#         if (length (res) == 1)
	#                 return (res[[1]]) else
		return (res)
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

