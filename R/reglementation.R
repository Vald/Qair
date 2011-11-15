representatif <- function (x, pc.min=0.75, na.consecutif.max=720) {
	return (mean (!is.na(x))>=pc.min & !any (slide (is.na(x), na.consecutif.max+1, 1, 1) == 1, na.rm=TRUE) )
}

aot <- function(x, seuil, rep.min)
	if (mean(!is.na(x)) >= rep.min) {
		sum(ifelse(!is.na(x) & x > seuil, x - seuil, 0), na.rm = TRUE)/mean(!is.na(x))
	} else { NA }

alerte400NO2 <- function(x, seuil, detail) {
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

margevlPM25 <- function (x, seuil, detail) {
	if (!detail) {
		val.seuil <- (29:25)[findInterval(year(start(x)), c(1900, 2011:2013, 2015))]
		x[T] <- data.frame (x) > val.seuil
	}
	return (x)
}

depO3sur8h120 <- function (x, seuil) {
	x[T] <- sapply (data.frame(x), slide, 8, 1, 0.75, 'mean')
	x <- changeTimeIntervalSupport (x, 'day', 0.75, max, na.rm=TRUE)
	x[T] <- round.a (data.frame (x), seuil$precision)
	return (x)
}

protecVegeFroidSO2 <- function (x, seuil) {
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

aot40maiJuillet <- function (x, seuil) {
	timezone (x) <- 'UTC'
	x <- x[month (start(x)) %in% 5:7 & hour (end(x)) %in% 8:19,]
	x <- changeTimeIntervalSupport (x, 'year', min.coverage=0, aot, seuil=80, rep.min=seuil$rep.b.comparaison)
	x[T] <- round.a (data.frame (x), seuil$precision)
	return (x)
}

aot40maiJuillet5ans <- function (x, seuil) {
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

seuils <- list (list (polluant='03', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.calcul=0.75, base.comparaison='year', rep.comparaison=0.9,
		      precision=0,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=alerte400NO2, rep.comparaison=1, precision=0, nb.max=2,
		      seuil=400, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='03', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=18,
		      seuil=200, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01')),
		list (polluant='03', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01')),
		list (polluant='12', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=30, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=30, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=80, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=35,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='24', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=40, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=10, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='39', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison=margevlPM25, rep.comparaison=0.9, precision=0,
		      seuil=25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2015-01-01') ),
		list (polluant='19', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='AA', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.25, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='19', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.5, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='AA', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=0.5, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=50, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type="seuil d'information et de recommandation", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      precision=0,
		      seuil=300, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type="seuil d'alerte", protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=new_period (hour=3), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=500, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=24,
		      seuil=350, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type='valeur limite', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='day', rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=3,
		      seuil=125, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='01', type='niveau critique', protection='la végétation', sites=c('rural régional', 'rural national'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=protecVegeFroidSO2, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=20, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', type='objectif de qualité', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depO3sur8h120, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0,
		      seuil=120, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', type='objectif de qualité', protection='la végétation', sites=c('rural régional', 'rural national', 'périurbain'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=aot40maiJuillet, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=6000, unite='microg/m3.h', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depO3sur8h120, rep.b.comparaison=0.75,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=25,
		      seuil=120, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', type='valeur cible', protection='la végétation', sites=c('rural régional', 'rural national', 'périurbain'), 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=aot40maiJuillet5ans, rep.b.comparaison=0.9,
		      precision=0,
		      seuil=18000, unite='microg/m3.h', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		list (polluant='', type='', protection='', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      comparaison='year', rep.comparaison=0.9, precision=0, nb.max=0,
		      seuil=, unite='', reference='', applicable=),
		)


preparation.base <- function (x, seuil, base=c('calcul', 'comparaison') ) {
	base <- match.arg (base)
	representativite <- seuil[[sprintf ('rep.b.%s', base)]]
	base <- seuil[[sprintf ('base.%s', base)]]
	if (is.null (base) ) {
	} else if (is.function (base) ) {
		x <- do.call (base, c(list(x), list(seuil) ) )
	} else if (is.character (base) | is.period (base) ) {
		x <- changeTimeIntervalSupport (x, base, representativite)
	} else stop ('Préparation des données impossible.')

	if (!is.null (seuil$precision) )
		x[T] <- round.a (data.frame (x), seuil$precision)

	return (x)
}

comparaison <- function (x, seuil, detail) { 
	if (is.null (seuil$comparaison) ) {
		if (!detail)
			x[T] <- data.frame (x) > seuil$seuil
	} else if (is.function (seuil$comparaison) ) {
		x <- do.call (seuil$comparaison, c(list(x), list(seuil), list(detail) ) )
	} else if (is.character (seuil$comparaison) | is.period (seuil$comparaison) ) {
		x <- changeTimeIntervalSupport (x, seuil$comparaison, seuil$rep.comparaison,
						function (x, seuil) sum (x>seuil, na.rm=TRUE), seuil=seuil$seuil)
		if (!detail)
			x[T] <- data.frame (x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
	} else stop ('Impossible de calculer un résultat.')

	return (x)
}

validation.reglementaire <- function (x, seuil, etapes=c('preparation.calcul', 'preparation.comparaison'),
				      resultat=c('detail', 'comparaison')) {
	resultat <- match.arg (resultat)
	if ('preparation.calcul' %in% etapes)
		x <- preparation.base (x, seuil, 'calcul')
	if ('preparation.comparaison' %in% etapes)
		x <- preparation.base (x, seuil, 'comparaison')
	if (resultat == 'detail')
		x <- comparaison (x, seuil, detail=TRUE)
	else if (resultat == 'comparaison')
		x <- comparaison (x, seuil, detail=FALSE)

	return (x)
}
