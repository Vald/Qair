#' Affichage des objets de classe seuil (classe S3)
#'
#' @method print seuil
#' @param x objet à afficher
#' @param format précise le format d'affichage avec une combinaison des caractères
#' '\%P', '\%t', '\%p', '\%s', '\%u', '\%b', '\%f' (cf section \sQuote{Details}).
#' Si le seuil est extraordinaire et qu'il est défini avec sa propre description
#' \sQuote{format} est ignoré.
#' @param \dots pour les autres méthodes
#'
#' @section Details :
#' la signification des différentes caractères pour le formatage suit :
#' \describe{
#' \item{\%P}{polluant concerné par le seuil},
#' \item{\%t}{type de seuil},
#' \item{\%p}{protection visée par le seuil},
#' \item{\%s}{valeur du seuil},
#' \item{\%u}{unité du seuil},
#' \item{\%b}{base temporelle du calcul},
#' \item{\%f}{fréquence temporelle autorisée pour le dépassement du seuil}
#' }
#'
print.seuil <- function (x, ...) print (format (x, ...) )
#' Mise forme 'lisible' des objets de classe seuil (classe S3)
#'
#' @rdname print.seuil
#' 
#' @method format seuil
#' @inheritParams print.seuil
#' 
#' @return une chaîne de caractères décrivant l'objet \code{x}
format.seuil <- function (x, format, ...) {
	if (missing(format))
	{
		if (!is.null (x$description) ) return (x$description)
		format <- "%P : %t pour %p. %s %u en moyenne%b à ne pas dépasser%f"
	}

	get.unite <- function(x) {
		if (inherits (x, 'POSIXctp') )
			x <- as.character(unit(x))
		c('seconde', 'minute', 'heure', 'jour', 'mois', 'an')[
					match(x, c('second', 'minute', 'hour', 'day', 'month', 'year'))]
	}
	get.val <- function (x) duration(x)
	P <- x$cchim
	t <- x$type
	p <- x$protection
	s <- as.character(x$seuil)
	u <- x$unite
       	b <- ifelse (!is.null (x$base.comparaison) & is.character (x$base.comparaison),
		       sprintf (' sur 1 %s', get.unite(x$base.comparaison) ), '??')
	f <- ifelse (!is.null (x$comparaison) & is.character (x$comparaison) & !is.null(x$nb.max),
		sprintf (' plus de %s fois tous les %ss', x$nb.max, get.unite(x$comparaison)),
		ifelse ( !is.null (x$comparaison) & is.character (x$comparaison) & is.null(x$nb.max),
			 	sprintf (' sur 1 %s', get.unite(x$comparaison)), ifelse (
			 !is.null (x$comparaison) & inherits (x$comparaison, 'POSIXctp') & !is.null(x$nb.max),
			 	sprintf (' plus de %s fois tous les %i %ss', x$nb.max, get.val(x$comparaison), get.unite(x$comparaison)), ifelse (
			 !is.null (x$comparaison) & inherits (x$comparaison, 'POSIXctp') & is.null(x$nb.max),
			 	sprintf (' sur %i %s', get.val(x$comparaison), get.unite(x$comparaison)),
			 '') ) ) )
	gsub('%P', P,
	gsub('%t', t,
	gsub('%p', p,
	gsub('%s', s,
	gsub('%u', u,
	gsub('%b', b,
	gsub('%f', f, format)))))))
}

#' liste les differentes typologie de site possible
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
		      comparaison=POSIXctp(3, 'hour'), rep.comparaison=1, precision=0, nb.max=2,
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
		      comparaison=POSIXctp (3, 'hour'), rep.comparaison=1, precision=0, nb.max=2,
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
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=aot40, rep.b.comparaison=0.9,
		      mois=5:7-1, MORE.b.comparaison='mois',
		      precision=0,
		      seuil=6000, unite='microg/m3.h', reference='Décret 2010-1250 du 21 octobre 2010',
		      description="O3 : objectif de qualité pour la protection de la végétation. 6000 microg/m3.h en AOT40 de mai à juillet à ne pas dépasser."),
		list (polluant='08', cchim='O3', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison=depsur8h, rep.b.comparaison=0.75,
		      comparaison=sur3ans, precision=0, nb.max=25,
		      seuil=120, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2010-01-01'),
		      description="O3 : valeur cible pour la protection de la santé humaine. 120 microg/m3 pour le maximum journalier de la moyenne sur 8 heures à ne pas dépasser plus de 25 fois par an en moyenne sur 3 ans."),
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
		      comparaison=POSIXctp(3, 'hour'), rep.comparaison=1, precision=0, nb.max=2,
		      seuil=240, unite='microg/m3', reference='Décret 2010-1250 du 21 octobre 2010'),
		list (polluant='08', cchim='O3', type="seuil d'alerte", protection="la mise en oeuvre progressive de mesures d'urgence", sites=typologies, 
		      base.comparaison='hour', rep.b.comparaison=0.75,
		      comparaison=POSIXctp(3, 'hour'), rep.comparaison=1, precision=0, nb.max=2,
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
		      precision=2,
		      seuil=6, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AH', cchim='Arsenic', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=6, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='82', cchim='Cadmium', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=5, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AJ', cchim='Cadmium', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=5, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='87', cchim='Nickel', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=20, unite='ng/m3', reference='Décret 2010-1250 du 21 octobre 2010', applicable=as.POSIXct('2012-12-31')),
		list (polluant='AC', cchim='Nickel', type='valeur cible', protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
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
		      precision=0,
		      seuil=12, unite='microg/m3', reference='Directive 2008/50/CE du 21 mai 2008'),
		list (polluant='39', cchim='PM2.5', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=0,
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
		      precision=2,
		      seuil=2.4, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='80', cchim='Arsenic', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=3.6, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AH', cchim='Arsenic', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=2.4, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AH', cchim='Arsenic', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=3.6, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='82', cchim='Cadmium', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=2, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='82', cchim='Cadmium', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=3, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AJ', cchim='Cadmium', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=2, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AJ', cchim='Cadmium', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=3, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='87', cchim='Nickel', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=10, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='87', cchim='Nickel', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=14, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AC', cchim='Nickel', type="seuil d'évaluation inférieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
		      seuil=10, unite='ng/m3', reference='Directive 2004/107/CE du 15 décembre 2004'),
		list (polluant='AC', cchim='Nickel', type="seuil d'évaluation supérieur", protection='la santé humaine', sites=typologies, 
		      base.calcul='hour', rep.b.calcul=0.75, base.comparaison='year', rep.b.comparaison=0.9,
		      precision=2,
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

#' Seuils reglementaires pour la qualite de l'air
#' 
#' Cette fonction permet d'accéder à l'ensemble des seuils
#' réglementaires applicables en France pour la surveillance
#' de la qualité de l'air.
#' 
#' Les seuils concernés sont les objectifs de qualité, les valeurs
#' limtes, les valeurs cibles et niveaux critiques définis
#' pour la protection de la santé humaine et de la végétation.
#' Les seuils d'évaluation supérieur et inférieur sont également
#' définis, ainsi que les seuils d'information et de recommandation
#' et les seuils d'alertes.
#'
#' L'ensemble des polluants concernés par ces seuils sont pris
#' en compte : NO2, NOx, PM10, PM2.5, plomb, SO2, O3, CO, C6H6,
#' arsenic, cadmium, nickel et benzo(a)pyrène.
#'
#' Les modalités d'application des 
#{ seuils d'information et de recommandation et les seuils
#' d'alerte dépendant d'arrêtés régionaux, ils sont donnés
#' ici à titre indicatif.
#'
#' Pour récupérer un ou plusieurs seuil(s), il suffit de préciser
#' les paramètres souhaités. La structure d'un \sQuote{seuil}
#' n'est pas très lisible en soit car elle permet de réaliser
#' la comparaison d'une mesure au seuil  à l'aide des fonctions
#' \code{\link{preparation.base}}, \code{\link{comparaison}} \code{\link{validation.reglementaire}}
#' (pour plus de détails, se reporter aux-dites fonctions).
#'
#' @param type type du seuil recherché : "objectif de qualité",
#' 	"seuil d'information et de recommandation", "seuil d'alerte",
#' 	"valeur limite", "niveau critique", "valeur cible",
#' 	"seuil d'évaluation inférieur" ou "seuil d'évaluation supérieur".
#' @param protection domaine pour lequel est défini le seuil :
#' 	"la santé humaine", "la végétation"
#' 	ou "la mise en oeuvre progressive de mesures d'urgence".
#' @param polluant code ISO du polluant auquel s'applique le seuil
#' (tel que défini dans les bases XR ou Pol'air).
#' 	("03", "12", "24", "39", "19", "AA", "01", "08", "04", "V4", "80",
#' 	"AH", "82", "AJ", "87", "AC", "P6")
#' @param cchim code plus \sQuote{compréhensible} pour chaque polluant.
#' 	("NO2", "NOx", "PM10", "PM2.5", "Plomb", "SO2", "O3", "CO",
#' 	"C6H6", "Arsenic", "Cadmium", "Nickel", "B(a)P")
#' @param sites typologie des sites auxquels le seuil est applicable
#' 	(cf \code{\link{typologies}}).
#'
#' @return
#' l'objet retourné est une liste constituée de tous les seuils qui 
#' correspondent aux paramètres entrés. Un seuil est en réalité
#' une liste (la classe \sQuote{seuil} sert principalement à permettre
#' un affichage plus lisible de chaque seuil) contenant les éléments 
#' suivants :
#' \item{polluant}{code ISO du polluant}
#' \item{cchim}{code chimique du polluant}
#' \item{type}{type de seuil (OQ, VL, etc.)}
#' \item{protection}{domaine ciblé par le seuil}
#' \item{sites}{typologie des sites où le seuil est applicable}
#' \item{base.calcul}{Pour certains calcul, les données doivent 
#'	avant tout être préparées d'une certaine manière. Il peut
#' 	s'agir de les mettre au format horaire (ce qui est le cas
#' 	le plus courant). Si l'opération à réaliser est une simple
#' 	conversion temporelle, \code{base.calcul} peut être une 
#' 	chaîne de caractères utilisable comme \code{unit} pour la 
#' 	la fonction \code{\link[timetools]{POSIXctp}}
#' 	('hour' par exemple) ou directement un objet \code{\link[timetools]{POSIXctp}}.
#' 	Si l'opération de préparation est plus complexe, \sQuote{base.calcul}
#' 	peut être directement une fonction à appliquer aux données. NULL
#' 	si aucun préparation n'est nécessaire.}
#' \item{rep.b.calcul}{Dans le cas où \code{base.calcul} est un 
#' 	\code{\link[timetools]{POSIXctp}} (ou une chaîne de caractères),
#' 	\sQuote{rep.b.calcul} est la représentativité minimale
#' 	en-dessous de laquelle la conversion temporelle donnera NA.}
#' \item{base.comparaison}{Dans certains cas, les mesures ne sont
#' 	par directement comparées au seuil : elles doivent être 
#' 	préalablement transformées. Dans le cas où la transformation
#' 	est uniquement temporelle (concentration moyenne annuelle par
#' 	exemple), \code{base.comparaison} est une
#' 	chaîne de caractères utilisable comme \code{unit} pour la fonction
#'	\code{\link[timetools]{POSIXctp}}
#' 	('hour' par exemple) ou directement un objet \code{\link[timetools]{POSIXctp}}.
#' 	Pour des transformations plus complexes (calcul d'AOT40), 
#' 	\code{base.comparaison} peut être directement une fonction. NULL
#'	si aucune transformation n'est requise.}.
#' \item{rep.b.comparaison}{Dans le cas où \code{base.comparaison}
#'	correspond à une transformation temporelle, \code{base.rep.comparaison}
#' 	est la représentativité minimale 
#' 	en-dessous de laquelle la conversion temporelle donnera NA.}
#' \item{precision}{Entier indiquant la précision à laquelle la 
#' 	comparaison doit être effectuée.}
#' \item{seuil}{Valeur numérique à laquelle sont comparées les mesures
#' 	ou les mesures transformées pour déterminer s'il y a dépassement.}
#' \item{unite}{Chaîne de caractères symbolisant dans quelle unité 
#' 	doivent être exprimées les concentrations du polluant concerné
#' 	par le seuil.}
#' \item{reference}{Chaîne de caractères précisant de quel texte 
#' 	réglementaire est issu le seuil.}
#' \item{comparaison}{Soit NULL si les données préparées avec
#' 	\code{base.comparaison} ont juste à être comparées au seuil ;
#'	soit un objet \code{\link[timetools]{POSIXctp}} (ou chaîne de caractères correspondante)
#'	si, une fois comparées, les données préparées doivent être 
#' 	agrégées sur cette période (nombre limite de dépassements journaliers
#' 	sur une année, etc.) ; directement une fonction dans les cas 
#' 	plus complexes (notamment en cas d'existance de marges de dépassement).}
#' \item{rep.comparaison}{Dans le cas où \code{comparaison}
#'	correspond à une transformation temporelle, \code{rep.comparaison}
#' 	est la représentativité minimale 
#' 	en-dessous de laquelle la conversion temporelle donnera NA.}
#' \item{nb.max}{Dans les cas où le seuil est un nombre à ne pas dépasser
#' 	sur une période, le nombre en question.}
#' \item{applicable}{\code{\link{POSIXct}} donnant la date d'application
#' du seuil, ou NULL.}
#' \item{description}{L'affichage d'un seuil est normalement déduit automatiquement
#' 	de ses caractéristiques (\code{\link{format.seuil}}). Cependant
#' 	dans certains cas, cette déduction n'est pas possible. Il est alors
#' 	possible de préciser la chaîne représentation le seuil ici.}
#' 
#' Le détail fourni ci-dessus doit permettre de définir d'autres seuils
#' utilisable avec \code{\link{validation.reglementaire}} sans pour
#' autant que ces seuils soient \sQuote{réglementaires}.
#'
#'@seealso \code{\link{typologies}} \code{\link{validation.reglementaire}},
#' \code{\link{comparaison}}, \code{\link{preparation.base}}, \code{\link{format.seuil}}
#'
#' @references
#' Décret 2010-1250 du 21 octobre 2010, Directive 2008/50/CE du 21 mai 2008, Directive 2004/107/CE du 15 décembre 2004
#' 
seuils <- function (type=c("objectif de qualité",
	"seuil d'information et de recommandation",
	"seuil d'alerte", "valeur limite", "niveau critique", "valeur cible",
	"seuil d'évaluation inférieur", "seuil d'évaluation supérieur"),
		    protection=c("la santé humaine", "la végétation",
	"la mise en oeuvre progressive de mesures d'urgence"),
		    polluant=c("03", "12", "24", "39", "19", "AA", "01",
	"08", "04", "V4", "80", "AH", "82", "AJ", "87", "AC", "P6"),
		    cchim=c("NO2", "NOx", "PM10", "PM2.5", "Plomb", "SO2", "O3",
	"CO", "C6H6", "Arsenic", "Cadmium", "Nickel", "B(a)P"),
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


