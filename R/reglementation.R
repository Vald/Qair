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
	if (nrow(x) == 0) stop ("Il n'est pas possible de calculer un AOT 'mai-juillet' si aucune donnée pour cette période n'est fournie.")
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

margevlPM25 <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2011:2013, 2015), seuil=29:25) )
	if (!detail) {
		if (use.marges)
			val.seuil <- (29:25)[findInterval(year(start(x)), c(1900, 2011:2013, 2015))] else 
			val.seuil <- 25
		x[T] <- data.frame (x) > val.seuil
		x@data <- data.frame (lapply (x@data, as.logical))
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

	if (!detail) {
		x[T] <- data.frame (x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
		x@data <- data.frame (lapply (x@data, as.logical))
	}
	return (x)
}

margevlNO2y <- function (x, seuil, detail, use.marges=TRUE, get.marges=FALSE, ...) {
	if (get.marges) return (data.frame (annee=c(1900, 2001:2010), seuil=30:20*2) )
	if (!detail) {
		if (use.marges)
			val.seuil <- (30:20*2)[findInterval(year(start(x)), c(1900, 2001:2010))] else 
			val.seuil <- 40
		x[T] <- data.frame (x) > val.seuil
		x@data <- data.frame (lapply (x@data, as.logical))
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
		x@data <- data.frame (lapply (x@data, as.logical))
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
	valid.x <- RegularTimeIntervalDataFrame (floor_date(min(start(x)), 'year'),
						 ceiling_date(max(end(x)), 'year'),
						 'year', timezone(x))
	valid.x <- project (x[month(start(x))%in% 4:9,], valid.x,
			    split.from=FALSE, merge.from=TRUE,
			    function(x) sum (!is.na(x)) > 4,
			    min.coverage=0)
	valid.x[is.na(data.frame(valid.x))] <- 0
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
	if (!detail) {
		new.x[T] <- data.frame (new.x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
		x@data <- data.frame (lapply (x@data, as.logical))
	}
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
	if (!detail) {
		new.x[T] <- data.frame (new.x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
		x@data <- data.frame (lapply (x@data, as.logical))
	}
	return (new.x)
}

#' Mise en forme des donnees avant calculs reglementaires
#' 
#' Cette fonction permet de s'assurer que les données utilisées
#' au moment de l'application des calculs réglementaires sont
#' au bon format.
#'
#' Suivant les seuils, il peut y avoir une ou deux "pré"-paration(s)
#' à réaliser avant calcul, l'approche est cependant identique 
#' dans les cas et une seule fonction est nécessaire.
#' Voir \code{\link{validation.reglementaire}} pour l'explication
#' détaillée de l'articulation de ces deux pré-calculs avant
#' application des seuils.
#'
#' @param x jeu de données (\code{\link[timetools]{TimeIntervalDataFrame-class}})
#' 	contenant les données à mettre en forme
#' @param seuil seuil (cf \code{\link{seuis}}) pour lequel les données
#' 	doivent être mise en forme
#' @param base chaîne de caractères indiquant si la préparation
#' 	est celle de l'étape \sQuote{calcul} ou \sQuote{comparaison}.
#' 	Pour chaque seuil, les paramètres pouvant être différents entre
#'	ces deux étapes, il est important de bien préciser celle
#'	qui doit être réalisée (cf \code{\link{validation.reglementaire}}
#' 	pour l'articulation des ces deux étapes).
#' @param check.720 booléen indiquant si, lorsque cela est pertinent,
#' 	la règle des \sQuote{720 heures consécutives} doit être vérifiée
#' 	ou non (cette règle apparaît dans un guide \sQuote{ADEME} mais
#'	dans aucun texte réglementaire).
#' @param \dots arguments supplémentaires pouvant être transmis aux 
#' 	fonctions appelées dans le cadre du calcul. Le plus souvent, 
#' 	il peut s'agir de l'argument \code{use.marges} (TRUE ou FALSE)
#' 	qui précise si le calcul doit être réalisé en prenant comme 
#' 	seuil sa valeur définitive, ou bien cette valeur \sQuote{réhaussée}
#'	de la marge de dépassement applicable (lorsque cela est pertinent).
#'
#' @seealso \code{\link[timetools]{TimeIntervalDataFrame-class}},
#' \code{\link{validation.reglementaire}}, \code{\link{seuils}},
#' \code{\link{comparaison}}
#'
#' @return un objet de classe \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' qui contient les valeurs préparées en tenant des critères de validité du seuil.
#' Si les critères ne sont pas respectés, les valeurs sont NAnifiées.
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

#' Compare un jeu de donnees a des valeurs réglementaires
#'
#' Cette fonction prend un jeu de données au format
#' \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' et le compare a un seuil réglementaire.
#'
#' La fonction ne fait aucune autre action que comparer
#' les données au seuil : aucune vérification sur le bon
#' formatage des données n'est réalisées (base horaire, annuelle, etc.)
#' Pour cela, il est possible de préparer les données au moyen de
#' la fonction \code{\link{preparation.base}}.
#' C'est ce que réalise la fonction \code{\link{validation.reglementaire}}.
#' Se reporter à cette dernière pour voir l'articulation de
#' la fonction \code{\link{preparation.base}} avec la 
#' fonction \code{\link{comparaison}}.
#'
#' @param detail booléen indiquant si le détail doit être retourné ou non
#' @inheritParams preparation.base
#' @param \dots arguments supplémentaires éventuellement
#' 	utilisés lorsqu'une fonction spécifique est utilisée
#' 	pour la comparaison avec le seuil (cf \code{\link{validation.reglementaire}}
#' 	pour l'explication du mécanisme de calcul).
#'
#' @seealso \code{\link[timetools]{TimeIntervalDataFrame-class}},
#' \code{\link{validation.reglementaire}}, \code{\link{seuils}},
#' \code{\link{preparation.base}}
#'
#' @return un objet de classe \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' qui contient le résultat de la comparaison. Si le détail est demandé
#' la valeur (si le seuil consiste simplement en une valeur à ne pas
#' dépasser) ou le nombre de dépassement du seuil (s'il
#' le seuil ne doit pas être dépassé plus d'un nombre de fois donné)
#' est retourné. TRUE (critère dépassé) ou FALSE (critère respecté) si le détail n'est pas demandé.
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
		
		if (!detail) {
			x[T] <- data.frame (x) > if (!is.null (seuil$nb.max)) seuil$nb.max else 0
			x@data <- data.frame (lapply (x@data, as.logical))
		}
	} else stop ('Impossible de calculer un résultat.')

	return (x)
}

#' Applique strictement des seuils reglementaires a des valeurs donnees
#'
#' À partir d'un jeu de donnée et d'un seuil réglementaire (cf
#' code{\link{seuils}}, cette fonction permet de tester si les données
#' respectent le seuil. Les différents arguments de la fonction
#' permettent de contrôler les différentes étapes.
#'
#' La fonction \code{validation.reglementaire} permet de
#' vérifier qu'un jeu de données respecte un critère réglementaire
#' donné. Cette vérification est faite par un appel successif
#' aux fonctions \code{preparation.base} (avec l'argument \code{base}
#' = \code{'calcul'} puis \code{'comparaison'}) et \code{comparaison}.
#' Ainsi cette fonction permet de préparer et d'assurer qu'un je de 
#' données est correctement formaté avant de le comparer à un
#' critère réglementaire.
#'
#' Si l'utilisateur sait que ses données sont correctes, 
#' ou si elles sont incorrectes (pas la bonne couverture temporelle
#' par exemple), mais qu'il souhaite quand même comparer
#' ses données au seuil, il peut désactiver les étapes 
#' de préparation en précisant l'argument :\cr
#' \code{etapes='comparaison'}\cr
#' ou n'importe quelle combinaison des 3 valeurs possibles.
#' 
#' Note: quel que soit dans lequel sont indiqués les étapes
#' souhaitées, ces dernières sont toujours effectuées dans l'ordre 
#' suivante :
#' \enumerate{
#' \item preparation.calcul ;
#' \item preparation.comparaison ;
#' \item preparation.comparaison ;
#'}
#'
#' Pour une utilisation plus avancée de la fonction, se reporter
#' aux différentes sections suivantes.
#'
#' @section Introduction:
#' La comparaison de données à des seuils réglementaire doit être
#' appliquée de manière rigoureuse afin d'assurer la fiabilité des 
#' résultats. D'autre part, afin de facilité le maintien des 
#' calculs qui réalisent la comparaison, il est intéressant 
#' d'uniformiser au maximum la procédure de calcul. Ainsi, 
#' en cas de modification du mécanisme, tous les calculs peuvent
#' bénificier de la modification.
#'
#' Pour l'utilisateur final, le fait que la procédure est ou non
#' uniformisée est normalement transparent. Les détails donnés ici
#' s'adresse donc principalement à ceux qui souhaitent aller plus loin
#' dans la compréhension de l'implémentation des calculs.
#'
#' @section Processus de comparaison:
#' Pour comprendre comment a été structuré la procédure
#' de calcul il est nécessaire de \sQuote{modéliser} cette procédure, 
#' en quoi (quelles étapes) consiste exactement \sQuote{comparer des données
#' à un seuil réglementaire}.
#'
#' Tout d'abord \sQuote{comparer à un seuil} n'est pas la terminologie 
#' la plus appropriée : il s'agit en réalité de savoir si un niveau
#' de pollution respecte ou non certains critères.
#' Chaque critère doit alors répondre à un certain nombre de questions
#' pour pouvoir être défini :
#' \itemize{
#' \item Quel \sQuote{niveau} doit être comparé. Une concentration,
#'	moyenne annuelle, des concentrations horaires, journalières,
#' 	quelques choses de plus complexe (AOT40, valeur journalière
#'	maximale des moyennes 8 heures, etc).
#' \item Quelle valeur ne doit pas être dépassée ?
#' \item Combien de fois cette valeur ne doit-elle pas être dépassée ?
#' \item Sur combien de temps ne doit-elle pas être dépassée ?
#'	(Nombre de moyennes journalières ne devant pas dépasser un
#'	seuil sur 1 an, moyenne horaire à ne pas dépasser sur 3 heures
#' consécutives, etc.).
#' \item Si la valeur à comparer est une valeur agrégée (moyenne annuelle,
#' journlaière), à partir de quelles données de base doit-elle être 
#' calculée ? (ici, soit la donnée de base est horaire, soit elle 
#' n'est pas définie)
#' }
#' 
#' Au travers de ces questions, il ressort que jusqu'à trois étapes
#' peuvent être nécessaires dans la vérification du respect
#' de critères réglementaires :
#' \itemize{
#' \item préparation/vérification de la donnée de base pour le calcul (
#' les données sont-elles bien horaires, sinon, peuvent-elles être 
#' transformées en horaire ?) ;
#' \item préparation/vérification de la donnée de base pour la 
#' comparaison (une fois en horaire, la donnée doit-elle être
#' convertie en tri-horaire, journalière, autre ? ) ;
#' \item vérification du respect du critère réglementaire.
#' }
#' Ces trois étapes sont détaillées par la suite.
#'
#' @section Préparation de la donnée de base:
#' Comme indiqué précédemment, la préparation de la donnée
#' de base est requise uniquement lorsque la donnée
#' à comparer doit être calculée sur une base spécifique
#' (base horaire par exemple le plus souvent).
#' 
#' Dans ce cas, les informations nécessaires sont :
#' \itemize{
#' \item la base temporelle (horaire le plus souvent) ou une méthode
#' permettant d'obtenir cette base ;
#' \item le pourcentage de représentativité minimal
#' requis pour qu'une données soit calculée dans le  cas d'une 
#' base temporelle.
#' } 
#' Dans un seuil (cf \code{\link{seuils}}), ces informations
#' sont stockées dans \code{base.calcul} et \code{rep.b.calcul}.
#' Dans le cas où il ne s'agit pas d'une base temporelle
#' mais d'une méthode d'obtention, \code{base.calcul} est
#' alors une fonction prenant comme argument \code{x} (les données
#' à transformer) et \code{seuil} (le seuil en question) et éventuellement
#' d'autres arguments récupérables au moyen des \code{\dots} de la fonction
#' \code{validation.reglementaire}.
#'
#' Si cette étape n'est pas requise, les deux éléments indiqués
#' ne sont pas renseignés pour le seuil.
#'
#' La donnée obtenue peut alors être soumise aux fonctionnalités de
#' préparation de la comparaison.
#'
#' Cette étape est réalisée au moyen de la fonction \code{\link{preparation.base}}
#' avec l'argument \code{base = 'calcul'}.
#' 
#' @section Préparation de la donnée de comparaison:
#' La préparation de la donnée avant comparaison est une opération
#' plus souvent requise et parfois plus complexe que la préparation
#' de la donnée de base de calcul.
#' 
#' Son fonctionnement est cependant identique :
#' \itemize{
#' \item soit la donnée doit être agrégée sur une base temporelle
#' \item spécifique avant d'être comparée (moyenne annuelle, moyenne
#' journalière, etc), auquel cas \code{base.comparaison} fournit
#' cette base (\code{'day'}, \code{year}) et \code{rep.b.comparaison}
#' indique la représentativité minimale pour préparer la donnée ;
#' \item soit la donnée doit être agrégée de façon plus complexe (
#' AOT40, AOT40 en moyenne sur 5 ans, valeur maximale journalière
#' de la valeur moyenne sur 8 heures, etc), auquel cas \code{base.comparaison}
#' est une fonction qui permet de réaliser cette agrégation avec
#' comme argument \code{x} (les données
#' à transformer) et \code{seuil} (le seuil en question) et éventuellement
#' d'autres arguments récupérables au moyen des \code{\dots} de la fonction
#' \code{validation.reglementaire}.
#' }
#'
#' Quelque soit le cas, la donnée obtenue doit pouvoir 
#' être directement comparée au seuil (à l'exception de quelques
#' rares cas : cf section \sQuote{Vérification du respect du critère}).
#'
#' Cette étape est réalisée au moyen de la fonction \code{\link{preparation.base}}
#' avec l'argument \code{base = 'comparaison'}.
#' 
#' @section Vérification du respect du critère:
#' Une fois que les données sont prêtes, normalement il n'y a plus 
#' qu'à comparer avec le seuil du critère. Cependant, lorsqu'un seuil
#' est \sQuote{à ne pas dépasser plus de x fois sur une période}, 
#' l'information permettant de savoir si le critère est respecté est
#' le nombre de dépassement du seuil. Dans ce cas, le seuil
#' possède un élément nommé \code{comparaison} qui indique de quelle
#' période il s'agit (\code{'year'} le plus souvent, mais peut être
#' \code{new_period(hour=3)} pour des seuils d'alerte).
#'
#' Dans ce cas, le seuil possède deux autres éléments.
#' Le premier, \code{rep.comparaison}, 
#' indique la représentativité minimale de la période qui doit
#' être couverte pour que la comparaison soit validée.
#' Le second \code{nb.max} indique le nombre de dépassements maximal
#' autorisés.
#'
#' Dans le cas où il existe des marges de dépassement, autrement dit :
#' quand le seuil varie en fonction du temps, \code{comparaison}
#' est une fonction qui réalise la comparaison. Cette fonction
#' possède au minimum les arguments suivants :
#' \describe{
#' \item{x}{données à comparer au seuil}
#' \item{seuil}{le seuil lui-même}
#' \item{detail}{booléen indiquant si la fonction doit retournée
#' le détail ou juste si le critère est respecté (prend la valeur
#' indiquée à l'appel de la fonction \code{\link{comparaison}})}
#' \item{use.marges}{booléen indiquant si le calcul doit être réalisé
#' avec la valeur applicable augmentée de la marge de dépassement}
#' \item{get.marges}{dans le cas d'un appel direct à la fonction, 
#' retourne une data.frame contenant l'évolution de la marge.
#' L'utilisation se fait de la sorte :\cr
#' \code{seuils()[[61]]$comparaison(get.marges=TRUE)}.
#' }
#' \item{\dots}{Éventuellement d'autres arguments}
#' }
#'
#' Si le détail est demandé
#' la valeur (si le seuil consiste simplement en une valeur à ne pas
#' dépasser) ou le nombre de dépassement du seuil (s'il
#' le seuil ne doit pas être dépassé plus d'un nombre de fois donné)
#' est retourné. TRUE ou FALSE si le détail n'est pas demandé.
#'
#' Cette étape est réalisée par un appel à la fonction \code{\link{comparaison}}.
#' La valeur de \code{detail} prise dans la fonction est 
#' celle donnée en argument à l'appel de \code{validation.reglementaire}.
#'
#' @section Autres éléments définissant un critère:
#' En plus des éléments présentés ci-dessus, un seuil est défini
#' par d'autres \sQuote{méta}-données. Ces dernières sont 
#' précisées dans la définition de la fonction \code{\link{seuils}}.
#' Une liste \sQuote{castée} en tant que \code{'seuil'} qui respecte
#' les définitions précédentes ainsi que celles données dans la 
#' documentation d'un seuil peut parfaitement être utilisée en argument
#' de la fonction \code{validation.reglementaire}. Par exemple, 
#' dans le cas où il est nécessaire de calculer la moyenne annuelle
#' en ozone et de vérifier que cette moyenne ne dépasse pas 50 microgrammes par mètre cube
#' (quelle drôle d'idée ...), c'ets possible : voir \sQuote{examples}.
#' 
#' @section Fonctionnalités avancées:
#' Pour finir, l'argument \code{\dots} permet de personnaliser
#' à l'envie le fonctionnement de la fonction
#' \code{validation.reglementaire}. En effet, en ajoutant
#' un argument dont le nom correspond à l'un des éléments 
#' du seuil, la valeur de cet élément va être \sQuote{écrasée}
#' au moment du calcul.
#'
#' Ainsi, pour les seuils à respecter en moyenne annuelle, 
#' le taux de représentativité exigé réglementairement est 90%.
#' La valeur de l'élément \code{rep.comparaison} est alors 0.9.
#' Lors d'un suivi \sQuote{à la volée} des données de mesures
#' il est possible de trouver ce 90% trop contraignant
#' (lors d'une campagne mobile de 2 mois, on souahite pouvoir
#' s'informer du respect ou non du critère, sans pourtant
#' qu'il soit possible de respecter les 90%).
#' 
#' Il suffit alors de préciser l'argument suivant \code{rep.comparaison=0},
#' et la comparaison sera effectuée (n'importe quelle mesure
#' représentant normalement plus de 0% d'une année). voir la section
#' \sQuote{examples}.
#'
#' De cette manière, n'importe quel paramètre du seuil peut être modifié.
#' Cette fonctionalité est toutefois à utiliser avec précaution :
#' le remplacement du calcul de la valeur maximale journalière 
#' de la moyenne glissante sur 8 heures par un AOT pourrait avoir 
#' des conséquences inattendues (ou pas).
#' 
#' @param seuils list de \code{\link{seuils}} à appliquer au jeu de données.
#' @param etapes indique si les données doivent d'abord être préparées
#'	avant d'appliquer le seuil, et quelles préparations doivent 
#' 	être réalisées (\sQuote{calcul} et/ou \sQuote{comparaion}).
#'	Voir la section \sQuote{Details} pour plus d'infos.
#' 	Si la comparaison doit être réalisée, cette info doit également
#' 	être indiquée.	
#' @param resultat dans le cas où la comparaison est demandée, 
#'	indique si le résultat doit contenir le détail
#'	des calculs, juste le bilan, ou les deux (voir \sQuote{Value}).
#' @param \dots tout argument pouvant être transmis aux autres fonctions
#' 	\sQuote{use.marges} par exemple (cf \code{\link{comparaison}} et
#' 	\code{\link{preparation.base}}).
#'	
#'	Il est également possible de mettre en argument l'un des attributs
#'	du seuil (cf \code{\link{seuils}}) pour remplacer sa valeur par 
#' 	défaut (par exemple, dans le calcul de l'AOT, il est possible
#' 	de remplacer le seuil de 6000 par 8000 et ajoutant
#'	\code{seuil=8000} en argument).
#' @inheritParams preparation.base
#' 
#' @seealso \code{\link[timetools]{TimeIntervalDataFrame-class}},
#' \code{\link{comparaison}}, \code{\link{seuils}},
#' \code{\link{preparation.base}}
#'
#' @examples
#' # création d'un jeu de données 'bidon'
#' #-------------------------------------
#' donnees <- RegularTimeIntervalDataFrame('2010-01-01', '2010-03-01', 'hour', 'UTC')
#' donnees$yo <- sample (100, nrow(donnees), TRUE)
#' head (donnees)
#' summary (donnees)
#'
#' # définition d'un nouveau seuil
#' #------------------------------
#' mon.seuil <- list(base.comparaison='year', rep.comparaison=0.75,
#'	seuil=50, polluant='08', cchim='O3', type='personnalisé',
#'	description="seuil personnalisé pour l'ozone",
#'	sites=typologies)
#'	# typologies est un vecteur qui contient toutes les
#' # typologies définies
#'
#' # c'est bien une liste !!
#' mon.seuil
#' class(mon.seuil) <- 'seuil'
#' # ça devient un seuil :)
#' mon.seuil
#' 
#' # redéfinition 'locale' d'un paramètre
#' #-------------------------------------
#' # sélection d'un seuil pour tester
#' seuil <- seuils(cchim='CO', type='valeur limite')
#' seuil[[1]]
#'
#' validation.reglementaire(donnees, seuil, resultat='comparaison')
#' validation.reglementaire(donnees, seuil, resultat='comparaison', rep.comparaison=0)
#'
#' @return un objet de classe \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' qui contient soit les données préparées (en fonction de ce qui a été demandé)
#' soit le résultat de la comparaison. Si le détail est demandé
#' la valeur (si le seuil consiste simplement en une valeur à ne pas
#' dépasser) ou le nombre de dépassement du seuil (s'il
#' le seuil ne doit pas être dépassé plus d'un nombre de fois donné)
#' est retourné. TRUE ou FALSE si le détail n'est pas demandé.
#' Renvoi les deux si c'est demandé.
validation.reglementaire <- function (x, seuils,
				      etapes=c('preparation.calcul', 'preparation.comparaison', 'comparaison'),
				      resultat=c('detail', 'comparaison'), check.720=TRUE, ...) {
	etapes <- match.arg (etapes, several.ok=TRUE)
	resultat <- match.arg (resultat, several.ok=TRUE)

	param.to.change <- list (...)
	if (length (param.to.change) > 0)
		for (i in 1:length(seuils)) {
			ptc <- param.to.change[names (param.to.change) %in% names(seuils[[i]])]
		    seuils[[i]][names (ptc)] <- ptc
		}

	if ('preparation.calcul' %in% etapes) {
		prepas.cal <- lapply (seuils, '[', c('base.calcul', 'rep.b.calcul', 'precision'))
		prepas.cal.unique <- unique (prepas.cal)
		x <- lapply (prepas.cal.unique, preparation.base, x=x, base='calcul', check.720=check.720)
		names (x) <- sapply (lapply (prepas.cal.unique, as.character), paste, collapse='-')
		attributes(x)$prepas.cal <- prepas.cal.unique 
	}
	if ('preparation.comparaison' %in% etapes) {
		prepas.comp <- lapply (seuils, '[', c('base.comparaison', 'rep.b.comparaison', 'precision'))
		if ('preparation.calcul' %in% etapes) {
			prepas.comp <- split (prepas.comp,
					     sapply (lapply (prepas.cal, as.character), paste, collapse='-') )
		     	prepas.cal.unique <- attributes(x)$prepas.cal
			x <- mapply (function (x, prepas.comp, check.720) {
					prepas.comp.unique <- unique (prepas.comp)
					x <- lapply (prepas.comp.unique, preparation.base,
						     x=x, base='comparaison', check.720=check.720)
					names (x) <- sapply (lapply (prepas.comp.unique, as.character),
							     paste, collapse='-')
					attributes(x)$prepas.comp <- prepas.comp.unique
					x
				},
				x, prepas.comp[names(x)], check.720, SIMPLIFY=FALSE)
			names (x) <- sapply (lapply (prepas.cal.unique, as.character), paste, collapse='-')
			attributes(x)$prepas.cal <- prepas.cal.unique
		} else {
			prepas.comp.unique <- unique (prepas.comp)
			x <- lapply (prepas.comp.unique, preparation.base, x=x, base='comparaison', check.720=check.720)
			names (x) <- sapply (lapply (prepas.comp.unique, as.character), paste, collapse='-')
			attributes(x)$prepas.comp <- prepas.comp.unique 
		}
	}
	if ('comparaison' %in% etapes) {
		y <- list()
		for (i in 1:length (seuils) ) {
			# selection de la donnee preparee en fonction du seuil qui va etre teste
			if (all (c('preparation.calcul', 'preparation.comparaison') %in% etapes) ) {
				n.prepa.cal <-
					paste (as.character(seuils[[i]][c('base.calcul', 'rep.b.calcul', 'precision')]), collapse='-')
				n.prepa.comp <- 
					paste (as.character(seuils[[i]][c('base.comparaison', 'rep.b.comparaison', 'precision')]), collapse='-')
				z <- x[[n.prepa.cal]][[n.prepa.comp]]
			} else if (any (c('preparation.calcul', 'preparation.comparaison') %in% etapes) ){
				n.prepa <- sub ('preparation.', '', setdiff(etapes, 'comparaison'))
				n.prepa <- 
					paste (as.character(seuils[[i]][c(sprintf('base.%s', n.prepa),
									  sprintf('rep.b.%s', n.prepa),
									  'precision')]), collapse='-')
				z <- x[[n.prepa]]
			} else {
				z <- x
			}

			if ('detail' %in% resultat) {
				det <- comparaison (z, seuils[[i]], detail=TRUE, check.720)
				if (!'comparaison' %in% resultat)
					y[[i]] <- det
			}

			if ('comparaison' %in% resultat) {
				comp <- comparaison (z, seuils[[i]], detail=FALSE, check.720)
				if ('detail' %in% resultat) {
					y[[i*2-1]] <- det
					y[[i*2]] <- comp
				} else
					y[[i]] <- comp
			}
			rm (z)
		}
	x <- y
	}
	
	return (x)
}

