#' Calcul de roses des vents
#'
#' Cette fonction permet de calculer des "roses des vents".
#'
#' Le résultat de cette fonction peut être directement utilsé par la 
#' fonction \code{\link{plot.rosevent}} pour afficher graphiquement 
#' la rose des vents. La fonction \code{\link{rosevent}}
#' est un raccourci pour l'emploi successif de ces deux fonctions.
#'
#' Les deux vecteurs \sQuote{dv}, \sQuote{fv} doivent avoir le 
#' même nombre d'éléments.
#'
#' La fonction \code{compute.rosevent} utilise la fonction \code{\link{compute.rose}}
#' pour faire les calculs à proprement parler. Pour avoir des détails
#' sur l'utilisation/la définition des paramètres \code{dv}, \code{fv} et
#' \code{breaks} il est possible de se reporter à la-dite fonction.
#'
#' La notion de \sQuote{vent calme} est prise en compte dans le calcul des roses
#' des vents. Pour une définition exacte, se référer à la section \sQuote{References}.
#' D'une manière simplifiée, les vents calmes sont des vents dont la vitesse
#' est trop faible pour considérer qu'un régime de vent est établi.
#' Dans ce cas, le vent n'est pas comptabilisé comme venant d'une direction particulière,
#' il est traité comme un vent nul.
#'
#' Les vents calmes sont définis à l'aide du paramètre \code{vlims}.
#' \itemize{
#' \item si la valeur \code{0} est contenue dans \code{vlims}, alors 
#' 	les vents calmes ne sont pa pris en compte et seuls les vents
#' 	nuls sont traités à part ;
#' \item si la valeur \code{0} n'est pas dans \code{vlims}, alors
#' 	les vents calmes sont définis comme les vents ayant une vitesse 
#' 	inférieure ou égale à la plus faible valeur spécifiée dans \code{vlims}.
#' }
#' 
#' @param dv vecteur \sQuote{numeric} contenant les directions du vent en degrés
#'	(0 -> Nord, 90 -> Ouest, etc.).
#' @param fv vecteur \sQuote{numeric} contenant la vitesse du vent en m/s.
#'	Les valeurs de \sQuote{values} pour lesquelles \sQuote{fv} est 0 ou NA
#'	ne sont pas prises en compte.
#'	(Attention : la prise en compte de ce paramètre risque d'évoluer à l'avenir).
#' @param breaks vecteur \sQuote{numeric} indiquant les limites entre chaque secteur
#' 	de vents (en degrés). Si une valeur indiquée est en-dehors de [0-360], 
#'	elle est prise en compte, mais modulo 360.
#' 
#'	Les valeurs par défaut correspondent aux 16 points cardinaux (N, NNE, NE, 
#'	ENE, E, ESE, SE, SSE, S, etc.).
#' @param vlims vecteurs contenant les limites des différentes classes de vitesses 
#' 	de vent. Voir la section \sQuote{Details} pour la prise en compte des vents
#' 	calmes.
#'
#' @return une liste de listes structurées de manière identique à ce qui
#' 	est obtenu en utilisant la fonction \code{\link{compute.rose}} ainsi
#' 	qu'un élément supplémentaire : \code{vlim}.
#' 	Chaque élément de la liste contient les éléments suivants :
#' \item{breaks}{un vecteur \sQuote{numeric} contenant les limites des secteurs de vents.
#'		Voir la section\sQuote{Details} pour plus de précisions.}
#' \item{values}{Vecteur \sQuote{numeric} de longueur égale au nombre de secteurs de vent
#'		 définis.}
#' \item{opened}{Un facteur indiquant si les secteurs de vent sont ouverts à gauche ou à droite
#'		(pour l'instant, seuls les secteurs ouverts à gauche sont pris en compte).}
#' \item{vlim}{Valeur numérique correspondant à la vitesse de vent à laquelle correspond
#'		l'élément en question (\code{0} correspond aux vents nuls et calmes).}
#'
#' @references \href{http://comprendre.meteofrance.com/jsp/site/Portal.jsp?&page_id=2866&document_id=1195&portlet_id=1796}{Meteo France}
#'
#' @seealso \code{\link{plot.rosevent}}, \code{\link{rosevent}}, \code{\link{compute.rose}}, \code{\link{rosepol}} 
compute.rosevent <- function(dv, fv, breaks=0:17*22.5+22.5/2, vlims) {
	if (missing (vlims) )
		vlims <- c(2, 5, 8, max (fv, na.rm=TRUE) )
	vlims <- sort (unique (vlims) )
	null.is.zero <- 0 %in% vlims
	fv[!is.na(fv) & fv <= min (vlims)] <- 0
	vlims <- vlims[vlims != 0]

	select <- !is.na (fv) & !is.na (dv)
	cent <- sum (select)
	f.zero <- sum (select & fv == 0)

	select <- select & fv > 0
	dv <- dv[select]
	fv <- fv[select]
	if (max (vlims) < max (fv) )
		warning("The 'fv' max is higher than the 'vlims' max. All values won't be taken into account.")

	fun.tmp <- function (vlim, dv, fv, breaks) {
		res <- compute.rose (fv, dv, fv, breaks,
			      function(fv, vlim) sum (fv <= vlim),
			      vlim=vlim)
		res$vlim <- vlim
		return (res)
	}

	roses <- lapply (vlims, fun.tmp, dv, fv, breaks)
	if (null.is.zero) {
		roses[[length(roses)+1]] <- list()
		roses[[length(roses)]]$breaks <- roses[[1]]$breaks
		roses[[length(roses)]]$values <- rep (0, length (roses[[1]]$values) )
		roses[[length(roses)]]$vlim <- 0
		roses <- roses[order (sapply(roses, '[[', 'vlim') )]
	} else {
		roses[[1]]$values[TRUE] <- 0#sum (roses[[1]]$values, na.rm=TRUE)/length (roses[[1]]$values)/cent*100
	}
	for (i in 1:length (roses))
		roses[[i]]$values <- (roses[[i]]$values + f.zero/length (roses[[1]]$values))/cent*100
	return (roses)
}


#' Trace des roses des vents
#'
#' Cette fonction ne réalise aucun calcul à proprement parler, 
#' elle sert uniquement à représenter graphiquement des roses 
#' telles que calculées par la fonction \code{\link{compute.rosevent}}.
#'
#' Lorsque la rose doit être d'abord calculée, il est possible d'utiliser
#' d'abord la fonction \code{\link{compute.rosevent}} ou directement
#' \code{\link{rosevent}}.
#'
#' Cette fonction a été réalisée de manière à pouvoir tracer une rose
#' sur un graphique \sQuote{vierge} ou bien à l'ajouter sur un graphique
#' déjà existant. Pour ajouter la rose à un graphique existant, il suffit
#' de mettre l'argument \code{add} à \code{TRUE}.
#'
#' L'insertion de la rose sur la graphique peut être ajustée à l'aide des 
#' paramètres \code{centre} et \code{facteur}.
#'
#' @param x liste contenant la structure \sQuote{rose} à représenter. Cette
#'	structure doit être similaire à ce qui est retourné par la fonction
#'	\code{\link{compute.rosevent}}.
#' @param y argument conservé uniquement pour que la fonction reste cohérente
#'	avec la fonction générique \sQuote{plot}.
#' @param col couleurs de remplissage de la rose. Une valeur est attendue
#' 	pour chaque classe de vitesses.
#' @param border couleurs de la bordure de la rose. Une couleur peut être
#' 	donnée pour chaque classe de vitesses (mais pas nécessairement).
#' @param density the density of shading lines, in lines per inch.  The default
#' 	value of ‘NULL’ means that no shading lines are drawn.  A
#' 	zero value of ‘density’ means no shading nor filling whereas
#'	negative values (and ‘NA’) suppress shading (and so allow
#' 	color filling). Une valeur par classe de vitesses, ou pas.
#'
#' @param rlim limites du graphiques. contient 2 valeurs, la première est la valeur
#' 	correspondant la concentration au centre de la rose ; la seconde à celle 
#' 	au bord extérieur de la rose.
#' @param centre lorsque la rose est ajoutée à un graphique, donne les coordonnées
#'	dans le graphique pré-existant du centre de la rose. Liste avec deux élements
#'	nommés \code{x} et \code{y}.
#' @param pas.rose valeur indiquant la précision de la rose (1 -> un point tous les
#' 	degrés, 0.5 -> 2 points par degré, etc).
#' @param facteur valeur numérique indiquant le grossissement à appliquer à la rose.
#'	Utile uniquement lorsque la rose est ajoutée à un graphique pré-existant pour 
#' 	régler la taille de la rose.
#' @param add logical indiquant si la rose doit être ajoutée à un graphique pré-existant.
#' @param \dots arguments supplémentaires transmis à la fonction \code{\link{plot.rose}}.
#'
#' @seealso \code{\link{compute.rose}}, \code{\link{rosepol}}, \code{\link{plot.rosevent}}, \code{\link{rosevent}} 
plot.rosevent <- function (x, y, col, border, density=-1, rlim=NULL,
			   centre=list (x=0, y=0), pas.rose=1, facteur=1, add=FALSE, ...) {
	rose <- x

	if (missing (col) )
		col <- sample (colors(), length (rose) ) else
		col <- rep (col, length(rose))[1:length(rose)]
	if (is.null(rlim) )
		rlim <- compute.lim (unlist (lapply (rose, '[[', 'values') ), na.rm=TRUE)
	density <- rep (density, length(rose))[1:length(rose)]
	if (missing (border) )
		border <- par()$col
	border <- rep (border, length(rose))[1:length(rose)]

	for (j in 1:length (rose) ) {
		if (j == 1) {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, at.rho=NA, at.theta=NA, add=add)
		} else if (j == length (rose) ) {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, add=TRUE, ...)
		} else {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, at.rho=NA, at.theta=NA, add=TRUE)
		}
	}
}

#'  Calcul et affichage de roses des vents
#'
#' Cette fonction est un raccourci pour utiliser simultanément les fonctions
#' \code{\link{compute.rosevent}} puis \code{\link{plot.rosevent}}.
#'
#' @inheritParams compute.rosevent
#' @inheritParams plot.rosevent
#' @param \dots arguments supplémentaires pour la fonction \code{\link{plot.rosevent}}.
#'
#' @return une liste invisible de listes structurées de manière identique à ce qui
#' 	est obtenu en utilisant la fonction \code{\link{compute.rose}} ainsi
#' 	qu'un élément supplémentaire : \code{vlim}.
#' 	Chaque élément de la liste contient les éléments suivants :
#' \item{breaks}{un vecteur \sQuote{numeric} contenant les limites des secteurs de vents.
#'		Voir la section\sQuote{Details} pour plus de précisions.}
#' \item{values}{Vecteur \sQuote{numeric} de longueur égale au nombre de secteurs de vent
#'		 définis.}
#' \item{opened}{Un facteur indiquant si les secteurs de vent sont ouverts à gauche ou à droite
#'		(pour l'instant, seuls les secteurs ouverts à gauche sont pris en compte).}
#' \item{vlim}{Valeur numérique correspondant à la vitesse de vent à laquelle correspond
#'		l'élément en question (\code{0} correspond aux vents nuls et calmes).}
#'
#' @seealso \code{\link{compute.rosevent}}, \code{\link{plot.rosevent}} 
rosevent <- function (dv, fv, breaks=0:17*22.5+22.5/2, vlims,
		      col, border, density=-1, rlim=NULL,
			   centre=list (x=0, y=0), pas.rose=1, facteur=1, add=FALSE, ...) {
	rose <- compute.rosevent(dv, fv, breaks, vlims)
	plot.rosevent(x=rose, col=col, border=border, density=density, rlim=rlim, centre=centre, pas.rose=pas.rose, facteur=facteur, add=add, ...)
invisible(rose)

}
