#' Calcul de roses de concentrations
#'
#' Cette fonction permet de calculer des "roses de concentrations" (moyennes, maximales, etc.).
#' Elle permet
#' de répartir les valeurs prises par une variable quelconque
#' en fonction des directions de vents concomitantes (en tenant
#' compte des vents nuls) et d'appliquer une fonction d'agrégation
#' à chaque groupe ainsi obtenu.
#'
#' Le résultat de cette fonction peut être directement utilsé par la 
#' fonction \code{\link{plot.rose}} pour afficher graphiquement 
#' la rose des concentrations. La fonction \code{\link{rosepol}}
#' est un raccourci pour l'emploi successif de ces deux fonctions.
#'
#' Les trois vecteurs \sQuote{values}, \sQuote{dv}, \sQuote{fv} doivent avoir le 
#' même nombre d'éléments.
#'
#' La fonction est construite de telle façon que l'ensemble des directions de vents
#' est pris en compte. Autrement dit, si \code{breaks <- c(0, 90)}, deux secteurs 
#' de vents sont définis : ]0 - 90] et ]90 - 360]
#' (la taille des deux secteurs n'est donc pas identique). D'autre part, 
#' deux secteurs de vents ne peuvent pas se chevaucher : si \code{breaks <- c(0, 40, 20)},
#' il sera modifié en \code{c(0, 20, 40)}.
#'
#' Les secteurs de vent indiqués par \sQuote{breaks} sont considérés comme ouverts
#' à gauche. Autrement dit, dans l'exemple précédent, une valeur correspondant à un
#' vent de direction 0° sera rangée dans le secteur ]90 - 360] ;
#' et une valeur correspondant à un vent de direction 1° sera rangée dans le 
#' secteur ]0 - 90].
#'
#' Le vecteur \sQuote{breaks} de la liste de retour diffère de celui entré en argument
#' si ce dernier possédait des doublons, n'était pas ordonné ou contenait des
#' valeurs en-dehors du domaine [0-360].
#'
#' Le vecteur \sQuote{breaks} à une longueur égale au nombre de secteur plus un.
#' En effet, sa première valeur et sa dernière valeur sont égales à un modulo 360
#' près. Ce format permet d'accéder plus simplement aux extrémités de chaque secteur.
#' Ainsi, le secteur \sQuote{i} est délimité par ]breaks[i] - breaks[i+1]].
#' La première valeur de \sQuote{breaks} sera donc systématiquement nulle ou négative.
#' 
#' @param values vecteur contenant les valeurs de la variable à agréger
#'	en fonction du vent.
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
#' @param FUN fonction à utiliser pour l'agrégation par secteur de vent (sous forme
#'	de \sQuote{character} ou directement la fonction).
#' @param \dots arguments supplémentaires pour la fonction FUN.
#'
#' @examples
#' compute.rose (sample (10, 50, TRUE),
#'		 sample (360, 50, TRUE),
#'		 sample (0:9, 50, TRUE, c(1, rep(10, 9))))
#'
#' @return une liste
#' \item{breaks}{un vecteur \sQuote{numeric} contenant les limites des secteurs de vents.
#'		Voir la section\sQuote{Details} pour plus de précisions.}
#' \item{values}{Vecteur \sQuote{numeric} de longueur égale au nombre de secteurs de vent
#'		 définis.}
#' \item{opened}{Un facteur indiquant si les secteurs de vent sont ouverts à gauche ou à droite
#'		(pour l'instant, seuls les secteurs ouverts à gauche sont pris en compte).}
#'
#' @seealso \code{\link{plot.rose}}, \code{\link{rosepol}}, \code{\link{compute.rosevent}}, \code{\link{rosevent}} 
compute.rose <- function(values, dv, fv, breaks=0:17*22.5+22.5/2, FUN, ...) {
	dots <- list (...)
	if (missing (FUN) ) {
		FUN <- mean
		if (length (dots) == 0)
			dots$na.rm=TRUE
	}
	FUN <- match.fun(FUN)
	# les trois vecteurs values, dv et fv doivent avoir la meme longeur
	if(length(values) != length(dv) || length(values) != length(fv)) stop('values, dv and fv should have the same length.')
	
	# coherence de breaks
	while(any(breaks < 0))
		breaks[breaks < 0] <- breaks[breaks < 0] + 360
	while(any(breaks > 360))
		breaks[breaks > 360] <- breaks[breaks > 360] - 360
	breaks <- sort (unique (breaks%%360) )
	if (min (breaks) == 0)
		breaks <- c(breaks, 360) else
		breaks <- c(max(breaks)-360, breaks)

	# repartition des mesures dans les intervalles
	dv <- dv %% 360
	temp <- findInterval(dv, breaks)%%length(unique(breaks%%360))
	temp <- ifelse(temp != 0, temp, length(unique(breaks%%360)))

	at.rose <- rep(NA, length(unique(breaks%%360)))
	
	dots$X <- ifelse(!is.na(fv) & fv > 0, values, NA)
	dots$INDEX <- temp
	dots$FUN <- FUN
	
	at.rose[sort(unique(temp))] <- do.call (tapply, dots)
	at.rose[is.na(at.rose)] <- 0

	return(list(breaks = breaks, values=at.rose, opened=factor(c('left', 'right'), ordered=FALSE)[2]))
}


#' Trace des roses de concentrations (moyennes, maximales, etc.)
#'
#' Cette fonction ne réalise aucun calcul à proprement parler, 
#' elle sert uniquement à représenter graphiquement des roses 
#' telles que calculées par la fonction \code{\link{compute.rose}}.
#'
#' Lorsque la rose doit être d'abord calculée, il est possible d'utiliser
#' d'abord la fonction \code{\link{compute.rose}} ou directement
#' \code{\link{rosepol}}.
#'
#' Cette fonction a été réalisée de manière à pouvoir tracer une rose
#' sur un graphique \sQuote{vierge} ou bien à l'ajouter sur un graphique
#' déjà existant. Pour ajouter la rose à un graphique existant, il suffit
#' de mettre l'argument \code{add} à \code{TRUE}.
#'
#' Afin de contrôler au mieux l'affichage de la rose, les paramètres graphiques
#' peuvent être regroupés en 4 catégories. Ces catégories sont les paramètres liés :\cr
#' \describe{
#' \item{à la rose :}{x, y, col, border, density ;}
#' \item{à l'échelle des concentrations :}{tous les paramètres en *.rho et nb.theta ;}
#' \item{à l'échelle des directions de vent :}{tous les paramètres en *.theta sauf nb.theta ;}
#' \item{au graphique en général :}{rlim, centre, pas.rose, facteur, add.}
#' }
#'
#' \code{x} est une liste de même format que la valeur retournée par \code{\link{compute.rose}}.
#' Éventuellement, l'élément \code{values} de \code{x} peut être une liste (de taille identique
#' au nombre de secteurs) de \code{\link[base]{data.frame}}. Dans ce cas, \code{y} indique
#' le nom ou la place de l'élément à prendre dans chaque élément de cette liste.
#' 
#' Les autres éléments relatifs à la rose (\code{col, border, density}) sont identiques
#' à ceux de la fonction \code{\link[graphics]{polygon}}.
#'
#' Pour les paramètres de l'échelle des concentrations, \code{at.rho, 
#' labels.rho, lwd.rho et lty.rho} s'utilisent de manière identique 
#' à ceux de la fonction \code{\link[graphics]{par}}. \code{expr.rho}
#' permet indique si les étiquettes (\code{labels.rho}) sont des expressions.
#' En effet, il est possible de définir ces étiquettes avec des expressions
#' sur le modèle de la fonction \code{\link[grDevices]{plotmath}}. Ça permet
#' entre autre d'afficher des microg/m3 de manière plus officielle (avec le
#' véritable caractère grec et en mettant des exposants). \code{expr.rho}
#' doit alors juste être mis à \code{TRUE}.
#'
#' Pour les paramètres de l'échelle des vents, \code{at.theta, 
#' labels.theta, col.theta, lwd.theta et lty.theta} s'utilisent de manière identique 
#' à ceux de la fonction \code{\link[graphics]{par}}.
#'
#' Pour les autres paramètres, voir la description détaillée.
#' 
#' @param x liste contenant la structure \sQuote{rose} à représenter. Cette
#'	structure doit être similaire à ce qui est retourné par la fonction
#'	\code{\link{compute.rose}}.
#' @param y voir la section \sQuote{Details}.
#' @param col couleur de remplissage de la rose.
#' @param border couleur de la bordure de la rose.
#' @param density the density of shading lines, in lines per inch.  The default
#' 	value of ‘NULL’ means that no shading lines are drawn.  A
#' 	zero value of ‘density’ means no shading nor filling whereas
#'	negative values (and ‘NA’) suppress shading (and so allow
#' 	color filling).
#'
#' @param at.rho valeurs de rayon auxquelles doivent être affichés des divisions
#'	pour l'échelle des concentrations.
#' @param theta.rho angle en degré où affiché les graduations
#' @param cex.rho character expansion pour la graduation
#' @param unite chaîne de caractères qui sera ajouté à chaque étiquette de l'échelle
#' 	des concentrations.
#' @param labels.rho chaînes de caractères ou expressions (cf \sQuote{Details})
#'	indiquant les étiquettes de l'échelle de concentrations.
#' @param col.rho couleur à appliquer à l'échelle des concentrations (cf \sQuote{Details}).
#' @param lwd.rho épaisseur de l'échelle des concentrations (cf \sQuote{Details}).
#' @param lty.rho type de l'échelle des concentrations (pointillés, continu) (cf \sQuote{Details}).
#' @param expr.rho logical indiquant si labels.rho contient des chaînes de caractères
#'	ou des expressions (cf \sQuote{Details}).
#' @param nb.theta indique (en degré) la précision des cercles de l'échelle des concentrations.
#'	Un pas de 1 signifie qu'un point sera tracé tous les degrés.
#'
#' @param at.theta indique pour quels degrés les étiquettes pour l'échelle
#' 	des directions de vent doivent être affichées.
#' @param labels.theta chaînes de caractères contenant les étiquettes pour l'échelle
#'	des directions de vent.
#' @param col.theta couleur à appliquer à l'échelle des directions de vents (cf \sQuote{Details}).
#' @param lwd.theta épaisseur de l'échelle des directions de vent (cf \sQuote{Details}).
#' @param lty.theta type de l'échelle des concentrations (pointillés, continu) (cf \sQuote{Details}).
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
#' @param \dots pour éventuellement d'autres méthodes.
#'
#' @seealso \code{\link{compute.rose}}, \code{\link{rosepol}}, \code{\link{plot.rosevent}}, \code{\link{rosevent}} 

plot.rose <- function(x, y,
	col, border, density=-1,
	rlim=NULL,
	at.rho = NULL, unite = '', labels.rho=NULL, col.rho, lwd.rho=1, lty.rho=2, expr.rho=FALSE, cex.rho=1, theta.rho=sample(360, 1),
	at.theta = 0:7*45, labels.theta = c('N', 'NE', 'E', 'SE', 'S', 'SO', 'O', 'NO'), nb.theta = 360, col.theta, lwd.theta=1, lty.theta=2,
	centre = list(x=0, y=0), pas.rose = 1, facteur=1, add=FALSE, ...) {

	rose <- x
	breaks <- rose$breaks
	if(missing(y)) at.rose <- rose$values else at.rose <- sapply(rose$values, '[[', y)

	if(is.null(rlim)) {
		rlim <- c(min(c(at.rose, 0), na.rm = TRUE), max(at.rose, na.rm = TRUE))
		fact <- ifelse(log(abs(rlim), 10) >= 1, 10^floor(log(abs(rlim), 10)), 10^ceiling(log(abs(rlim), 10)))
		rlim <- ceiling(abs(rlim/fact)) * fact * sign(rlim)
		rlim <- ifelse(is.na(rlim), 0, rlim)
	}

	if(is.null(at.rho)) at.rho <- seq(min(rlim), max(rlim), length=3)[-1]
	if(is.null(labels.rho)) labels.rho <- paste(at.rho, unite, sep='')

	if(length(labels.theta) != length(at.theta)) {
		warning("Les labels de directions ne correspondent pas en nombre avec leurs emplacements.\nIls sont remplaces par des valeurs par defaut.")
		labels.theta <- paste(round(at.theta), 'deg')
	}

	if(!add) {
		#par(mar=c(0, 0, 0, 0) + 0.1)
		plot(NA, asp=1, axes=FALSE, xlim=c(-1, 1)*abs(diff(rlim))*1.1+centre$x, ylim=c(-1, 1)*abs(diff(rlim))*1.1+centre$y, ann=FALSE)
	}

	if(missing(col)) col <- par()$bg
	if(missing(border)) border <- par()$col
	if(missing(col.rho)) col.rho <- par()$col.axis
	if(missing(col.theta)) col.theta <- par()$col.axis

	#         theta <- mapply(seq, breaks[-(length(breaks)-0:1)], breaks[-c(1, length(breaks))], by=pas.rose, SIMPLIFY=FALSE)
	theta <- mapply(seq, breaks[-length(breaks)], breaks[-1], by=pas.rose, SIMPLIFY=FALSE)
	theta <- lapply(theta, '*', pi/180)
	x <- facteur * unlist(mapply('*', at.rose - min(rlim), lapply(theta, sin), SIMPLIFY=FALSE)) + centre$x
	y <- facteur * unlist(mapply('*', at.rose - min(rlim), lapply(theta, cos), SIMPLIFY=FALSE)) + centre$y
	polygon(x[c(1:length(x), 1)], y[c(1:length(y), 1)], density=density, col=col, border=border)

	if(!all(is.na(at.theta)))
	{
		text(sin(at.theta * pi/180) * abs(diff(rlim)) * 1.1 * facteur + centre$x,
		     cos(at.theta * pi/180) * abs(diff(rlim)) * 1.1 * facteur + centre$y,
		     labels.theta, col=col.theta)
		segments(x0=centre$x, y0=centre$y, x1=sin(at.theta * pi/180) * abs(diff(rlim)) * facteur + centre$x, y1=cos(at.theta * pi/180) * abs(diff(rlim)) * facteur + centre$y, col=col.theta, lty=lty.theta, lwd=lwd.theta)
	}

	if(!all(is.na(at.rho)))
	{
		theta <- seq(0, 360, length=nb.theta)
		mapply(polygon,
		       as.data.frame(outer(cos(theta * pi/180), at.rho - min(rlim), '*') * facteur +centre$x),
		       as.data.frame(outer(sin(theta * pi/180), at.rho - min(rlim), '*') * facteur +centre$y),
		       MoreArgs=list(lty=lty.rho, lwd=lwd.rho, col=col.rho, density=0))
		theta <- theta.rho*pi/180
		text((at.rho - min(rlim))*sin(theta) * facteur + centre$x,
		     (at.rho - min(rlim))*cos(theta) * facteur + centre$y,
		     if(expr.rho) parse(text=labels.rho) else labels.rho,
		     cex=cex.rho, col=col.rho)
	}
}

#'  Calcul et affichage de roses de concentrations
#'
#' Cette fonction est un raccourci pour utiliser simultanément les fonctions
#' \code{\link{compute.rose}} puis \code{\link{plot.rose}}.
#'
#' @inheritParams compute.rose
#' @inheritParams plot.rose
#' @param \dots arguments supplémentaires pour la fonction FUN.
#'
#' @return une liste invisible
#' \item{breaks}{un vecteur \sQuote{numeric} contenant les limites des secteurs de vents.
#'		Voir la section\sQuote{Details} pour plus de précisions.}
#' \item{values}{Vecteur \sQuote{numeric} de longueur égale au nombre de secteurs de vent
#'		 définis.}
#' \item{opened}{Un facteur indiquant si les secteurs de vent sont ouverts à gauche ou à droite
#'		(pour l'instant, seuls les secteurs ouverts à gauche sont pris en compte).}
#'
#' @seealso \code{\link{compute.rose}}, \code{\link{plot.rose}}
rosepol <- function(
	values, dv, fv, breaks=0:17*22.5+22.5/2, FUN = 'mean',
	col, border, density=-1,
	rlim=NULL,
	at.rho = NULL, unite = '', labels.rho=NULL, col.rho, lwd.rho=1, lty.rho=2, expr.rho=FALSE, cex.rho=1, theta.rho=sample(360, 1),
	at.theta = 0:7*45, labels.theta = c('N', 'NE', 'E', 'SE', 'S', 'SO', 'O', 'NO'), nb.theta = 360, col.theta, lwd.theta=1, lty.theta=2,
	centre = list(x=0, y=0), pas.rose = 1, facteur=1, add=FALSE,
	...) {
	rose <- compute.rose(values, dv, fv, breaks, FUN, ...)
	plot.rose(x=rose, col=col, border=border, density=density, rlim=rlim, at.rho=at.rho, unite=unite, labels.rho=labels.rho, at.theta=at.theta, labels.theta=labels.theta, nb.theta=nb.theta, centre=centre, pas.rose=pas.rose, facteur=facteur, add=add, col.rho=col.rho, col.theta=col.theta, expr.rho=expr.rho, lty.theta=lty.theta, lty.rho=lty.rho, cex.rho=cex.rho, theta.rho=theta.rho)
invisible(rose)
}





