\name{plot.rose}
\alias{plot.rose}
\alias{rosepol}
\alias{rosepol.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Affichage de roses de pollution }
\description{
  Permet d'afficher toutes sortes de roses de pollution a partir d'objet de classe 'Qair' et 'rose'
}
\usage{
\method{plot}{rose}(x, \dots, r, rlim,
    add = FALSE, thetaPas = 1, density = NULL,
    col, border, lwd.border = 1, lty.border = 1,
    at.axis, lwd.axis = 1, lty.axis = 2, col.axis = "grey",
    label.axis, alpha.axis = 45, cex.axis = 0.7,
    ray = TRUE, lwd.ray = 0.5 * lwd.border, lty.ray = 1, col.ray = "black",
    typeRose = c("centre", "zero", "boite"), deg=0:11*30, cex.deg = 0.7,
    main)

rosepol(donnees, formula, breaksVent = 0:36 * 10 - 5, FUN, \dots)
\method{rosepol}{Qair}(donnees, formula, breaksVent = 0:36 * 10 - 5, FUN, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Un objet de classe 'rose' dont l'utilisateur veut obtenir la representation. Si cet objet n'est pas aggrege, la fonction \code{\link{mean}} sera utilisee par defaut. }
	\item{\dots}{ non utilise. present pour compatibilite. }
  \item{r}{ Un vecteur de 'character' correspondant aux donnees de rose dont on veut avoir la representation. Par defaut, toutes les donnees de 'rose' seront consecutivement affichees. }
  \item{rlim}{ Un vecteur de 2 elements, ou une liste de vecteurs de 2 elements. Les 2 elements de chaque vecteur indique la valeur au centre du graphique et l'extension maximale de ce dernier. }
  \item{add}{ logical, indique si la representation de la rose des pollutions doit etre ajoutee a un graphique existant. (Par defaut un nouveau graphique est cree.) }
  \item{thetaPas}{ numeric indiquant, en degres, la finesse du trace de la rose de pollution. }
  \item{density}{ Indique la methode de remplissage des secteurs de la rose de pollution. Une valeur NULL ou de -1 induit le remplissage des secteurs, une valeur de 0 implique que seuls les bordes seront traces, et une valeur positive correspondra a un hachurage. (cf. pour plus de details : \code{\link{polygon}}.) }
  \item{col}{ Vecteur de couleurs servant a remplir les secteurs. Si plusieurs roses sont superposees (cas des roses de quantiles), ce vecteur sera recycle si sa taille et le nombre de roses ne correspondent pas. }
  \item{border}{ Vecteur de couleurs servant pour la bordure des secteurs. Noir par defaut, ce vecteur prend la valeur de 'col' dans le cas ou density = 0 }
  \item{lwd.border}{ Epaisseur des bordures des secteurs. (cf. \code{\link{plot.default}}) }
  \item{lty.border}{ Type des bordures. (cf. \code{\link{plot.default}}) }
  \item{at.axis}{ Vecteur de 'numeric' definissant les cercles de graduation. Par defaut, 5 cercles reguliers sont traces. }
  \item{lwd.axis}{ Epaisseur des cercles de graduation. (cf. \code{\link{plot.default}}) }
  \item{lty.axis}{ Type des cercles de gradution. En lignes discontinues par defaut. (cf. \code{\link{plot.default}}) }
  \item{col.axis}{ Couleur des cercles de graduation. "grey" par defaut. }
  \item{label.axis}{ Vecteur de 'character'. Legende sur les cercles de graduation. Par defaut, la graduation avec l'unite correspondant au polluant affiche. (L'unite est recuperee a partir de l'objet 'rose') }
  \item{alpha.axis}{ Vecteur de 'numeric' correspondant aux angles (en degres) auxquels doivent etre affiches les 'label.axis'. }
  \item{cex.axis}{ numeric. taille de la police (character expansion) des label.axis. (cf. \code{\link{plot.default}}) }
  \item{ray}{ logical. Indique si les secteurs doivent etre visuellement separes par des segments. (si density = 0, n'est pas applicable.) }
  \item{lwd.ray}{ Epaisseur des segments de separation. (cf. \code{\link{plot.default}}) }
  \item{lty.ray}{ Type des segments de separation. (cf. \code{\link{plot.default}}) }
  \item{col.ray}{ Couleurs des segments de separation. (cf. \code{\link{plot.default}}) }
  \item{typeRose}{ 'character' specifiant le type de la rose : 'normal' signifie que les secteurs partent du centre du graphique,
        'zero' que les secteurs partent de la graduation 0, 'boite' signifie que des 'cases' sont traces. 
        Ce dernier est principalement utilise pour la representation de quantile. }
  \item{deg}{ vecteur d'entiers indiquant les directions en degres pour lesquels la graduation doit apparaitre }
  \item{cex.deg}{ numeric. taille de la police (character expansion) des directions (0, 90, 180, 270). (cf. \code{\link{plot.default}}) }
  \item{main}{ S'il n'est pas indique, le titre du graphique sera le nom du polluant, s'il est NULL, aucun titre ne sera affiche,   
        sinon sa valeur fera office de titre.  }

  \item{donnees}{ objet de type 'Qair' contenant les donnees a representer, ainsi que des donnees de  directions et de vitesses de vent. }
  \item{formula}{ de la forme ~dv+vv avec dv le nom de la colonne des directions de vent et vv celui de la colonne des vitesses de vent. (cf. \code{\link{calcRose}}) }
  \item{breaksVent}{ un vecteur d'angles (en degre) correspondant aux separations entre les secteurs souhaites sur la rose de pollution. Les valeurs entrees sont automatiquement ramenees entre 0 et 360 degre. (cf. \code{\link{calcRose}}) }
  \item{FUN}{ la fonction a utiliser pour l'aggregation des donnees. Si 'FUN' n'est pas precisee lorsque calcRose est appliquee a un objet 'rose', la fonction 'mean' est appliquee par defaut avec l'argument na.rm=T. (cf. \code{\link{calcRose}}) }
  \item{\dots}{ Permet de preciser des arugments specifiques a la fonction FUN. (cf. \code{\link{calcRose}}), ou a la fonction 'plot'. (pour rosepol et rosevent.) }
}
\details{
  Cette fonction s'appuie sur la classe 'rose', donc des donnees reparties selon les directions de vent, pour representer des roses de pollution.
  Les roses peuvent etre des roses de moyennes par secteur, de quantiles, de min, de max, de nombre de donnees, etc.
}
\seealso{ \code{\link{calcRose}}, \code{\link{plot}} }
\examples{
  data(larochelle)
  rose <- calcRose(larochelle, ~DV_ATM+VV_ATM)
  larochelle$diffO3 <- larochelle$O3_VAU - larochelle$O3_AYT
  attributes(larochelle)$unite[names(larochelle) == "diffO3"] <- "µg/m3"

  # 1{
  par(mfrow=c(2, 2))
  plot (rose["O3_VER"])
  plot (calcRose(larochelle, ~DV_ATM+VV_ATM, FUN="max", na.rm=TRUE)["O3_VER"])
  plot (
    calcRose(larochelle, ~DV_ATM+VV_ATM, FUN="quantile", na.rm=TRUE),
    r="DV_ATM", rlim=c(160, 360), typeRose="boite")
  plot (
    calcRose(larochelle, ~DV_ATM+VV_ATM, FUN="mean", na.rm=TRUE)["diffO3"],
    col="skyblue", typeRose="zero")
  # }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ dplot }
