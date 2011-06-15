\name{rosevent}
\alias{rosevent}
\alias{rosevent.Qair}
\alias{calcRoseVent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Affichage de roses de vent }
\description{
  Permet d'afficher des roses de vent à partir d'objet 'Qair'
}
\usage{
rosevent (
  donnees, formula,
  breaksVent, vlim,
  leg=locator(1), col, \dots)
\method{rosevent}{Qair} (
  donnees, formula,
  breaksVent, vlim,
  leg=locator(1), col, cex.leg=0.7, \dots)

calcRoseVent(vv, vlim, tot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{ objet de type 'Qair' contenant les donnees a representer, ainsi que des donnees de  directions et de vitesses de vent. }
  \item{formula}{ de la forme ~dv+vv avec dv le nom de la colonne des directions de vent et vv celui de la colonne des vitesses de vent.
  (cf. \code{\link{calcRose}}) }
  \item{breaksVent}{ un vecteur d'angles (en degre) correspondant aux separations entre les secteurs souhaites sur la rose de pollution.
  Les valeurs entrees sont automatiquement ramenees entre 0 et 360 degre. (cf. \code{\link{calcRose}}) }
  \item{vlim}{ vecteur contenant des numerics. Definit les classes de vitesses de vents pour la rose de vent.
        la valeur prise par defaut des classes est calculee a partir des centiles 25, 50, 75 et 100. (donc 4 classes de vitesses de vent.) }
  \item{leg}{ liste contenant un element 'x' et un 'element 'y' specifiant les coordonnees ou doit apparaitre la legende.
        Par defaut, la fonction demande a l'utilisateur de 'cliquer cette position'. Si cette valeur est NULL, la legende n'est pas affichee. }
  \item{col}{ Vecteur de couleurs servant a remplir les secteurs. Si plusieurs roses sont superposees (cas des roses de quantiles),
  ce vecteur sera recycle si sa taille et le nombre de roses ne correspondent pas. }
  \item{cex.leg}{ numeric indiquant la taille des caractres dans la legende. (character expansion) }
  \item{\dots}{ Permet de preciser des arugments specifiques a la fonction 'plot.rose'. (\code{\link{plot.rose}}) }
}
\details{
  Cette fonction s'appuie sur la classe 'rose', donc des donnees reparties selon les directions de vent, pour representer des roses de vent.

  La fonction calcRoseVent est utilisee en interne pour le calcul de la rose des vents.
}
\seealso{ \code{\link{calcRose}}, \code{\link{plot}}, \code{\link{plot.rose}} }
\examples{
  data(larochelle)
  rosevent(larochelle, ~DV_ATM+VV_ATM, leg=NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ dplot }
