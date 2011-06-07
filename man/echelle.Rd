\name{echelle}
\alias{echelle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Affecter une couleur selon une echelle de valeur. }
\description{
  Permet d'attribuer a des valeurs numeriques les couleurs associees dans une echelle de couleurs.
}
\usage{
echelle (
  donnees,
  mini = 0, maxi = ceiling(max(donnees, na.rm = TRUE)), nclass = 101,
  inverted = FALSE, col = arcenciel(nclass, inverted))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{ Vecteur de numeric contenant les valeurs auxquelles on souhaite asoocier les couleurs. }
  \item{mini}{ Valeur minimale de l'echelle de couleur. Par defaut = 0. }
  \item{maxi}{ Valeur maximale de l'echelle de couleur. Par defaut = valeur maxi de 'donnees'. }
  \item{nclass}{ nombre de couleurs differentes dans l'echelle de couleurs }
  \item{inverted}{ Les couleurs de l'echelle doivent-elles etre inversees ? }
  \item{col}{ Vecteur de couleurs parmi lesquelles les couleurs de l'echelle vont etre selectionnees. }
}
\value{
  Retourne un vecteur de couleurs de meme taille que donnees.
}
\seealso{ \code{\link{froid}}, \code{\link{chaud}}, \code{\link{colors}} }
\examples{
  plot(0:101, y <- c(0:50, 50:0), type="h", col=echelle(y, 0, 50, 51), lwd=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
\keyword{ manip }
