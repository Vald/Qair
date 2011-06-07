\name{quantile.a}
\alias{quantile.a}
\alias{round.a}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calcule de quantiles (percentiles) et d'arrondis  }
\description{
  la fonction permet de calculer des quantiles et des arrondis avec la definition ADEME 
}
\usage{
quantile.a(x, probs, pc = 0.75)

round.a(x, digits=0)
}
\arguments{
  \item{x}{ vecteur de numeric pour lesquels le quantile doit etre calcule}
  \item{probs}{ valeur du quantile}
  \item{pc}{ pourcentage minimal de donnees valide pour realiser le calculer (par defaut : 0.75)}
  \item{digits}{ precision a laquelle l arrondi doit etre realise}
}
\details{
  Les percentiles sont calcules a partir de valeurs effectivement mesurees et non de valeurs interpolees ou extrapolees.Toutes  
  les valeurs mesurees sont portees dans une liste etablie par ordre croissant pour chaque site :  
  x1 < x2 < x3 < ...< xk < ...< xn - 1< xn. Le percentile Z [P(Z)] est l'element de rang k, pour lequel k est  
  calcule suivant la formule :  
  k = Z * n / 100 et P(Z) = xk  

  l'arrondi est realise au plus proche entier. Si le nombre a arrondir est equidistant des deux entiers les plus proches,  
  l'arrondi est fait a l'entier superieur.  
  note : la regle est adaptee dans le cas d'arrondis a un ou plusieurs chiffres apres la virgule.  
}
\value{
  quantile.a renvoie le quantile demande, ou s'il n'y a pas suffisamment de donnees valides, NA.

  round.a renvoie un vecteur de numeric de meme longueur que le vecteur original, avec les valeurs arrondies.
}
\author{ Vladislav Navel }
\examples{
  round.a(1.5)  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
