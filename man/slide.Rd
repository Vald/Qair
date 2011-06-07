\name{slide}
\alias{slide}
\title{ Calculs de moyennes glissantes }
\description{
  Cette fonction permet de faire des moyennes glissantes (ou des variances) sur un vecteur avec  
  comme parametre le pas de calcul, la fenetre de calcul et le pourcentage de valeurs  
  necessaires à chaque pas (par rapport a la fenetre) pour executer le calcul.  
}
\usage{
slide(x, fenetre, pas, pc = 0.75, FUN=c("mean", "min", "max", "var"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vecteur de numeric sur lequel le calcul doit etre execute. }
  \item{fenetre}{ nombre de donnnees sur lesquelles la moyenne doit etre calculee. }
  \item{pas}{ precise le pas auquel doit etre realisee la moyenne. Typiquement, les moyennes horaires glissantes 8h
  seront calculees avec un pas = 1. Les moyennes journalieres (non-glissantes) seront realisees avec un pas de 24. }
  \item{pc}{ pourcentage de donnees valide necessaire pour realiser un calcul. }
  \item{FUN}{ fonction a appliquer (ne fonctionne que pour 'mean', 'min', 'max' et 'var' pour l'instant) }
}
\details{
  La fonction calcule les moyennes ou les variances. Elle devrait evolue afin de pouvoir permettre de calculer des min, max, etc.
}
\value{
  la valeur retournee est un vecteur de taille egale a la taille du vecteur original divise par le 'pas'. (arrondi au superieur)  
}
\author{ Vladislav Navel }
\examples{
  data(larochelle)  
  par(mfrow=c(1, 2))  
  plot(larochelle[grep("O3_", names(larochelle), value=TRUE)], ask.leg=FALSE)  

  journalier <- data.frame(sapply(larochelle, slide, 96, 96))  
  attributes(journalier) [
    setdiff(names(attributes(larochelle)), c("row.names", "names"))
  ] <- attributes(larochelle) [
    setdiff(names(attributes(larochelle)), c("row.names", "names"))]
  attributes(journalier)$unite[1] <- "heure"  
  attributes(journalier)$class[1] <- "heure"  
  plot(journalier[grep("O3_", names(larochelle), value=TRUE)], ask.leg=FALSE)  
}
\keyword{ arith }
\keyword{ math }
