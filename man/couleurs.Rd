\name{couleurs}
\alias{arcenciel}
\alias{chaud}
\alias{froid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Divers fonctions liees aux couleurs }
\description{
 Chaque fonction retourne des details dans un type de ton.
}
\usage{
chaud (n = 100, inverted = FALSE)
froid (n = 100, inverted = FALSE)
arcenciel (n = 101, inverted = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ nombre de couleurs souhaitees. (entre 1 et 100, 101 pour la fonction arcenciel) }
  \item{inverted}{ logical. cf value. }
}
\value{
  Renvoie un vecteur de couleurs. Si inverted == F, les plus pales sont renvoyees au debut, si inverted == T, les plus sombres sont renvoyees au debut.
}
\examples{
  par(mfrow=c(1, 2))
  plot(NA, xlim=c(0, 100), ylim=c(0, 100))
  abline(v=0:100, col=c(froid(50, TRUE), "white", chaud(50)), lwd=5)
  plot(NA, xlim=c(0, 100), ylim=c(0, 100))
  abline(h=0:100, col=arcenciel(101), lwd=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ color }
