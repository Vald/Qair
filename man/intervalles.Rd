\name{intervalles}
\alias{intervalles}
\alias{intervalles.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Acces aux intervalles d'une rose }
\description{
  Permet d'acceder aux intervalles definissant les secteurs d'un objet 'rose'.
}
\usage{
intervalles (rose)
\method{intervalles}{rose} (rose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rose}{ Un objet de type 'rose', agggrege ou non. }
}
\value{
  Renvoie une liste de vecteurs. Chacun de ces vecteurs definit les bornes inferieure et superieure d'un secteur de vent.
}
\seealso{ \code{\link{calcRose}}, \code{\link{plot.rose}} }
\examples{
  data(larochelle)
  intervalles(calcRose(larochelle, ~DV_ATM+VV_ATM))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ dplot }
