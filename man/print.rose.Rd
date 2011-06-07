\name{print.rose}
\alias{print.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Printing 'rose' }
\description{
  Print a 'rose'.
}
\usage{
\method{print}{rose}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ un objet de classe 'rose' dont on veut afficher les caracteristiques principale. }
  \item{\dots}{ Arguments to be passed to or from other methods. }
}
\note{ 
  Si l'utilisateur souhaite acceder au contenu reel de la 'rose', il suffit d'utiliser la comande as.list(rose).
}
\seealso{ \code{\link{print}}, \code{\link{as.list.rose}} }
\examples{
  data(larochelle)
  print(calcRose(larochelle, ~DV_ATM+VV_ATM))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
\keyword{ manip }
