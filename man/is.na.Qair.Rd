\name{is.na.Qair}
\alias{is.na.Qair}
\alias{!.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ is.na et ! appliques a un objet Qair }
\description{
  La fonction permet d'appliquer de maniere specifique la fonction is.na aux objets 'Qair'
}
\usage{
is.na(x)
!(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ un objet de classe 'Qair' }
}
\details{
  Ces deux fonctions permettent d'appliquer les fonctions par defaut a un objet Qair. La colonne 'date' n'est pas affectee, et reste sous forme de dates.
}
\seealso{ \code{\link{NA}}, \code{\link{Logic}} }
\examples{
  data(larochelle)
  head(is.na(larochelle))
  colMeans(!is.na(larochelle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ NA }
