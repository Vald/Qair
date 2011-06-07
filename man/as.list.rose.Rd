\name{as.list.rose}
\alias{as.list.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transformation d'un objet 'rose' en objet 'list' }
\description{
    Permet d'obtenir une list a partir d'un objet de la classe rose  
}
\usage{
\method{as.list}{rose} (x, \dots )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{  un objet de la classe 'rose' a convertir en list.  }
  \item{\dots}{  Arguments to pass to or from other methods  }
}
\value{
  La fonction renvoie un element de type 'list' dont les sous-elements sont du type 'Qair'.
}
\seealso{  \code{\link{calcRose}}  }
\examples{
  data(larochelle)
  rose <- calcRose(larochelle, ~DV_ATM+VV_ATM)
  summary(as.list(rose))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
