\name{as.data.frame.Qair}
\alias{as.data.frame.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transformation d'un objet Qair en objet data.frame }
\description{
  Permet d'obtenir une data.frame a partir d'un objet de la classe Qair  
}
\usage{
  \method{as.data.frame}{Qair} (x, row.names = NULL, optional = FALSE, ... )
}
\arguments{
  \item{x}{  une structure de donnees dont la classe herite de "Qair".  
  Objet renvoye par l'une des fonctions suivantes
  \code{\link{Xair2R}}, \code{\link{toHeure}}, \code{\link{toJour}}, \code{\link{toHebdo}}, \code{\link{toMois}}, \code{\link{toAn}}, \code{\link{toPJour}}, \code{\link{toPHebdo}}, \code{\link{toPMois}}, \code{\link{toPAn}}  }
  \item{row.names}{inutilise. Necessaire pour la compatibilte avec la methode generic}
  \item{optional}{inutilise. Necessaire pour la compatibilte avec la methode generic}
  \item{\dots}{inutilise. Necessaire pour la compatibilte avec la methode generic}
}
\value{
  Cette fonction est essentiellement utile en interne pour l'application des fonctions recurrentes sur la classe Qair.  
  Elle renvoie le tableau de donnees contenu dans "donnees" en supprimant tous les attributs supplementaires caraterisant la classe Qair.  
}

\seealso{  \code{\link{Xair2R}}, \code{\link{toHeure}}, \code{\link{toJour}}, \code{\link{toHebdo}}, \code{\link{toMois}}, \code{\link{toAn}}, \code{\link{toPJour}}, \code{\link{toPHebdo}}, \code{\link{toPMois}}, \code{\link{toPAn}} }
\examples{
  data(larochelle)
  summary(larochelle)
  class(larochelle)
  class(as.data.frame(larochelle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
