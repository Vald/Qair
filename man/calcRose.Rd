\name{calcRose}
\alias{calcRose}
\alias{calcRose.Qair}
\alias{calcRose.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Calcul sur les donnees pour obtenir des roses de pollution  }
\description{
  Permet de realiser les calculs necessaires a la realisation de roses de pollution.
}
\usage{
calcRose (donnees, ... )
\method{calcRose}{Qair} (donnees, formula, breaksVent = 0:36 * 10 - 5, FUN, ... )
\method{calcRose}{rose} (donnees, FUN, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{ Un element de type 'Qair' contenant une colonne avec des valeurs de vitesses de vent et une colonne avec des valeurs de directions de vent. 
    Ou un element de type 'rose' non encore aggrege. (cf. details).  }
  \item{formula}{ de la forme ~dv+vv avec dv le nom de la colonne des directions de vent et vv celui de la colonne des vitesses de vent. }
  \item{breaksVent}{ un vecteur d'angles (en degre) correspondant aux separations entre les secteurs souhaites sur la rose de pollution. Les valeurs entrees sont automatiquement ramenees entre 0 et 360 degre. }
  \item{FUN}{ la fonction a utiliser pour l'aggregation des donnees. Si 'FUN' n'est pas precisee, la fonction 'mean' est appliquee par defaut avec l'argument na.rm=T. }
  \item{\dots}{ Permet de preciser des arguments specifiques a la fonction FUN. }
}
\details{
  La fonction 'calcRose' appliquee a un objet de type 'rose' (cette utilisation est uniquement accessible par les programmes) ou a un objet de type 'Qair' renvoie un objet de type 'rose' aggrege et directement representable via la fonction \code{\link{plot.rose}}.
}
\value{
  Dans le cas d'un objet 'Qair' renvoie un objet 'rose', aggrege.
  Dans le cas d'un objet 'rose' renvoie un objet 'rose', aggrege. (inaccessible)
}
\seealso{ \code{\link{plot.rose}}, \code{\link{as.list.rose}} }
\examples{
  data(larochelle)

  # 1
  calcRose(larochelle, ~DV_ATM+VV_ATM)
  # 2
  calcRose(larochelle, ~DV_ATM+VV_ATM, FUN="mean", na.rm=TRUE)
  # 3
  as.list(calcRose(larochelle, ~DV_ATM+VV_ATM))

  calcRose (
    larochelle, ~DV_ATM+VV_ATM, breaksVent=c(0, 90, 180, 270),
    FUN="toPJour", na.rm=TRUE)
  # 4
  as.list (calcRose (
    larochelle, ~DV_ATM+VV_ATM, breaksVent=c(0, 90, 180, 270),
    FUN="toPJour", na.rm=TRUE) )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
