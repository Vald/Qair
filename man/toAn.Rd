% --- Source file: toAn.Rd ---
\name{toAn}
\alias{toHeure}
\alias{toHeure.qh}
\alias{toHeure.Qair}

\alias{toJour}
\alias{toJour.qh}
\alias{toJour.heure}
\alias{toJour.jour}
\alias{toJour.Qair}

\alias{toHebdo}
\alias{toHebdo.qh}
\alias{toHebdo.heure}
\alias{toHebdo.jour}
\alias{toHebdo.Qair}

\alias{toMois}
\alias{toMois.qh}
\alias{toMois.heure}
\alias{toMois.jour}
\alias{toMois.Qair}

\alias{toAn}
\alias{toAn.qh}
\alias{toAn.heure}
\alias{toAn.jour}
\alias{toAn.hebdo}
\alias{toAn.mois}
\alias{toAn.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convertir des donnees 'Qair' d'une base temporelle a une autre. }
\description{
  Ces differentes fonctions permettent de realiser des conversions entre les differents sous-types de la classe 'Qair'. (toujours de la plue petite unite temporelle vers la plus grande)
}
\usage{
toHeure(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHeure}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHeure}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)

toJour(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toJour}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toJour}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toJour}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", nbIndiv, ...)

toHebdo(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHebdo}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHebdo}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHebdo}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toHebdo}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", nbIndiv, ...)

toMois(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toMois}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toMois}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toMois}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toMois}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", nbIndiv, ...)

toAn(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{hebdo}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{mois}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toAn}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", nbIndiv, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{ Objet de typ 'Qair' a convertir. }
  \item{pc}{ valeur comprise entre 0 et 1, indiquant le pourcentage de representativite pour qu'une valeur soit conservee. Par exemple, avec pc=0.75, une moyenne horaire ne sera calculee a partir de valeurs qh que si au moins 3 quarts d'heure sont connus pour l'heure dite. }
  \item{prec}{ numeric ou vecteur de numeric indiquant la precision souhaitee du resultat. Les regles des arrondis applicables dans la qualite de l'air sont utilisees ici. (1 -> un chiffre apres la virgule, 2 -> deux chiffres, etc.) }
  \item{FUN}{ La fonction a appliquer. Par defaut les moyennes sont calculees, mais il est possible de calculer d'autres indicateurs. (valeur horaire max. sur une journee, etc.) }
  \item{\dots}{ Arguments supplementaires necessaires au bon fonctionnement de la fonction FUN. Dans le cas d'un appel a la fonction \code{\link{mean}}, l'argument na.rm est mis a TRUE. }

  \item{nbIndiv}{ Parametre precisant de quel sous-type est le 'Qair' a convertir. (L'utilisateur n'est pas encourage a utiliser les fonctions utilisant cet argument.) }
}
\details{
  Les methodes generales pour la classe 'Qair', ne doivent normalement pas etre utilisees par l'utilisateur, elles servent uniquement dans le cas d'appel aux autres methodes.
}
\value{
  Renvoie un objet de classe 'Qair', dont le sous-type est celui precise par l'appel de la fonction.
}
\seealso{ \code{\link{toPJour}}, \code{\link{toPMois}}, \code{\link{toPAn}} }
\examples{
  data(larochelle)

  # toHeure1
  toHeure(larochelle["07-01-01", "07-01-10"])

  # toJour1
  toJour(larochelle["07-01-01", "07-02-01"])
  # toJour2
  toJour(toHeure(larochelle["07-01-01", "07-02-01"]))

  # toHebdo1
  toHebdo(larochelle)
  # toHebdo2
  toHebdo(toHeure(larochelle))
  # toHebdo3
  toHebdo(toJour(larochelle))

  # toMois1
  toMois(larochelle)
  # toMois2
  toMois(toHeure(larochelle))
  # toMois3
  toMois(toJour(larochelle))

  # toAn1
  toAn(larochelle)
  # toAn2
  toAn(toHeure(larochelle))
  # toAn3
  toAn(toJour(larochelle))
  # toAn4
  toAn(toHebdo(larochelle))
  # toAn5
  toAn(toMois(larochelle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
