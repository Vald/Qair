\name{toPAn}

\alias{toPJour}
\alias{toPJour.qh}
\alias{toPJour.heure}
\alias{toPJour.Qair}

\alias{toPHebdo}
\alias{toPHebdo.qh}
\alias{toPHebdo.heure}
\alias{toPHebdo.jour}
\alias{toPHebdo.Qair}

\alias{toPMois}
\alias{toPMois.qh}
\alias{toPMois.heure}
\alias{toPMois.jour}
\alias{toPMois.Qair}

\alias{toPAn}
\alias{toPAn.qh}
\alias{toPAn.heure}
\alias{toPAn.jour}
\alias{toPAn.mois}
\alias{toPAn.Qair}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculer des profils moyens (ou autre) journaliers, mensuels, annuels }
\description{
  Permet de calculer des profils de comportements sur une periode (jour, semaine, mois, annee) a partir de donnees 'Qair'. 
}
\usage{
toPJour(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPJour}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPJour}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPJour}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", heures, ...)

toPHebdo(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPHebdo}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPHebdo}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPHebdo}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPHebdo}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", jours, ...)

toPMois(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPMois}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPMois}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPMois}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPMois}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", jours, ...)

toPAn(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPAn}{qh}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPAn}{heure}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPAn}{jour}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPAn}{mois}(donnees, pc = 0.75, prec = NA, FUN = "mean", ...)
\method{toPAn}{Qair}(donnees, pc = 0.75, prec = NA, FUN = "mean", mois, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{donnees}{ Objet de typ 'Qair' sur lequel on veut calculer un profil. }
  \item{pc}{ valeur comprise entre 0 et 1, indiquant le pourcentage de representativite pour qu'une valeur soit conservee. }
  \item{prec}{ numeric ou vecteur de numeric indiquant la precision souhaitee du resultat. Les regles des arrondis applicables dans la qualite de l'air sont utilisees ici. (1 -> un chiffre apres la virgule, 2 -> deux chiffres, etc.) }
  \item{FUN}{ La fonction a appliquer. Par defaut les moyennes sont calculees, mais il est possible de calculer d'autres indicateurs. (valeur max. chaque sur une semaine, etc.) }
  \item{\dots}{ Arguments supplementaires necessaires au bon fonctionnement de la fonction FUN. Dans le cas d'un appel a la fonction \code{\link{mean}}, l'argument na.rm est mis a TRUE. }

  \item{heures}{ Parametre correspondant aux heures de la journee. Argument a ne pas utiliser puisque dans une fonction interne. }
  \item{jours}{ Parametre correspondant aux jours du mois. Argument a ne pas utiliser puisque dans une fonction interne. }
  \item{mois}{ Parametre correspondant aux mois de l'annee. Argument a ne pas utiliser puisque dans une fonction interne. }
}
\details{
  Les profils journaliers obtenus seront toujours sur une base horaire, meme si les donnees d'entree sont en quart-horaire.
  Les profils hebdomadaires seront toujours sur une base journaliere, meme si les donnees d'entree sont en quart-horaire ou horaire. 
  Les profils mensuels obtenus seront toujours sur une base journaliere, meme si les donnees d'entree sont en quart-horaire ou horaire. 
 Les profils annuels obtenus seront toujours sur une base mensuelle, meme si les donnees d'entree sont en quart-horaire, horaire ou journalier. 
}
\value{
  Un objet de la classe 'Qair', dont la sous-classe sera 'pjour', 'pmois', 'phebdo' ou 'pan' selon les cas.
}
\seealso{ \code{\link{toAn}} }
\examples{
  data(larochelle)
  # toPJour1
  toPJour(larochelle)
  # toPJour2
  toPJour(toHeure(larochelle))

  # toPHebdo1
  toPHebdo(larochelle)
  # toPHebdo2
  toPHebdo(toHeure(larochelle))
  # toPHebdo3
  toPHebdo(toJour(larochelle))

  # toPMois1
  toPMois(larochelle)
  # toPMois2
  toPMois(toHeure(larochelle))
  # toPMois3
  toPMois(toJour(larochelle))

  # toPAn1
  toPAn(larochelle)
  # toPAn2
  toPAn(toHeure(larochelle))
  # toPAn3
  toPAn(toJour(larochelle))
  # toPAn4
  toPAn(toMois(larochelle))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
