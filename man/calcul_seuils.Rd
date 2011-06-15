\name{calcul_seuils}
\alias{calcul_seuils}
\alias{calcul_seuil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calcul de valeurs reglementaires. }
\description{
  Cette fonction compare une serie de donnee temporelle a des niveaux reglementaires.
}
\usage{
calcul_seuils(date, x, id_poll, seuils, pourDiffusion = FALSE)

calcul_seuil(date, x, seuil)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{ vecteur d'objet 'chron' indiquant la date de chaque 'mesure'. (l'unite temporelle est l'heure) }
  \item{x}{ vecteur de numeric correspondant aux mesures de polluants. (meme taille que 'date') }
  \item{id_poll}{ chaine de caracteres donnant la nature du polluant mesure.
  ("As", "C6H6", "BaP", "Cd", "NO2", "SO2", "CO", "Ni", "Nox", "O3", "PM10", "Pb") }
  \item{seuils}{ data.frame contenant la definition des seuils reglementaires. \code{\link{seuils.Qair}}
  s'il n'est pas specifie, il prend le tableau defini pas defaut dans le package Qair. }
  \item{pourDiffusion}{ indique si la balise 'diffusion' de 'seuils' doit etre prise en compte ou non. }

  \item{seuil}{ data.frame avec une seule ligne correspondant a la definition d'un calcul reglementaire. }
}
\details{
  La fonction 'calcul_seuils' permet de realiser les calculs pour tous les seuils reglementaires definis dans 'seuils'.
  Autant de fois que necessaire, elle fait appelle a la fonction 'calcul_seuil' qui realise ce calcul pour un seul seuil.
}
\value{
  L'appel a la fonction 'calcul_seuil' renvoie un numeric correspondant a la definition du seuil.

  L'appel a la fonction 'calcul_seuils' renvoie un vecteur de numeric correspondant aux seuils definis dans 'seuils' et 
  correspondant au polluant 'id_poll'.
}
\author{ Vladislav Navel }
\seealso{ \code{\link{seuils.Qair}} }
\examples{
data(seuils.Qair)
data(larochelle)
larochelle <- toHeure(larochelle)

date <- larochelle$date
x <- larochelle$O3_VER

calcul_seuils(date, x, "O3", seuils.Qair)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ math }
