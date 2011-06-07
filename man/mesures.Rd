% --- Source file: mesures.Rd ---
\name{mesures}
\alias{mesures}
\alias{mesures.Qair}
\alias{mesures.rose}
\alias{stations}
\alias{stations.Qair}
\alias{stations.rose}
\alias{unites}
\alias{unites.Qair}
\alias{unites.rose}
\alias{latitudes}
\alias{latitudes.Qair}
\alias{latitudes.rose}
\alias{longitudes}
\alias{longitudes.Qair}
\alias{longitudes.rose}
\alias{lambertx}
\alias{lambertx.Qair}
\alias{lambertx.rose}
\alias{lamberty}
\alias{lamberty.Qair}
\alias{lamberty.rose}
\alias{names.Qair}
\alias{names.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Acces aus attributs d'objets 'Qair' et/ou 'rose'. }
\description{
  Permet d'acceder aux informations relatives aux donnees contenues dans des objets de type 'Qair' et 'rose'. (mesures, unites, stations)
}
\usage{
  mesures(donnees)
  \method{mesures}{Qair}(donnees)
  \method{mesures}{rose}(donnees)

  stations(donnees)
  \method{stations}{Qair}(donnees)
  \method{stations}{rose}(donnees)

  unites(donnees)
  \method{unites}{Qair}(donnees)
  \method{unites}{rose}(donnees)

  latitudes(donnees)
  \method{latitudes}{Qair}(donnees)
  \method{latitudes}{rose}(donnees)

  longitudes(donnees)
  \method{longitudes}{Qair}(donnees)
  \method{longitudes}{rose}(donnees)

  lambertx(donnees)
  \method{lambertx}{Qair}(donnees)
  \method{lambertx}{rose}(donnees)

  lamberty(donnees)
  \method{lamberty}{Qair}(donnees)
  \method{lamberty}{rose}(donnees)

  \method{names}{Qair}(x)
  \method{names}{rose}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Un objet de la classe 'Qair' ou un objet de la classe 'rose'. }
  \item{donnees}{ Un objet de la classe 'Qair' ou un objet de la classe 'rose'. }
}
\value{
  Renvoie un vecteur de 'character' decrivant les mesures/stations/unites/noms correspondant aux donnees contenues dans 'donnees' ou 'rose'.
}
\note{ 
  Un element de classe 'rose' aura les memes mesures/stations/unites/noms que l'element 'Qair' dont il est issu.
}
\seealso{ \code{\link{[.Qair}} }
\examples{
  data(larochelle)
  mesures(larochelle)
  stations(larochelle)
  unites(larochelle)
  names(larochelle)

  rose <- calcRose(larochelle, ~DV_ATM+VV_ATM)
  mesures(rose)
  stations(rose)
  unites(rose)
  names(rose)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ attribute }
