\name{ntf2rgf93}
\alias{ntf2rgf93}
\alias{geographic2cartesien}
\alias{cartesien2geographic}
\alias{geographic2cartesien}
\title{ Groupes de fonctions permettant de travailler avec des coordonnees geographiques (notamment avec les systemes francais NTF et RGF93) }
\description{
  Les fonctions geographic2cartesien et cartesien2geographic permettent de passer des coordonnees geographiques a cartesiennes sur une ellipse donnee.
  La fonction ntf2rgf93 permet de passer d'un systeme geographique a l'autre.
}
\usage{
ntf2rgf93(longitudes, latitudes, inv = FALSE)

geographic2cartesien(longitude, latitude, ellps, altitude = 0)

cartesien2geographic(coords, ellps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{longitudes}{ vecteur de numeric contenant les longitudes a convertir en degres decimaux. }
  \item{latitudes}{ vecteur de numeric contenant les latitudes a convertir en degres decimaux. }
  \item{inv}{ Booleen. Si sa valeur est FALSE (defaut), convertit de NTF en RGF93. Si sa valeur est TRUE, convertit de RGF93 en NTF. }

  \item{longitude}{ vecteur de numeric contenant les longitudes a convertir en degres decimaux. }
  \item{latitude}{ vecteur de numeric contenant les latitudes a convertir en degres decimaux. }
  \item{altitude}{ vecteur de numeric contenant les altitudes a convertir en metres. Ce vecteur est optionnel. }
  \item{ellps}{ Peut prendre les valeurs 'clrk80' ou 'grs80'. La premiere ellipsoide est celle utilisee en NTF, la seconde en RGF93. }

  \item{coords}{ Liste contenant trois vecteurs nommes 'x', 'y' et 'z' contenant les coordonnees a convertir. }
}
\value{
  la fonction ntf2rgf93 retourne une data.frame avec des coordonnees geographiques dans le systeme choisi.
  la fonction cartesien2geographic retourne une data.frame avec des coordonnees geographiques. 
  la fonction geographic2cartesien retourne une data.frame avec des coordonnees cartesiennes. 
}
\references{ 
  les formules de conversions ont etablies a partir de la documentation disponible sur le site de l'IGN. (\url{http://professionnels.ign.fr/14/la-gamme/ficheProduitCMS.do?idDoc=5352513})  
 }
\author{ Vladislav Navel }

\section{Warning}{L'usage de ces fonctions est recommande avec le package 'proj4'. }

\examples{
ntf2rgf93(2.42567186, 48.84451225)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }
\keyword{ manip }

