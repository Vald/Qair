\name{toGoogleEarth}
\alias{toGoogleEarth}
\alias{toGoogleEarth.default}
\alias{toGoogleEarth.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ From Qair to Google Earth }
\description{
  Les fonctions decrites ici permettent, a partir de donnees ponctuelles, ou de type 'Qair' ou 'rose' de creer des fichiers '.kml' directement
  importables dans GoogleEarth.
}
\usage{
toGoogleEarth(x, file, ...)

\method{toGoogleEarth}{default} (
  x, file, ...,
  description = NULL, picto = NULL,
  maxi=NULL,
  cols = colorRampPalette (
    c("purple", "blue", "cyan", "green", "yellow", "red")),
  unite=NULL, coeff=0.5, cote=1/3600/5,
  leg.cex = 1, leg.width=150, leg.bg='white')

\method{toGoogleEarth}{rose} (
  x, file, ...,
  r, coeff = 1, rlim,
  col = hsv(0, 1, 1), transparency = TRUE,
  axis, altitude = 1)

\method{toGoogleEarth}{Qair} (
  x, file, ..., polluant,
  maxi, coeff=0.5, cote=1/3600/5,
  cols = colorRampPalette (
    c("purple", "blue", "cyan", "green", "yellow", "red")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ contient les donnees a representer dans Google Earth. }
  \item{file}{ character contenant le nom du fichier. son extension doit etre '.kml' }
  \item{\dots}{ arguments vers ou en provenance d'autres fonctions }

  \item{description}{ Vecteur de character de meme longueur que le nombre de points a representer. Apparaitra a cote de chacun des points sur la carte. }
  \item{picto}{ chemin vers une image pour remplacer l'image par defaut de GoogleEarth. }

  \item{r}{ Dans le cadre de l'application de la fonction a une 'rose'. character donnant le nom de la colonne de la 'rose' a representer. Pour representer
  plusieurs variables apartenant a une meme rose, il faut utiliser plusieurs la fonction. }
  \item{coeff}{ Les dimensions de la rose ne sont parfois pas adaptees a l'echelle de la carte. coeff est applique aux rayons de la rose pour 
  modifier sa taille. Idem, lorsqu'il s'agit des barres representant les concentrations aux stations. }
  \item{rlim}{ correspond aux limites centrale et peripherique de la rose. (\code{\link{plot.rose}}) }
  \item{col}{ couleur a appliquer a la rose. doit etre imperativement au format '#XXXXXX'. Une couleur du type 'blue' entrainera une erreur. }
  \item{transparency}{ logical. La rose doit-elle etre transparente ou non ? }
  \item{axis}{ les axes de graduations de la rose. }
  \item{altitude}{ La rose est representer de maniere plane. Si elle est trop grande, elle peut 'disparaitre' dans le sol. Il suffit alors 
  d'augmenter l'altitude. De plus, une altitude nulle implique obligatoirement que la rose ne sera pas transparente. }

  \item{polluant}{ nom de la colonne dans l'objet Qair pour laquelle on souhaite realiser la representation dans Google Earth }
  \item{cote}{ taille (en degres decimaux) du cote des barres representant les concentrations. }

  \item{leg.cex}{character expansion pour la légende. Permet de régler
  la taille du texte. (cf \code{\link{par}})}
  \item{leg.width}{permet de régler la largeur de la légende. (pour éviter
  que le texte ne déborde)}
  \item{leg.bg}{Pour régler la couleur du fond de la légende}

  \item{cols}{ une fonction du type de celle renvoyee par colorRampPalette. (cf. \code{\link{colorRamp}}) }
}
\details{
  \code{x} est une liste de 2 ou 3 vecteurs dans le cas d'un appel par defaut. Les noms des vecteurs doivent etre 'longitude' et 'latitude'.
  le troisieme doit etre nomme 'value' et est utilise pour representer des valeurs numeriques dispersees spatialement et non plus des points.
}
\value{
  Ces fonctions creent des fichiers .kml importables dans GoogleEarth.
}
\section{Warning }{L'usage de ces fonctions est recommande avec le package 'proj4'. }

\author{ Vladislav Navel }
\seealso{ \code{\link{ntf2rgf93}}, \code{\link{colorRamp}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }
\keyword{ manip }
