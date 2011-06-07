\name{plot.Qair}
\alias{plot.Qair}
\alias{points.Qair}
\alias{lines.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Affiche les donnees d'objet de la classe 'Qair' }
\description{
  La fonction plot definit un format par defaut pour les objets de la classe 'Qair', qu'ils soient des series temporelles ou des profils.
}
\usage{
\method{plot}{Qair} (x, y, xlim, ylim, ...,
  axes=TRUE, x.leg, y.leg, cex.leg=0.7,
  legend=c("abrev", "complet", "none"),
  ask.leg=T, out.format="y-m-d")
\method{points}{Qair}(x, which = names(x), col, ...)
\method{lines}{Qair}(x, which = names(x), col, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Objet de class 'Qair' contenant les donnees a tracer. }
  \item{y}{ vecteur de 'character' correspondant aux  colonnes a afficher. }
  \item{xlim}{ Limites en abscisse du graphique. }
  \item{ylim}{ Limites en ordonnees du graphique. }
  \item{axes}{ logical, les axes doivent-ils etre traces ? }
  \item{x.leg}{ bord gauche de la legende. }
  \item{y.leg}{ bord superieur de la legende }
  \item{cex.leg}{ taille de la police de la legende }
  \item{legend}{ La legende doit-elle prendre les noms courts des colonnes ('abrev'), les designations completes des mesures  ('complet'), ou ne pas etre affichee ('none'). }
  \item{ask.leg}{ logical, l'emplacement de la legende doit-il etre indique par un clique sur le graphe ? }
  \item{out.format}{ chaine de characteres indiquant le format souhaite pour l'affichage des dates sur le graphique. (cf. \code{\link{chron}}) }

  \item{which}{ vecteur de 'character' correspondant aux  colonnes a afficher. }
  \item{col}{ couleurs a affecter aux valeurs devant etre representees. }

  \item{\dots}{ Tout argument pouvant etre transmis aux fonctions \code{\link{plot.default}}, \code{\link{lines.default}} ou \code{\link{points.default}}. }
}
\seealso{ \code{\link{plot.default}}, \code{\link{lines.default}} ou \code{\link{points.default}} }
\examples{
  data(larochelle)

  # 1{
  par(mfcol=c(3, 2))
  plot (larochelle, grep("O3", names(larochelle), value=TRUE), ask.leg=FALSE)
  plot (
    toPJour(larochelle),
    grep("O3", names(larochelle), value=TRUE),
    legend="complet", ask.leg=FALSE)
  plot (
    toPJour(larochelle, FUN="max", na.rm=TRUE),
    grep("O3", names(larochelle), value=TRUE),
    legend="complet", ask.leg=FALSE)
  plot (
    larochelle,
    grep("N", names(larochelle), value=TRUE),
    ask.leg=FALSE)
  plot (
    toPHebdo(larochelle),
    grep("N", names(larochelle), value=TRUE),
    ask.leg=FALSE)
  plot (
    toMois(larochelle),
    grep("N", names(larochelle), value=TRUE),
    ask.leg=FALSE)
  # }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ aplot }
\keyword{ hplot }
