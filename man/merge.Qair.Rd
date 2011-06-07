\name{merge.Qair}
\alias{merge.Qair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Merging 2 'Qair' objects }
\description{
  Permet de concatener deux objets de type 'Qair' dont le sous-type est identique.
}
\usage{
\method{merge}{Qair}(x, y, by = "date", all = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Un objet de type 'Qair' }
  \item{y}{ Un objet de type 'Qair' }
  \item{by}{ Noms des colonnes, sous forme de vecteur de 'character', presentent dans les 2 objets et sur lesquelles il faut realiser la fusion. (cf. details) }
  \item{all}{ logical, precise si lorsqu'une date apparait dans un seul des 2 objets, elle doit etre conservee ou non. (T : elle est conservee, F : elle est supprimee) }
  \item{\dots}{ Autres arguments a passer a la fonction \code{\link{merge}}. }
}
\details{
  Les 2 objets 'Qair' a fusionner doivent necessairement avoir le meme sous-type, sans quoi une erreur est generee. (sous-type 'heure', 'jour', 'mois', etc.)
  Seule la colonne 'date' est necessaire pour la realisation de la fusion. Neanmoins, lorsque l'utilisateur souhaite fusionner 2 'Qair' couvrant des periodes distinctes pour un meme polluant, il est possible de le rajouter la nom du polluant dans l'argument 'by'.
  Pour de plus amples informations sur le fonctionnement de la fonction sous-jacente : \code{\link{merge}}
  Bien entendu, les attributes de x et y sont egalements fusionnies, si besoin.
}
\value{
  Renvoie un objet 'Qair' regroupant les informations contenues dans x et y et de meme sous-type que x et y.
}
\seealso{ \code{\link{merge}},\code{\link{Xair2R}},\code{\link{[.Qair}} }
\examples{
  data(larochelle)

  # les donnees d'ozone sont separees des donnees de NOx
  nox <- larochelle[grep("N", names(larochelle))]
  ozone <- larochelle[grep("O3", names(larochelle))]

  # les donnees du mois de janvier sont conservees pour les nox,
  # et celles du mois de juin pour l'ozone
  nox <- nox["07-01-01", "07-02-01"]
  ozone <- ozone["07-06-01", "07-07-01"]

  # affichage des donnees
  par(mfrow=c(1, 2))
  plot(nox, ask.leg=FALSE);plot(ozone, ask.leg=FALSE)
  dev.off()

  # les donnees sont collees ensemble et affichees
  plot(merge(nox, ozone), ask.leg=FALSE)

  # 1
  merge(toJour(nox), toJour(ozone))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
