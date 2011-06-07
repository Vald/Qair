\name{Xair2bilan}
\alias{Xair2bilan}
\title{ calcule des bilans reglementaires pour des donnees stockees dans Xair }
\description{
  Cette fonction est un 'encapsulement' de la fonction 'calcul_seuils' realisee pour fonctionner  
  specifiquement sur des donnees stockees dans une base Xair.
}
\usage{
Xair2bilan (
  dated, datef, seuils,
  parAn = TRUE, fichier = TRUE, interactive.seuils = FALSE,
  ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dated}{ date de debut de la periode souhaitee. Un character au format 'aa-mm-jj' }
  \item{datef}{ date de fin de la periode souhaitee. Un character au format 'aa-mm-jj' }
  \item{seuils}{ data.frame contenant la definition des seuils a appliquer. Par defaut, les valeurs definies
  dans data(seuils.Qair) sont utilisees. (\code{\link{seuils.Qair}}) }
  \item{parAn}{ logical indiquant si la fonction doit separer les donnees par annee avant de faire les calculs. sinon,
  un bilan global sur la periode specifiee est realise. }
  \item{fichier}{ logical indiquant sur le resultat doit etre sauvegarde dans un fichier '.csv'. Peut egalement etre une chaine
  de caracteres specifiant directement le nom du fichier ou enregistrer le resultat. }
  \item{interactive.seuils}{ logical. Si = TRUE, permet de modifier de maniere interactive la definition des seuils. }
  \item{\dots}{ autres arguments a passer a la fonction \code{\link{Xair2R}}. (sauf dated, datef et dt qui est egal a 'heure' obligatoirement) }
}
\details{
  utilise la fonction \code{\link{calcul_seuils}} pour l'appliquer a des donnees Xair et les pre-formater dans un fichier '.csv'.
}
\value{
  si fichier = FALSE, renvoie automatiquement le resultat. (une data.frame si parAn = TRUE, une liste de data.frame si parAn=FALSE)
  renvoie la meme chose si fichier = TRUE (ou s'il s'agit d'un chemin a un fichier), mais de maniere 'invisible. (\code{\link{invisible}})
}
\author{ Vladislav Navel }
\seealso{ \code{\link{calcul_seuils}}, \code{\link{Xair2R}}, \code{\link{seuils.Qair}}, \code{\link{invisible}} }
\examples{
## Not run:
# avant de faire tourner l'exemple, completer le vecteur de polluants avec
# des 'NOM_COURT_MES' de la base Xair (mettre des mesures
# avec différents polluants)
polluants <- 

# calcul avec tous les polluants pour les 3 annees
Xair2bilan("2006-01-01", "2008-12-31", polluants=polluants)

# calcul global sur la periode
Xair2bilan("2006-01-01", "2008-12-31", parAn = FALSE, polluants=polluants)

# calcul global uniquement pour 2008
Xair2bilan("2008-01-01", "2008-12-31", parAn = FALSE, polluants=polluants)

## End(Not run)
}
\keyword{ math }
\keyword{ arith }
\keyword{ manip }
