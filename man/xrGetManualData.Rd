\name{xrGetManualData}
\alias{xrGetManualData}
\title{Fonction pour recuperer des donnees manuelles de XR}
\usage{
  xrGetManualData(conn, start, end, sites = NULL,
    polluants = NULL, methodes = NULL,
    valid.states = c("A", "R", "O", "W", "P"),
    what = c("value", "state", "both"), campagnes = NULL,
    tz = "UTC", cursor = NULL,
    categories = as.character(0:4))
}
\arguments{
  \item{polluants}{chaînes de caractères correspondant aux
  polluants à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetPolluants}}).  si c'est un
  vecteur, est directement utilisé comme pattern pour la
  fonction xrGetPolluants. Si c'est une liste, les éléments
  doivent être nommés. Chaque élément est alors utilisé
  comme argument pour la fonction xrGetPolluants.  pattern
  doit alors est précisé :

  \code{... list(pattern='DF', search.fields='NOPOL') ...}}

  \item{sites}{chaînes de caractères correspondant aux
  sites à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetSitesPrelevement}}).}

  \item{methodes}{chaînes de caractères correspondant aux
  campagnes à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetMethodesPrelevement}}).}

  \item{categories}{vecteur indiquant les categories des
  mesures à rappatrier.  combinaison quelconques des
  valeurs '0', '1', '2', '3' ou '4' avec la correspondance
  suivante (attention ce doit être des chaînes de
  caractères) : \describe{ \item{'0'}{analyse ;}
  \item{'1'}{contrôle ;} \item{'2'}{blanc terrain ;}
  \item{'3'}{blanc laboratoire ;} \item{'4'}{blanc
  transport.} } Par défaut, toutes les catégories de
  mesures sont récupérées.}

  \item{conn}{une connexion valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{start}{date initiale de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{end}{date de fin de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{valid.states}{liste des codes états d'XR à
  considérer comme valide. Par défaut les données dont le
  code état est'A', 'R', 'O', 'W' ou 'P' sont considérées
  comme valides.  les autres sont remplacées par NA.}

  \item{what}{un des éléments suivant ('value', 'state',
  'both'). value : seules les valeurs sont récupérées ;
  state : seuls les codes états sont récupérées ; both :
  les deux sont rappatriés et un suffixe est ajouté au nom
  des séries pour les différencier.}

  \item{campagnes}{chaînes de caractères correspondant aux
  campagnes à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetCampagnes}}).}

  \item{tz}{timezone dans laquelle les données doivent être
  retournées. Si les dates debut et fin sont des chaînes de
  caractères, tz est utilisé pour les définir.}

  \item{cursor}{détermine le format de sortie des données :
  NULL pour un TimeIntervalDataFrame (par défaut) une
  valeur entre 0 et 1 pour un TimeInstantDataFrame (1 pour
  respecter les conventions XR).}
}
\value{
  un objet de classe
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
  contenant les données demandées.
}
\description{
  Fonction pour recuperer des donnees manuelles de XR
}
\seealso{
  \code{\link{xrGetSitesPrelevement}},
  \code{\link{xrGetMethodesPrelevement}},
  \code{\link{xrGetCampagnes}},
  \code{\link{xrGetPolluants}}
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
}

