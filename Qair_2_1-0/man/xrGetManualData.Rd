\name{xrGetManualData}
\alias{xrGetManualData}
\title{Fonction pour recuperer des donnees manuelles de XR}
\usage{
  xrGetManualData(conn, start, end, sites = NULL,
    polluants = NULL, methodes = NULL,
    valid.states = c("A", "R", "O", "W", "P"),
    what = c("value", "state", "both"), campagnes = NULL)
}
\arguments{
  \item{sites}{chaînes de caractères correspondant aux
  sites à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetSitesPrelevement}}).}

  \item{methodes}{chaînes de caractères correspondant aux
  campagnes à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetMethodesPrelevement}}).}

  \item{conn}{une connection valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{start}{date initiale de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{end}{date de fin de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{polluants}{chaînes de caractères correspondant aux
  polluants à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetPolluants}}).}

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

