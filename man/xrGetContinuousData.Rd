\name{xrGetContinuousData}
\alias{xrGetContinuousData}
\title{Recuperation de mesure continue dans une base XR}
\usage{
  xrGetContinuousData(conn, pattern = NULL, start, end,
    period = c("h", "qh", "d", "m", "y"), validated = TRUE,
    valid.states = c("A", "R", "O", "W", "P"),
    what = c("value", "state", "both"), search.fields,
    campagnes = NULL, reseaux = NULL, stations = NULL,
    polluants = NULL, collapse = c("AND", "OR"),
    XR6 = TRUE, tz = "UTC", cursor = NULL, exact = FALSE)
}
\arguments{
  \item{conn}{une connexion valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{pattern}{chaînes de caractères utilisées pour la
  recherche (cf \sQuote{Details}).}

  \item{start}{date initiale de la période à rapatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{end}{date de fin de la période à rapatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{period}{un des éléments suivant ('h', 'qh', 'd',
  'm', 'y') précisant la période des données à rapatrier.}

  \item{validated}{booléen. Si TRUE, les données sont
  récupérées dans la table normale de stockage de XR ; si
  FALSE, les données sont récupérées dans la table
  \code{BRUTE}.}

  \item{valid.states}{liste des codes états d'XR à
  considérer comme valide. Par défaut les données dont le
  code état est'A', 'R', 'O', 'W' ou 'P' sont considérées
  comme valides.  les autres sont remplacées par NA.}

  \item{what}{un des éléments suivant ('value', 'state',
  'both'). value : seules les valeurs sont récupérées ;
  state : seuls les codes états sont récupérées ; both :
  les deux sont rapatriés et un suffixe est ajouté au nom
  des séries pour les différencier.}

  \item{search.fields}{champ de la table dans lesquels
  \code{pattern} doit être recherché.}

  \item{campagnes}{chaînes de caractères correspondant aux
  campagnes à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetCampagnes}}).}

  \item{reseaux}{chaînes de caractères correspondant aux
  réseaux à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetReseaux}}).}

  \item{stations}{chaînes de caractères correspondant aux
  stations à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetStations}}).}

  \item{polluants}{chaînes de caractères correspondant aux
  polluants à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetPolluants}}).}

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}

  \item{XR6}{TRUE si la version de XR est supérieure ou
  égale à 6, FALSE sinon.}

  \item{tz}{timezone dans laquelle les données doivent être
  retournées. Si les dates debut et fin sont des chaînes de
  caractères, tz est utilisé pour les définir.}

  \item{cursor}{détermine le format de sortie des données :
  NULL pour un TimeIntervalDataFrame (par défaut) une
  valeur entre 0 et 1 pour un TimeInstantDataFrame (1 pour
  respecter les conventions XR).}

  \item{exact}{booléen indiquant si les mesures à
  rapatrier doivent être exactement identiques à
  \sQuote{pattern} ou si \sQuote{pattern} doit être utilisé
  dans une expression régulière.}
}
\value{
  un objet de classe
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
  contenant les données demandées.
}
\description{
  La fonction permet de récupérer une ou plusieurs séries
  de données stockée(s) dans une base XR entre 2 dates
  données au format voulu (heure, mois, etc.). Les données
  sont retournées au format
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
  défini dans le paquet \code{\link[timetools]{timetools}}.
}
\details{
  L'extraction des séries de données peut être réalisée en
  recherchant une chaîne de caractères (définie par
  \code{pattern}) sur un ou plusieurs champs de la table
  \code{MESURE} (la fonction \code{\link{xrGetMesures}} est
  alors utilisée). Les champs peuvent être précisés par
  l'argument \code{search.fields}.

  Il est également possible de récupérer plusieurs séries
  de données à partir des réseaux, campagnes, stations ou
  polluants définis dans XR. Il suffit pour cela d'indiquer
  une ou plusieurs chaînes dans les arguments
  \code{campagnes}, \code{reseaux}, \code{stations} ou
  \code{polluants}. Ces arguments sont alors utilisés dans
  les fonctions \code{xrGet*} adéquates, soit
  respectivement \code{\link{xrGetCampagnes}},
  \code{\link{xrGetReseaux}}, \code{\link{xrGetStations}}
  et \code{\link{xrGetPolluants}}.

  Si plusieurs de ces arguments sont renseignés, la
  conjonction entre les différents critères est précisée
  par l'argument \code{collapse}.
}
\seealso{
  \code{\link{xrGetMesures}}, \code{\link{xrGetCampagnes}},
  \code{\link{xrGetReseaux}}, \code{\link{xrGetStations}},
  \code{\link{xrGetPolluants}},
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
  \code{\link{xrGetManualData}}
}

