\name{Xair2R}
\alias{Xair2R}
\title{Récupération de mesure continue dans une base XR}
\usage{
  Xair2R(polluants, dated, datef,
    dt = c("qh", "heure", "jour", "mois", "an"), merging,
    codeV = c("A", "R", "O", "W"), dsn = NULL, uid = NULL,
    pwd = NULL, brute = FALSE, reseaux = NULL,
    stations = NULL, campagnes = NULL, host = NULL,
    keep.state = FALSE, XR6 = TRUE)
}
\arguments{
  \item{polluants}{chaînes de caractères utilisées pour la
  recherche}

  \item{dated}{date initiale de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{datef}{date de fin de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).  des
  mesures à rappatrier.}

  \item{dt}{un des éléments suivant ('h', 'qh', 'd', 'm',
  'y') précisant la période des données à rappatrier.}

  \item{merging}{booléen. Si TRUE, les données sont
  récupérées dans la table normale de stockage de XR ; si
  FALSE, les données sont récupérées dans la table
  \code{BRUTE}.}

  \item{codeV}{liste des codes états d'XR à considérer
  comme valide. Par défaut les données dont le code état
  est'A', 'R', 'O' ou 'W' sont considérées comme valides.
  les autres sont remplacées par NA.}

  \item{brute}{booléen. Si TRUE, les données sont
  récupérées dans la table normale de stockage de XR ; si
  FALSE, les données sont récupérées dans la table
  \code{BRUTE}.}

  \item{keep.state}{si FALSE, seules les valeurs de
  concentrations sont récupérées ; si TRUE les codes états
  sont également récupérés.}

  \item{XR6}{TRUE si la version de XR est supérieure ou
  égale à 6, FALSE sinon.}

  \item{reseaux}{chaînes de caractères correspondant aux
  réseaux à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetReseaux}}).}

  \item{stations}{chaînes de caractères correspondant aux
  stations à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetStations}}).}

  \item{campagnes}{chaînes de caractères correspondant aux
  campagnes à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetCampagnes}}).}

  \item{dsn}{Nom de la base de données (cf le pilote
  concerné, JDBC ou ODBC).  optionnel : sera demandé si
  nécessaire.}

  \item{uid}{Identifiant utilisé pour la connection.
  optionnel : sera demandé si nécessaire.}

  \item{pwd}{Mot de passe pour initialiser la connection.
  optionnel : sera demandé si nécessaire.}

  \item{host}{Adresse de l'hôte hébergeant la base de
  données. Uniquement pour les systèmes type unix,
  utilisant le pilote JDBC.  optionnel : sera demandé si
  nécessaire.}
}
\value{
  un objet de classe \code{TimeIntervalDataFrame} contenant
  les données demandées.
}
\description{
  La fonction permet de récupérer une ou plusieurs séries
  de données stockée(s) dans une base XR entre 2 dates
  données au format voulu (heure, mois, etc.). Les données
  sont retournées au format
  \code{\link[timetools]{TimeIntervalDataFrame}} défini
  dans le paquet \code{\link[timetools]{timetools}}. Cette
  fonction est définie pour conserver une certaine
  compatibilité avec les versions antérieures de Qair.
}
\seealso{
  \code{\link{xrGetContinuousData}}
}

