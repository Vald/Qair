\name{xrGetMesures}
\alias{xrGetMesures}
\title{Recuperation des mesures definies dans une base XR}
\usage{
  xrGetMesures(conn, pattern = NULL,
    search.fields = c("IDENTIFIANT", "NOM_COURT_MES"),
    campagnes = NULL, reseaux = NULL, stations = NULL,
    polluants = NULL, fields = NULL,
    collapse = c("AND", "OR"))
}
\arguments{
  \item{fields}{vecteurs indiquant les champs de la table à
  récupérer.  Tous par défaut.}

  \item{conn}{une connection valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{pattern}{chaînes de caractères utilisées pour la
  recherche (cf \sQuote{Details}).}

  \item{search.fields}{champ de la table dans lesquels
  \code{pattern} doit être recherché.}

  \item{campagnes}{chaînes de caractères correspondant aux
  campagnes à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetCampagnes}}).}

  \item{reseaux}{chaînes de caractères correspondant aux
  réseaux à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetReseaux}}).}

  \item{stations}{chaînes de caractères correspondant aux
  stations à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetStations}}).}

  \item{polluants}{chaînes de caractères correspondant aux
  polluants à rappatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetPolluants}}).}

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}
}
\value{
  une data.frame correspondant au contenu de la table pour
  les mesures trouvées.
}
\description{
  La fonction permet de lister les mesures existantes dans
  la base XR référencée par la connection \code{conn}.
}
\seealso{
  \code{\link{xrGetContinuousData}}
}

