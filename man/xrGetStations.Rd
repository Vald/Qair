\name{xrGetStations}
\alias{xrGetStations}
\title{Recuperation des stations de mesures definies dans une base XR}
\usage{
  xrGetStations(conn, pattern = NULL,
    search.fields = c("IDENTIFIANT", "NOM_COURT_SIT"),
    campagnes = NULL, reseaux = NULL, fields = NULL,
    mesures = NULL, collapse = c("AND", "OR"),
    exact = FALSE)
}
\arguments{
  \item{mesures}{chaînes de caractères correspondant aux
  mesures à rappatrier (optionnel) (utilisé via la fonction
  \code{\link{xrGetMesures}}).}

  \item{fields}{vecteurs indiquant les champs de la table à
  récupérer.  Tous par défaut.}

  \item{conn}{une connexion valide telle que retournée par
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

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}

  \item{exact}{booléen indiquant si les mesures à
  rappatrier doivent être exactement identiques à
  \sQuote{pattern} ou si \sQuote{pattern} doit être utilisé
  dans une expression régulière.}
}
\value{
  une data.frame correspondant au contenu de la table pour
  les stations trouvées.
}
\description{
  La fonction permet de lister les stations existantes dans
  la base XR référencée par la connexion \code{conn}.
}
\seealso{
  \code{\link{xrGetContinuousData}}
}

