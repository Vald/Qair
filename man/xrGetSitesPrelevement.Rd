\name{xrGetSitesPrelevement}
\alias{xrGetSitesPrelevement}
\title{Recuperation des sites de prelevement manuels definis dans une base XR}
\usage{
  xrGetSitesPrelevement(conn, pattern = NULL,
    search.fields = c("IDSITEP", "LIBELLE"),
    campagnes = NULL, fields = NULL,
    collapse = c("AND", "OR"), exact = FALSE)
}
\arguments{
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

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}

  \item{exact}{booléen indiquant si les mesures à
  rappatrier doivent être exactement identiques à
  \sQuote{pattern} ou si \sQuote{pattern} doit être utilisé
  dans une expression régulière.}
}
\value{
  une data.frame correspondant au contenu de la table pour
  les sites trouvés.
}
\description{
  La fonction permet de lister les sites de prélèvement
  existants dans la base XR référencée par la connexion
  \code{conn}.
}
\seealso{
  \code{\link{xrGetManualData}},
  \code{\link{xrGetMethodesPrelevement}}
}

