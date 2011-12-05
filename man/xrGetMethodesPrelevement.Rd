\name{xrGetMethodesPrelevement}
\alias{xrGetMethodesPrelevement}
\title{Recuperation des methodes de prelevement manuels definies dans une base XR}
\usage{
  xrGetMethodesPrelevement(conn, pattern = NULL,
    search.fields = c("LIBELLE"), fields = NULL,
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

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}
}
\value{
  une data.frame correspondant au contenu de la table pour
  les méthodes trouvées.
}
\description{
  La fonction permet de lister les méthodes de prélèvement
  existantes dans la base XR référencée par la connection
  \code{conn}.
}
\seealso{
  \code{\link{xrGetManualData}},
  \code{\link{xrGetSitesPrelevement}}
}

