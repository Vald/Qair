\name{xrGetReseaux}
\alias{xrGetReseaux}
\title{Récupération des réseaux de mesures définis dans une base XR}
\usage{
  xrGetReseaux(conn, pattern = NULL,
    search.fields = c("NOM_COURT_RES", "NOM_RES"),
    fields = NULL)
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
}
\value{
  une data.frame correspondant au contenu de la table pour
  les réseaux trouvés.
}
\description{
  La fonction permet de lister les réseaux existants dans
  la base XR référencée par la connection \code{conn}.
}
\seealso{
  \code{\link{xrGetContinuousData}}
}

