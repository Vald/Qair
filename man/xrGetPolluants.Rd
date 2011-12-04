\name{xrGetPolluants}
\alias{xrGetPolluants}
\title{Recuperation des polluants definis dans une base XR}
\usage{
  xrGetPolluants(conn, pattern = NULL,
    search.fields = c("NOPOL", "CCHIM", "NCON"),
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
  les polluants trouvés.
}
\description{
  La fonction permet de lister les polluants existants dans
  la base XR référencée par la connection \code{conn}.
}
\seealso{
  \code{\link{xrGetContinuousData}}
}

