% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrGetSitesPrelevement.R
\name{xrGetSitesPrelevement}
\alias{xrGetSitesPrelevement}
\title{Recuperation des sites de prelevement manuels definis dans une base XR}
\usage{
xrGetSitesPrelevement(conn, pattern = NULL, search.fields = c("IDSITEP",
  "LIBELLE"), campagnes = NULL, fields = NULL, collapse = c("AND", "OR"),
  exact = FALSE)
}
\arguments{
\item{conn}{une connexion valide telle que retournée par \code{\link{xrConnect}}.}

\item{pattern}{chaînes de caractères utilisées pour la recherche (cf \sQuote{Details}).}

\item{search.fields}{champ de la table dans lesquels \code{pattern} doit être
recherché.}

\item{campagnes}{chaînes de caractères correspondant aux campagnes à rapatrier
    (optionnel) (utilisé via la fonction\code{\link{xrGetCampagnes}}).
 si c'est un vecteur, est directement utilisé comme pattern pour la fonction
 xrGetCampagnes Si c'est une liste, les éléments doivent être nommés. Chaque
 élément est alors utilisé comme argument pour la fonction xrGetCampagnes
  pattern doit alors être précisé :

 \code{... list(pattern='POITIERS_003', search.fields='NOM_COURT_CM') ...}}

\item{fields}{vecteurs indiquant les champs de la table à récupérer.
Tous par défaut.}

\item{collapse}{conjonction à appliquer entre les différents critères de recherche
indiqués.}

\item{exact}{booléen indiquant si les mesures à rapatrier doivent être 
exactement identiques à \sQuote{pattern} ou si \sQuote{pattern} doit 
être utilisé dans une expression régulière.}
}
\value{
une data.frame correspondant au contenu de la table 
pour les sites trouvés.
}
\description{
La fonction permet de lister les sites de prélèvement existants dans la 
base XR référencée par la connexion \code{conn}.
}
\seealso{
\code{\link{xrGetManualData}}, \code{\link{xrGetMethodesPrelevement}}
}

