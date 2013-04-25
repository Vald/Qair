\name{xrGetStations}
\alias{xrGetStations}
\title{Recuperation des stations de mesures definies dans une base XR}
\usage{
  xrGetStations(conn, pattern = NULL,
    search.fields = c("IDENTIFIANT", "NOM_COURT_SIT"),
    campagnes = NULL, reseaux = NULL, fields = NULL,
    mesures = NULL, collapse = c("AND", "OR"),
    exact = FALSE, SP = FALSE,
    proj4string = CRS("+init=epsg:2154"))
}
\arguments{
  \item{mesures}{chaînes de caractères correspondant aux
  mesures à rapatrier (optionnel) (utilisé via la fonction
  \code{\link{xrGetMesures}}).  si c'est un vecteur, est
  directement utilisé comme pattern pour la fonction
  xrGetMesures Si C'est une liste, les éléments doivent
  être nommés. Chaque élément est alors utilisé comme
  argument pour la fonction xrGetMesures pattern doit alors
  être précisé :

  \code{... list(pattern = 'N2_VER', search.fields =
  'IDENTIFIANT') ...}}

  \item{fields}{vecteurs indiquant les champs de la table à
  récupérer.  Tous par défaut.}

  \item{SP}{booléen indiquant si le résultat doit être au
  format SpatialPointsDataFrame. proj4string est alors
  utilisé pour préciser le référentiel spatial choisi. Les
  coordonnées utilisées sont les champs 'LAMBERTX' et
  LAMBERTY' de XR.}

  \item{proj4string}{indique les paramètres de projections.
  Par défaut : CRS('+init=epsg:2154') (lambert 93). Pour du
  lambert II étendu, indiquer : CRS('+init=epsg:27572')}

  \item{conn}{une connexion valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{pattern}{chaînes de caractères utilisées pour la
  recherche (cf \sQuote{Details}).}

  \item{search.fields}{champ de la table dans lesquels
  \code{pattern} doit être recherché.}

  \item{campagnes}{chaînes de caractères correspondant aux
  campagnes à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetCampagnes}}).  si c'est un
  vecteur, est directement utilisé comme pattern pour la
  fonction xrGetCampagnes Si c'est une liste, les éléments
  doivent être nommés. Chaque élément est alors utilisé
  comme argument pour la fonction xrGetCampagnes pattern
  doit alors être précisé :

  \code{... list(pattern='POITIERS_003',
  search.fields='NOM_COURT_CM') ...}}

  \item{reseaux}{chaînes de caractères correspondant aux
  réseaux à rapatrier (optionnel) (utilisé via la
  fonction\code{\link{xrGetReseaux}}).  si c'est un
  vecteur, est directement utilisé comme pattern pour la
  fonction xrGetResaux. Si c'est une liste, les éléments
  doivent être nommés. Chaque élément est alors utilisé
  comme argument pour la fonction xrGetReseaux.  pattern
  doit alors est précisé :

  \code{... list(pattern='R-O3',
  search.fields='NOM_COURT_RES') ...}}

  \item{collapse}{conjonction à appliquer entre les
  différents critères de recherche indiqués.}

  \item{exact}{booléen indiquant si les mesures à rapatrier
  doivent être exactement identiques à \sQuote{pattern} ou
  si \sQuote{pattern} doit être utilisé dans une expression
  régulière.}
}
\value{
  une data.frame correspondant au contenu de la table pour
  les stations trouvées. Si SP est TRUE, la data.frame est
  "encapsulée" dans une
  \code{\link[maptools]{SpatialPointsDataFrame}}.
}
\description{
  La fonction permet de lister les stations existantes dans
  la base XR référencée par la connexion \code{conn}.
}
\seealso{
  \code{\link{xrGetContinuousData}},
  \code{\link[maptools]{SpatialPointsDataFrame}}
}

