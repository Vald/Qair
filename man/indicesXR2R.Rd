\name{indicesXR2R}
\alias{indicesXR2R}
\title{Recuperation des indices de qualite de l'air}
\usage{
  indicesXR2R(conn, agglos, start, end, detail = FALSE,
    format = c("type", "agglo", "aucun"))
}
\arguments{
  \item{agglos}{chaîne de caractères indiquant les les
  réseaux d'indices pour lesquels les donnée doivent être
  rappatriées (invoquer \code{indices2XR()} sans arguments
  pour connaître les valeurs possibles).}

  \item{detail}{booleen indiquant si le détails des indices
  doit être rappatrié également. Autrement dit si les
  sous-indices doivent également être récupérés dans la
  base XR. Si TRUE, le paramètre \sQuote{format} permet de
  préciser l'agencement des données.}

  \item{format}{utilisé uniquement si \sQuote{detail} est à
  TRUE. Trois formats sont à l'heure actuelle disponible :
  \describe{ \item{type}{une liste de TimeIntervalDataFrame
  est retournée. Le premier élément de la liste contient un
  TimeIntervalDataFrame avec les indices pour chaque agglo,
  le deuxième un TimeIntervalDataFrame avec les sous-indice
  du premier polluant, le troisième avec les sous-indices
  du deuxième polluant, etc.} \item{agglo}{Une liste de
  TimeIntervalDataFrame est retournée. Chaque élément de la
  liste contient un TimeIntervalDataFrame avec tous les
  indices et sous-indices pour une seule agglo.}
  \item{aucun}{Un simple TimeIntervalDataFrame est
  retourné. Les noms des colonnes sont formés sur le schéma
  AGGLO.POLLUANT} }}

  \item{conn}{une connexion valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{start}{date initiale de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{end}{date de fin de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}
}
\value{
  Si detail est FALSE, un objet de classe
  \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}

  si detail est TRUE, une liste de
  \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}.
  cf le \sQuote{format} pour le détail.

  contenant les indices pour la période demandée.
}
\description{
  La fonction permet de rappatrier les indices de la
  qualité de l'air stocké dans une base XR. Pour connaître
  la liste des réseaux d'indices definis, il suffit
  d'invoquer la fonction sans arguments.
}
\author{
  Fabrice Caïni, Vladislav Navel
}
\seealso{
  \code{\link{xrConnect}}, \code{\link{plot.indice}},
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
}

