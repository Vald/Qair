\name{indicesXR2R}
\alias{indicesXR2R}
\title{Recuperation des indices de qualite de l'air}
\usage{
  indicesXR2R(conn, agglos, start, end)
}
\arguments{
  \item{agglos}{chaîne de caractères indiquant les les
  réseaux d'indices pour lesquels les donnée doivent être
  rappatriées (invoquer \code{indices2XR()} sans arguments
  pour connaître les valeurs possibles).}

  \item{conn}{une connection valide telle que retournée par
  \code{\link{xrConnect}}.}

  \item{start}{date initiale de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}

  \item{end}{date de fin de la période à rappatrier (au
  format \code{\link[base]{POSIXct}} ou character pouvant
  être converti en \code{\link[base]{POSIXct}}).}
}
\value{
  un objet de classe
  \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  contenant les indices pour la période demandée.
}
\description{
  La fonction permet de rappatrier les indices de la
  qualité de l'air stocké dans une base XR. Pour connaître
  la liste des réseaux d'indices definis, il suffit
  d'invoquer la fonction sans arguments.
}
\seealso{
  \code{\link{xrConnect()}}, \code{\link{plot.indice}},
  \code{\link[timetools]{TimeIntervalDataFrame-class}}
}

