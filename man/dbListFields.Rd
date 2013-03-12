\name{dbListFields}
\alias{dbDisconnect}
\alias{dbDisconnect,RODBC-method}
\alias{dbListFields}
\alias{dbListFields,RODBC,character-method}
\title{extends dbListFields for RODBC objects}
\usage{
  dbListFields(conn, name, ...)

  dbDisconnect(conn, ...)
}
\arguments{
  \item{\dots}{pour compatibiltié avec la méthode
  générique}

  \item{conn}{a connexion object}

  \item{name}{a character string with the name of the
  remote table.}
}
\description{
  extends dbListFields for RODBC objects
}
\seealso{
  \code{\link[DBI]{dbListFields}},
  \code{\link[RODBC]{sqlColumns}}
}

