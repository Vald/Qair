\name{dbListFields.RODBC}
\alias{dbListFields.RODBC}
\title{extends dbListFields for RODBC objects}
\usage{
  \method{dbListFields}{RODBC} (conn, name, ...)
}
\arguments{
  \item{\dots}{pour compatibiltié avec la méthode
  générique}

  \item{conn}{a connection object}

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

