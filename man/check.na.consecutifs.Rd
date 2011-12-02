\name{check.na.consecutifs}
\alias{check.na.consecutifs}
\title{Test la validité d'un vecteur par rapport à nombre de NA consécutif}
\usage{
  check.na.consecutifs(x, na.consecutif.max = 720)
}
\arguments{
  \item{x}{vecteur à tester}

  \item{na.consecutif.max}{entier représentant le nombre
  maximal de valeurs NA consécutive autorisé}
}
\value{
  TRUE or FALSE see \sQuote{Description}.
}
\description{
  Le vecteur est considéré valide si le nombre de NA
  consécutifs qui le constitue est inférieur ou égal à
  \code{na.consecutif.max}. Si le vecteur est valide, la
  valeur retournée est TRUE, FALSE sinon.
}

