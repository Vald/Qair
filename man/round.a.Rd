\name{round.a}
\alias{round.a}
\title{Calcul des arrondis}
\usage{
  round.a(x, digits = 0)
}
\arguments{
  \item{x}{vecteur \sQuote{numeric} à arrondir.}

  \item{digits}{entier indiquant le nombre de décimales à
  conserver.}
}
\value{
  Un objet de même nature que \sQuote{x} contenant les
  valeurs arrondies.
}
\description{
  Cette fonction permet de calculer des arrondis. Plusieurs
  méthodes de calcul étant possible, celle-ci implémente
  strictement celle préconisée par l'ADEME pour le domaine
  de la surveillance de la qualité de l'air.
}
\seealso{
  \code{\link[base]{round}}, \code{\link{quantile.a}}
}

