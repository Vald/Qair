\name{quantile.a}
\alias{quantile.a}
\title{Calcul des quantiles}
\usage{
  quantile.a(x, probs, pc = 0.75, ...)
}
\arguments{
  \item{x}{vecteur \sQuote{numeric} pour lequel les
  quantiles doivent être calculés.}

  \item{pc}{valeur comprise entre 0 et 1 qui précise le
  pourcentage minimal de valeurs disponibles pour calculer
  le (les) quantile(s). La fonction retournera NA si ce
  pourcentage n'est pas atteint.}

  \item{\dots}{arguments pour d'autres méthodes.}

  \item{probs}{numeric vector of probabilities with values
  in \eqn{[0,1]}.  (Values up to \samp{2e-14} outside that
  range are accepted and moved to the nearby endpoint.)}
}
\value{
  NA si le pourcentage de données valides n'est pas atteint
  le ou les quantiles précisés par \code{probs}.
}
\description{
  Cette fonction permet de calculer des quantiles.
  Plusieurs méthodes de calcul étant possible, celle-ci
  implémente strictement celle préconisée par l'ADEME pour
  le domaine de la surveillance de la qualité de l'air.
}
\seealso{
  \code{\link[stats]{quantile}}, \code{\link{round.a}}
}

