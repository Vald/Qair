\name{larochelle}
\alias{larochelle}
\docType{data}
\title{ contient des donnees de pollution sur la rochelle. (France) }
\description{
  Les donnees concernees ont ete mesurees en 2007 sur les 3 stations d'Atmo Poitou-Charentes.
}
\usage{data(larochelle)}
\format{
  A data frame with 35040 observations on the following 8 variables.
  \describe{
    \item{\code{date}}{a chron - les dates des mesures, en quart-horaire}
    \item{\code{O3_VER}}{a numeric vector - concentration d'ozone sur la station de Verdun.}
    \item{\code{N2_VER}}{a numeric vector - concentration de dioxyde d'azote sur la station de Verdun. }
    \item{\code{NO_VER}}{a numeric vector - concentration de monoxyde d'azote sur la station de Verdun. }
    \item{\code{O3_VAU}}{a numeric vector - concentration d'ozone sur la station de Vaugoin. }
    \item{\code{O3_AYT}}{a numeric vector - concentration d'ozone sur la station d'Aytre. }
    \item{\code{DV_ATM}}{a numeric vector - direction de vents au siege d'Atmo Poitou-Charentes. }
    \item{\code{VV_ATM}}{a numeric vector - vitesse de vents au siege d'Atmo Poitou-Charentes. }
  }
}
\source{
  http://www.atmo-poitou-charentes.org
}
\examples{
data(larochelle)
plot (
  larochelle["07-01-01", "07-01-10"][grep("O3", names(larochelle), value=TRUE)],
  ask.leg=FALSE)
}
\keyword{datasets}
