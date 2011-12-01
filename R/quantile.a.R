#' Calcul des quantiles
#' 
#' Cette fonction permet de calculer des quantiles. Plusieurs
#' méthodes de calcul étant possible, celle-ci implémente strictement
#' celle préconisée par l'ADEME pour le domaine de la surveillance de 
#' la qualité de l'air.
#'
#' @inheritParams stats::quantile
#' @param x vecteur \sQuote{numeric} pour lequel les quantiles doivent 
#' 	être calculés.
#' @param pc valeur comprise entre 0 et 1 qui précise le pourcentage minimal
#'	de valeurs disponibles pour calculer le (les) quantile(s). La fonction
#' retournera NA si ce pourcentage n'est pas atteint.
#' @param \dots arguments pour d'autres méthodes.
#'
#' @return NA si le pourcentage de données valides n'est pas atteint
#'	le ou les quantiles précisés par \code{probs}.
#'
#' @seealso \code{\link[stats]{quantile}}, \code{\link{round.a}}
quantile.a <- function(x, probs, pc=0.75, ...)
	ifelse(mean(!is.na(x))>=pc, sort(x)[round.a(probs * sum(!is.na(x)))], NA)
