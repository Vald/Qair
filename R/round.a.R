#' Calcul des arrondis
#' 
#' Cette fonction permet de calculer des arrondis. Plusieurs
#' méthodes de calcul étant possible, celle-ci implémente strictement
#' celle préconisée par l'ADEME pour le domaine de la surveillance de 
#' la qualité de l'air.
#'
#' @param x vecteur \sQuote{numeric} à arrondir.
#' @param digits entier indiquant le nombre de décimales à conserver.
#'
#' @return Un objet de même nature que \sQuote{x} contenant les valeurs arrondies.
#'
#' @seealso \code{\link[base]{round}}, \code{\link{quantile.a}}
round.a <- function(x, digits=0) {
	.local <- function(x, digits) {
		n <- ifelse(x < 0, -1L, 1L)
		x <- abs(x)
		x <- x * (10**digits)
		n * ifelse (abs (x%%1 - 0.5) < .Machine$double.eps^0.5,
			ceiling(x)/(10**digits),
			round(x)/(10**digits) )
	}
	if (is.data.frame (x)) return (data.frame (lapply (x, .local, digits) ) )
	.local (x, digits)
}

