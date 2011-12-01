#' Calcule un interval de représentation.
#'
#' À partir d'un vecteur de valeurs numérique, la fonction
#' renvoie un vecteur de 2 valeurs pouvant servir de limites
#' pour un graphique (xlim, ylim, rlim, etc.)
#'
#' @param x vecteur de 'numeric' pour lequel on souhaite déterminer
#' 	les limites grpahiques
#' @param na.rm boolean les valeurs NA doivent être elles considérées ?
#' @return vecteur de 2 'numeric's

compute.lim <- function(x, na.rm=FALSE) {
	x <- range(x, na.rm=na.rm)

	e <- floor(log(abs(x), 10))
	u <- floor(abs(x)/10^e)
	r <- abs(x) - u * 10^e

	lim <- ifelse(r == 0, x, sign(x) * (u+1) * 10^e)

	if(all(x > 0)) lim[1] <- 0
	if(all(x < 0)) lim[2] <- 0
	
	lim[x == 0] <- 0

	if(na.rm) lim[is.na(lim)] <- 0

	return(lim)
}
