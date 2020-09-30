#' Fonction pour recuperer des donnees scan de XR
#'
#' @param \dots Arguments renvoyés à la fonction xrGetContinuousData.
#'  L'argument 'period' ne peut être modifié et sa valeur est fixée à 'scan'.
#'
#' @return un objet de classe \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'   contenant les données demandées.
#'
#' @seealso \code{\link{xrGetContinuousData}},
#'	\code{\link{xrGetSitesPrelevement}}, \code{\link{xrGetMethodesPrelevement}},
#'	 \code{\link{xrGetCampagnes}}, \code{\link{xrGetPolluants}}
#' 	\code{\link[timetools]{TimeIntervalDataFrame-class}}

xrGetScan <- function (...) {
	arguments <- list(...)
	arguments[['period']] <- 'scan'
	return(do.call(xrGetContinuousData, arguments))
}
