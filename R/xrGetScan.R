#' Fonction pour recuperer des donnees scan de XR
#'
#' @inheritParams xrGetContinuousData
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
