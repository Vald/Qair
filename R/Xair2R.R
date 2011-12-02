#' Récupération de mesure continue dans une base XR
#'
#' La fonction permet de récupérer une ou plusieurs séries 
#' de données stockée(s) dans une base XR entre 2 dates données
#' au format voulu (heure, mois, etc.). Les données sont retournées
#' au format \code{\link[timetools]{TimeIntervalDataFrame}} défini dans le 
#' paquet \code{\link[timetools]{timetools}}.
#' Cette fonction est définie pour conserver une certaine compatibilité
#' avec les versions antérieures de Qair.
#' 
#' @param polluants chaînes de caractères utilisées pour la recherche
#' @param dated date initiale de la période à rappatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param datef date de fin de la période à rappatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' 	des mesures à rappatrier.
#' @param dt un des éléments suivant ('h', 'qh', 'd', 'm', 'y') précisant la période
#' 	des données à rappatrier.
#' @param merging booléen. Si TRUE, les données sont récupérées dans la table normale
#'	de stockage de XR ; si FALSE, les données sont récupérées dans la table \code{BRUTE}.
#' @param codeV liste des codes états d'XR à considérer comme valide. Par défaut
#'	les données dont le code état est'A', 'R', 'O' ou 'W' sont considérées comme valides.
#'	les autres sont remplacées par NA.
#' @param brute booléen. Si TRUE, les données sont récupérées dans la table normale
#'	de stockage de XR ; si FALSE, les données sont récupérées dans la table \code{BRUTE}.
#' @inheritParams xrGetContinuousData
#' @inheritParams xrConnect
#' @param keep.state si FALSE, seules les valeurs de concentrations
#' 	sont récupérées ; si TRUE les codes états sont également récupérés.
#' @param XR6 TRUE si la version de XR est supérieure ou égale à 6, FALSE sinon.
#'
#' @return un objet de classe \code{TimeIntervalDataFrame}
#' 	contenant les données demandées.
#'
#' @seealso \code{\link{xrGetContinuousData}}
Xair2R <- function (polluants, dated, datef, dt = c("qh", "heure", "jour", "mois", "an"),
		    merging, codeV=c("A", "R", "O", "W"),
		    dsn=NULL, uid=NULL, pwd=NULL, brute=FALSE,
		    reseaux=NULL, stations=NULL, campagnes=NULL,
		    host=NULL, keep.state=FALSE, XR6=TRUE)
{
	warning ("La fonction Xair2R est obsolete. il est preferable d'utiliser la fonction xrGetContinuousData.")

	conn <- xrConnect(dsn=dsn, uid=uid, pwd=pwd, host=host)
	result <- xrGetContinuousData (conn, pattern = polluants, start = dated, end = datef,
			     period = switch (dt, qh='qh', heure='h', jour='d', mois='m', an='y'),
			     validated = !brute, valid.states = codeV, what = ifelse(keep.state, 'both', 'value'),
			     campagnes = campagnes, reseaux = reseaux, stations = stations, XR6 = XR6)
	dbDisconnect (conn)
	return (result)
}


