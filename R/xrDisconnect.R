#' Déconnexion à XR.
#'
#' @inheritParams xrGetContinuousData
#' @seealso \code{\link[base]{options}}
#'
#' @examples
#' \dontrun{
#' # cette ligne permet de définir l'identifiant et la base de données
#' options (Xair.host='xair', Xair.port='8443')
#' xr <- xrConnect()
#' xr <- xrDisconnect(xr)
#'}
#' @export
xrDisconnect <- function(conn) {
	if(conn[['logged']] && status_code(RETRY(
		'GET',
		sub('login', 'logout', xrGetUrl(conn, authentification=TRUE)),
		httr::config(ssl_verifypeer=FALSE,ssl_verifyhost=FALSE))) == 200) {
		conn[['logged']] <- FALSE
		message("Déconnexion réussie. L'API reste consultable sur sa partie publique (si activée).")
	} else {
		message('Connexion non-authentifiée, pas de déconnexion.')
	}
	return(conn)
}

