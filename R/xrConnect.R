#' Initialisation d'une connexion a une base XR.
#'
#' @section Details: Cette fonction permet d'initialiser une connexion avec une base XR.
#' Une fois définis, les arguments sont définis pour la session entière.
#'
#' Il est possible de définir ces 'arguments' en utilisant la 
#' fonction \code{\link[base]{options}} avec les mêmes arguments, mais précédés de 'Xair.'
#' (cf la section exemple).
#'
#' Les arguments non-spécifiés à l'appel de la fonction et qui n'apparaissent pas
#' dans les options seront interactivement demandés à l'utilisateur.
#' 
#' Il faut systématiquement préciser les éléments de connexion pour l'API-REST 
#' (même si non utilisée).
#'
#' @param host Adresse de l'hôte hébergeant l'API-REST.
#' @param port Port de l'hôte hébergeant l'API-REST (8443 ou 8080).
#' @param version Version de Qair avec laquelle doivent être compatibles les résultats
#'  des requêtes : 2 -> avec Qair_2* (rétro-compatibilité), 3 sinon.
#'  Non-pris en compte pour les données manuelles : le format est nécessairement
#'  en v2.
#' @param debug Si TRUE, chaque requête http envoyée est affichée dans le terminal.
#'  FALSE par defaut. Peut être spécifié via l'option Xair.debug.
#' @param nbattempt Nombre de tentative d'exécution d'une requête avant plantage
#'  définitif. 100 par défaut. Nécessaire dans le cadre de la limititation du nombre 
#'  de requêtes pr unité de temps imposée par ISEO.
#' @inheritParams xrGetContinuousData
#' @return Une connexion à la base XR.
#'
#' @aliases options, Xair.host, Xair.port
#' @seealso \code{\link[base]{options}}
#'
#' @examples
#' \dontrun{
#' # cette ligne permet de définir l'identifiant et la base de données
#' options (Xair.host='xair', Xair.port='8443')
#' xrConnect()
#'}
#' @export
xrConnect <- function(host=NULL, port=NULL, version=NULL, debug=NULL, nbattempt=NULL,
					  silent) {
	if(!is.null(debug)) options(Xair.debug=debug)
	if(!is.null(nbattempt)) options(Xair.nbattempt=nbattempt)
	if(!missing(silent)) options(Xair.silent=silent)

	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption('Xair.host'))) {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}

	if(!is.null(port)) {
		options(Xair.port=port)
	} else if(is.null(getOption('Xair.port'))) {
		cat("port d'accès à la base (8443 https, 8080 http) :\n")
		options(Xair.port=scan(what='character', nlines=1))
		cat('\n')
	}
	if(!options()[['Xair.port']] %in% c(8443, 8080))
		stop("XR n'accepte que les ports 8443 ou 8080")

	if(!is.null(version)) {
		options(Xair.version=version)
	} else if(is.null(getOption('Xair.version'))) {
		cat("version de Qair (2 pour une compatibilité avec la v2, 3 sinon :\n")
		options(Xair.version=scan(what='character', nlines=1))
		cat('\n')
	}
	if(!options()[['Xair.version']] %in% 2:3)
		stop("Les versions de Qair acceptées sont uniquement 2 ou 3")

	#--------------------------------------------------------------------------

	xr <- list(host   = options()[['Xair.host']],
			   port   = options()[['Xair.port']],
			   version= options()[['Xair.version']])
	class(xr) <- 'xr'

	if(nrow(suppressMessages(xrGetStations(xr))) == 0) stop('Problème de connexion')

	return (xr)
}

