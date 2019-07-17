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
#' Les arguments uid et pwd sont les identifiants classiques de connexion à XR.
#' Ils sont requis pour l'authentification auprès de la base. Si jamais ils ne correspondent
#' pas, la connexion à XR est limitée et l'accès à toutes les informations n'est
#' pas possible. Le package Qair fonctionne alors en mode dégradé.
#'
#' @param host Adresse de l'hôte hébergeant la base de données.
#'	optionnel : sera demandé si nécessaire.
#' @param port Port de l'hôte hébergeant la base de données (8443 ou 8080).
#'	optionnel : sera demandé si nécessaire.
#' @param version Version de Qair avec laquelle doivent être compatibles les résultats
#'  des requêtes : 2 -> avec Qair_2* (rétro-compatibilité), 3 sinon.
#'	optionnel : sera demandé si nécessaire.
#' @param uid Identifiant pour la connexion. Optionnel, sera demandé si nécessaire. Voir
#'  détails.
#' @param pwd Mot de passe pour la connexion. Optionnel, sera demandé si nécessaire. Voir
#'  détails.
#' @return Une connexion à la base XR (il s'agit en fait d'une liste avec 'host' et
#'  'port'. Cela permet de gérer plusieurs 'connexion' simultanément et de mimer
#'  le comportement de Qair2).
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
xrConnect <- function(host=NULL, port=NULL, version=NULL, uid=NULL, pwd=NULL) {
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

	if(!is.null(uid)) {
		options(Xair.=host)
	} else if(is.null(getOption('Xair.host'))) {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}


	xr <- list(host   = options()[['Xair.host']],
			   port   = options()[['Xair.port']],
			   version= options()[['Xair.version']])
	class(xr) <- 'xr'


	if(nrow(xrGetStations(conn, )))

	return (xr)
}

