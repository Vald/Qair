#' Initialisation d'une connection a une base XR.
#'
#' @section Details: Cette fonction permet d'initialiser une connection avec une base XR.
#' Sous windows, le pilote utilisé est ODBC (nécessite d'avoir
#' installer le paquet \code{\link[RODBC]{RODBC}}), sous linux JDBC
#' (nécessite d'avoir installer le paquet \code{\link[RJDBC]{JDBC}}).
#'
#' Une fois définis, les arguments sont définis pour la session entière.

#' Il est possible de définir ces 'argument' en utilisant la 
#' fonction \code{\link[base]{options}} avec les mêmes arguments, mais précédés de 'Xair.'
#' (cf la section exemple).
#'
#' Les arguments non-spécifiés à l'appel de la fonction et qui n'apparaissent pas
#' dans les options seront interactivement demandés à l'utilisateur.
#'
#' @param dsn Nom de la base de données (cf le pilote concerné, JDBC ou ODBC).
#'	optionnel : sera demandé si nécessaire.
#' @param uid Identifiant utilisé pour la connection.
#'	optionnel : sera demandé si nécessaire.
#' @param pwd Mot de passe pour initialiser la connection.
#'	optionnel : sera demandé si nécessaire.
#' @param host Adresse de l'hôte hébergeant la base de données. Uniquement
#'	pour les systèmes type unix, utilisant le pilote JDBC.
#'	optionnel : sera demandé si nécessaire.
#' @param ojdbc Emplacement de la librairie contenant la définition des classes
#'	nécessaires à RJDBC. Uniquement 
#'	pour les systèmes type unix, utilisant le pilote JDBC.
#' @return Une connection à la base XR (le type exact dépend du pilote utilisé).
#'
#' @aliases options, Xair.uid, Xair.pwd, Xair.host, Xair.dsn
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}},
#' 	\code{\link[base]{options}}
#'
#' @examples
#' \dontrun{
#' # dans le cas d'un système windows
#' # cette ligne permet de définir l'identifiant et la base de données
#' options (Xair.uid='vlad', Xair.dsn='BaseN')
#' # la ligne suivante ne demandera que le mot de passe.
#' xrConnect()
#'}
xrConnect <- function(dsn=NULL, uid=NULL, pwd=NULL, host=NULL, ojdbc=NULL) {
	# host et ojdbc sont a specifier uniquement dans le cas de l'utilisation de RJDBC

	# definition de la base de donnees (dsn)
	if(!is.null(dsn)) {
		options(Xair.dsn=dsn)
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='unix') {
		cat('nom de la base de donnees (DataSourceName) :\n')
		options(Xair.dsn=scan(what='character', nlines=1))
		cat('\n')
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='windows') {
		cat('nom de la base de donnees (DataSourceName) :\n',
		    names(odbcDataSources('system')), '\n')
		options(Xair.dsn=scan(what='character', nlines=1));cat('\n')
	}

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption('Xair.uid'))) {
		cat('identifiant pour la connection :\n')
		options(Xair.uid=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption('Xair.pwd'))) {
		cat('mot de passe pour la connection :\n')
		options(Xair.pwd=scan(what='character', nlines=1))
		cat('\n')
	}
	# definition de l'hote (unix like system)
	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption('Xair.host')) & .Platform$OS.type=='unix') {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}

	# connection a la base
	if(.Platform$OS.type=='windows') {
		conxair <- try (odbcConnect (getOption('Xair.dsn'),
					     uid = getOption('Xair.uid'),
					     pwd = getOption('Xair.pwd'),
					     case = 'nochange', believeNRows = TRUE) )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connection a la base Xair.')
	} else if(.Platform$OS.type=='unix') {
		if(!is.null(options()$Xair.ojdbc.file)) {
			drv <- JDBC('oracle.jdbc.OracleDriver', options()$Xair.ojdbc.file)
		} else {
			cat('Veuillez entrer le chemin vers le fichier java ojdbc*.jar\nAfin de ne pas avoir a renseigner ce chemin a chaque session\nvous pouvez definir la variable options(Xair.ojdbc.file = ...) dans le fichier ~/.Rprofile\n')
			drv <- scan(nmax = 1, what = 'character')
		}
		conxair <- try (dbConnect (drv,
					   paste ('jdbc:oracle:thin:@', getOption('Xair.host'),
						  ':1521:', getOption('Xair.dsn'), sep=''),
					   getOption('Xair.uid'),
					   getOption('Xair.pwd'),
					   identifer.quote='\'') )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connection a la base Xair.')
	} else {
		stop('platforme non reconnue')
	}
	return (conxair)
}

