#' Initialisation d'une connexion a une base XR.
#'
#' @section Details: Cette fonction permet d'initialiser une connexion avec une base XR.
#' Sous windows, le pilote utilisé par défaut est ODBC (nécessite d'avoir
#' installer le paquet \code{\link[RODBC]{RODBC}}), sous linux Oracle
#' (nécessite d'avoir installer le paquet \code{\link[ROracle:Oracle]{ROracle}}).
#' Il est également possible d'utiliser le paquet \code{\link[RJDBC:JDBC]{RJDBC}}).
#'
#' Une fois définis, les arguments sont définis pour la session entière.

#' Il est possible de définir ces 'arguments' en utilisant la 
#' fonction \code{\link[base]{options}} avec les mêmes arguments, mais précédés de 'Xair.'
#' (cf la section exemple).
#'
#' Les arguments non-spécifiés à l'appel de la fonction et qui n'apparaissent pas
#' dans les options seront interactivement demandés à l'utilisateur.
#'
#' @param dsn Nom de la base de données (cf le pilote concerné, JDBC, ODBC ou Oracle).
#'	optionnel : sera demandé si nécessaire.
#' @param uid Identifiant utilisé pour la connexion.
#'	optionnel : sera demandé si nécessaire.
#' @param pwd Mot de passe pour initialiser la connexion.
#'	optionnel : sera demandé si nécessaire.
#' @param host Adresse de l'hôte hébergeant la base de données. Uniquement
#'	pour les systèmes type unix, utilisant le pilote JDBC ou Oracle.
#'	optionnel : sera demandé si nécessaire.
#' @param ojdbc Emplacement de la librairie contenant la définition des classes
#'	nécessaires à RJDBC. Uniquement dans le cas où jdbc est choisi comme driver.
#' @param drv.type chaine de caractere indiquant le driver de base de données à utiliser.
#' 	peut prendre les valeurs 'oracle', 'jdbc' ou 'odbc'. Par défault, la valeur utilisée
#' 	est 'oracle' pour les systèmes type 'unix' et 'odbc' pour windows.
#' @inheritParams RODBC::odbcConnect
#' @return Une connexion à la base XR (le type exact dépend du pilote utilisé).
#'
#' @aliases options, Xair.uid, Xair.pwd, Xair.host, Xair.dsn
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}},
#' 	\code{\link[base]{options}}, \code{\link[ROracle]{Oracle}}
#'
#' @examples
#' \dontrun{
#' # dans le cas d'un système windows
#' # cette ligne permet de définir l'identifiant et la base de données
#' options (Xair.uid='vlad', Xair.dsn='BaseN')
#' # la ligne suivante ne demandera que le mot de passe.
#' xrConnect()
#'}
xrConnect <- function(dsn=NULL, uid=NULL, pwd=NULL, host=NULL, ojdbc=NULL, drv.type=NULL, believeNRows=TRUE) {
	# host et ojdbc sont a specifier uniquement dans le cas de l'utilisation de RJDBC

	# définition du pilote à utiliser
	if (!is.null(drv.type)) {
		options(Xair.drv=drv.type)
	} else if (is.null(getOption('Xair.drv'))) {
		if(.Platform$OS.type=='unix') {
			if( requireNamespace('ROracle', quietly=TRUE) ) {
				message ("le pilote 'oracle' est utilisé pour la connexion à XR")
				options (Xair.drv = 'oracle')
			} else {
			loadNamespace ('RJDBC')
				message ("le pilote 'jdbc' est utilisé pour la connexion à XR")
				options (Xair.drv = 'jdbc')
			}
		} else if(.Platform$OS.type=='windows') {
			loadNamespace ('RODBC')
			message ("le pilote 'odbc' est utilisé pour la connexion à XR")
			options (Xair.drv = 'odbc')
		} else {
			cat("Plateforme non-reconnue.\nMerci de préciser le driver (drv.type) à
utiliser ('jdbc', 'odbc' ou 'oracle')\n")
			options(Xair.drv=scan(what='character', nlines=1))
			cat('\n')
		}
	}

	if (!getOption('Xair.drv') %in% c('odbc', 'jdbc', 'oracle'))
		stop("le pilote pour la connexion à la base doit être 'jdbc', 'odbc' ou oracle'")

	# definition de la base de donnees (dsn)
	if(!is.null(dsn)) {
		options(Xair.dsn=dsn)
	} else if(is.null(getOption('Xair.dsn')) & getOption('Xair.drv') %in% c('jdbc', 'oracle')) {
		cat('nom de la base de donnees (DataSourceName) :\n')
		options(Xair.dsn=scan(what='character', nlines=1))
		cat('\n')
	} else if(is.null(getOption('Xair.dsn')) & getOption('Xair.drv') == 'odbc') {
		cat('nom de la base de donnees (DataSourceName) :\n',
		    names(RODBC::odbcDataSources('system')), '\n')
		options(Xair.dsn=scan(what='character', nlines=1));cat('\n')
	}

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption('Xair.uid'))) {
		cat('identifiant pour la connexion :\n')
		options(Xair.uid=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption('Xair.pwd'))) {
		cat('mot de passe pour la connexion :\n')
		options(Xair.pwd=scan(what='character', nlines=1))
		cat('\n')
	}
	# definition de l'hote (jdbc)
	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption('Xair.host')) & getOption('Xair.drv') %in% c('jdbc', 'oracle')) {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition de l'emplacement des pilotes jdbc
	if(!is.null(ojdbc)) {
		options(Xair.ojdbc.file=ojdbc)
	} else if(is.null(getOption('Xair.ojdbc.file')) & getOption('Xair.drv') == 'jdbc') {
		cat('emplacement du fichier ojdbc*.jar définissant le pilote nécessaire à JDBC :\n')
		options(Xair.ojdbc.file=scan(what='character', nlines=1))
		cat('\n')
	}

	# connexion a la base
	if(getOption('Xair.drv') == 'odbc') {
		loadNamespace ('RODBC')
		conxair <- try (RODBC::odbcConnect (getOption('Xair.dsn'),
					     uid = getOption('Xair.uid'),
					     pwd = getOption('Xair.pwd'),
					     case = 'nochange', believeNRows = believeNRows) )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connexion a la base Xair.')
	} else if(getOption('Xair.drv') == 'jdbc') {
		loadNamespace ('RJDBC')
		drv <- RJDBC::JDBC('oracle.jdbc.OracleDriver', getOption('Xair.ojdbc.file'))
		conxair <- try (dbConnect (drv,
					   paste ('jdbc:oracle:thin:@', getOption('Xair.host'),
						  ':1521:', getOption('Xair.dsn'), sep=''),
					   getOption('Xair.uid'),
					   getOption('Xair.pwd'),
					   identifer.quote='\'') )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connexion a la base Xair.')
	} else if(getOption('Xair.drv') == 'oracle') {
		loadNamespace ('ROracle')
		drv <- ROracle::Oracle()
		conxair <- try (dbConnect (drv, username=getOption('Xair.uid'),
					   password=getOption('Xair.pwd'),
					   sprintf('%s:1521/%s', getOption('Xair.host'),
 						   getOption('Xair.dsn')) ) )
		if(inherits(conxair, 'try-error'))
			stop('echec de la connexion a la base Xair.')
	} else {
		stop('platforme non reconnue')
	}
	return (conxair)
}

