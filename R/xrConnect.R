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
#' La connexion directe à la base de données ne sera initialisée que si drv.type
#' est non-nul (les arguments correspondant ne seront donc pris en compte que dans 
#' ce cas).
#'
#' @param host Adresse de l'hôte hébergeant l'API-REST et
#'  la base de données. Pour la base de données, nécessaire uniquement
#'	pour les systèmes type unix, utilisant le pilote JDBC ou Oracle.
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
#' @param dsn Nom de la base de données (cf le pilote concerné, JDBC, ODBC ou Oracle).
#'	optionnel : sera demandé si nécessaire.
#' @param uid Identifiant utilisé pour la connexion à la base de données.
#'	optionnel : sera demandé si nécessaire.
#' @param pwd Mot de passe pour initialiser la connexion à la base de données.
#'	optionnel : sera demandé si nécessaire.
#' @param ojdbc Emplacement de la librairie contenant la définition des classes
#'	nécessaires à RJDBC. Uniquement dans le cas où jdbc est choisi comme driver.
#' @param drv.type chaine de caractere indiquant le driver de base de données à utiliser.
#' 	peut prendre les valeurs 'oracle', 'jdbc' ou 'odbc'. Par défault, la valeur utilisée
#' 	est 'oracle' pour les systèmes type 'unix' et 'odbc' pour windows.
#' @inheritParams RODBC::odbcConnect
#' @inheritParams xrGetContinuousData
#' @return Une connexion à la base XR (il s'agit en fait d'une liste avec 'host' et
#'  'port'. Cela permet de gérer plusieurs 'connexion' simultanément et de mimer
#'  le comportement de Qair2).
#'
#' @aliases options, Xair.host, Xair.port, Xair.uid, Xair.pwd, Xair.host, Xair.dsn
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
					  dsn=NULL, uid=NULL, pwd=NULL, ojdbc=NULL, drv.type=NULL,
					  believeNRows=TRUE, silent) {
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

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption('Xair.uid')) && missing(uid)) {
		cat('identifiant pour la connexion :\n')
		options(Xair.uid=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption('Xair.pwd')) && missing(pwd)) {
		cat('mot de passe pour la connexion :\n')
		options(Xair.pwd=scan(what='character', nlines=1))
		cat('\n')
	}

	# conservation de l'accès à la base de données pour les données manuelles -

	conxair <- NULL
	# définition du pilote à utiliser
	if (is.null(drv.type)) drv.type <- getOption('Xair.drv')
	if (!is.null(drv.type)) {
		options(Xair.drv=drv.type)

		if (!getOption('Xair.drv') %in% c('odbc', 'jdbc', 'oracle'))
			stop("le pilote pour la connexion à la base doit être 'oracle', 'odbc' ou 'jdbc'")

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
			conxair <- try (RODBC::odbcConnect (getOption('Xair.dsn'),
							 uid = getOption('Xair.uid'),
							 pwd = getOption('Xair.pwd'),
							 case = 'nochange', believeNRows = believeNRows) )
			if(inherits(conxair, 'try-error'))
				stop('echec de la connexion a la base Xair.')
		} else if(getOption('Xair.drv') == 'jdbc') {
			drv <- RJDBC::JDBC('oracle.jdbc.OracleDriver', getOption('Xair.ojdbc.file'))
			conxair <- try (DBI::dbConnect (drv,
						   paste ('jdbc:oracle:thin:@', getOption('Xair.host'),
							  ':1521:', getOption('Xair.dsn'), sep=''),
						   getOption('Xair.uid'),
						   getOption('Xair.pwd'),
						   identifer.quote='\'') )
			if(inherits(conxair, 'try-error'))
				stop('echec de la connexion a la base Xair.')
		} else if(getOption('Xair.drv') == 'oracle') {
			drv <- ROracle::Oracle()
			conxair <- try (DBI::dbConnect (drv, username=getOption('Xair.uid'),
						   password=getOption('Xair.pwd'),
						   sprintf('%s:1521/%s',
								   getOption('Xair.host'),
								   getOption('Xair.dsn')) ) )
			if(inherits(conxair, 'try-error'))
				stop('echec de la connexion a la base Xair.')
		} else {
			stop('platforme non reconnue')
		}
	}

	#--------------------------------------------------------------------------

	xr <- list(host   = options()[['Xair.host']],
			   port   = options()[['Xair.port']],
			   version= options()[['Xair.version']],
			   db     = conxair)

	if(!is.null(getOption('Xair.uid')) & !is.null(getOption('Xair.pwd')))
		xr[['logged']] <- status_code(RETRY(
				'POST',
				xrGetUrl(xr, authentification=TRUE),
				httr::config(ssl_verifypeer=FALSE,ssl_verifyhost=FALSE),
				encode='form', terminate_on=400,
				body=list(username=getOption('Xair.uid'), password=getOption('Xair.pwd')))) == 200 else
		xr[['logged']] <- FALSE

	class(xr) <- 'xr'

	if(nrow(suppressMessages(xrGetStations(xr))) == 0) stop('Problème de connexion')

	return (xr)
}

