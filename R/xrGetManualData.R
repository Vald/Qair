# FIXME: lors de l'adaptation à l'API, voir si traitement possible
# des données QAI

#' Fonction pour recuperer des donnees manuelles de XR
#'
#' @param polluants chaînes de caractères correspondant aux polluants à rapatrier
#'   (optionnel) (utilisé via la fonction\code{\link{xrGetPolluants}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetPolluants. Si c'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetPolluants.
#'   pattern doit alors est précisé :
#'
#'  \code{... list(pattern='DF', search.fields='NOPOL') ...}
#' @param sites chaînes de caractères correspondant aux sites à rapatrier
#'   (optionnel) (utilisé via la fonction\code{\link{xrGetSitesPrelevement}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetSitesPrelevement. Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetSitesPrelevement.
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 618, search.fields = 'NSIT') ...}
#' @param methodes chaînes de caractères correspondant aux campagnes à rapatrier
#'   (optionnel) (utilisé via la fonction\code{\link{xrGetMethodesPrelevement}}).
#' @param categories vecteur indiquant les categories des mesures à rapatrier.
#'   combinaison quelconques des valeurs '0', '1', '2', '3' ou '4' avec la
#'   correspondance suivante (attention ce doit être des chaînes de caractères) :
#'  \describe{
#'     \item{'0'}{analyse ;}
#'     \item{'1'}{contrôle ;}
#'     \item{'2'}{blanc terrain ;}
#'     \item{'3'}{blanc laboratoire ;}
#'     \item{'4'}{blanc transport.}
#'   }
#'   Par défaut, toutes les catégories de mesures sont récupérées.
#' @param valid.states liste des codes états d'XR à considérer comme valide. Par défaut
#'	les données dont le code état est 'A', 'L', 'U', 'W' sont considérées comme valides
#'  et sont intégrées au tableau de données pour les données manuelles.
#'  Les codes 'I', 'l' et 'u' correspondent aux données 'invalides'.
#' @inheritParams xrGetContinuousData
#'
#' @return un objet de classe \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'   contenant les données demandées.
#'
#' @seealso \code{\link{xrGetSitesPrelevement}}, \code{\link{xrGetMethodesPrelevement}},
#'	 \code{\link{xrGetCampagnes}}, \code{\link{xrGetPolluants}}
#' 	\code{\link[timetools]{TimeIntervalDataFrame-class}}

xrGetManualData <-
	function (conn, start, end, sites=NULL, polluants=NULL, methodes=NULL,
	   	  valid.states = c("A", "L", "U", "W"),
		  what = c('value', 'state', 'both'),
		  campagnes = NULL, tz='UTC', cursor=NULL,
		  categories=as.character(0:4), silent) {

	# initialisation ----------------------------------------------------------
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)
	osaf     <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)

	what <- match.arg (what)
	categories <- match.arg(categories, several.ok=TRUE)

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/sampling?')

	# traitement des dates de debut et de fin ---------------------------------
	# si debut et fin ne sont pas en POSIXct, conversion

	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	# start et end sont mis en forme pour la requete --------------------------

	dformat <- '%Y-%m-%dT%H:%M:%SZ'
	from    <- format (start, format = dformat, tz='UTC')
	to      <- format (end+POSIXctp('second'), format = dformat, tz='UTC')

	query <- sprintf('%sfrom=%s&to=%s&', bquery, from, to)

	# recherche sur sites et campagnes ----------------------------------------

	if (!is.null (sites)) {
		if( !is.list(sites) )
			sites <- xrGetSitesPrelevement (conn, sites, campagnes=campagnes,
											resv3=TRUE, silent=silent) else {
			sites[['campagnes']] <- unique(c(sites[['campagnes']], campagnes))
			sites[['resv3']]     <- TRUE
			sites <- do.call(xrGetSitesPrelevement,
							 c(list(conn=conn), sites, silent=silent))
		}
		if(nrow(sites) > 0) query <- paste0(
			query, 'idSamplingSite=',
			paste0(sites[['identifier']], collapse=','),
			'&')

	} else if (!is.null(campagnes)) {
		sites <- xrGetSitesPrelevement (conn, campagnes=campagnes, resv3=TRUE)
		if(nrow(sites) > 0) query <- paste0(
			query, 'idSamplingSite=',
			paste0(sites[['idSamplingSite']], collapse=','),
			'&')
	}

	# recherche sur polluants -------------------------------------------------

	if (!is.null (polluants)) {
		if( !is.list(polluants) )
			polluants <- xrGetPolluants(conn, pattern = polluants,
										resv3=TRUE, silent=silent) else{
			polluants[['resv3']] <- TRUE
			polluants <- do.call(xrGetPolluants,
								 c(list(conn=conn), polluants, silent=silent))
		}
		if(nrow(polluants) > 0) query <- paste0(
			query, 'nopol=',
			paste0(polluants[['id']], collapse=','),
			'&')
	}

	# recherche sur methodes --------------------------------------------------

	if (!is.null (methodes)) {
		if( !is.list(methodes) )
			methodes <- xrGetMethodesPrelevement(conn, pattern = methodes,
										resv3=TRUE, silent=silent) else{
			methodes[['resv3']] <- TRUE
			methodes <- do.call(xrGetMethodesPrelevement,
								c(list(conn=conn), methodes, silent=silent))
		}
		if(nrow(methodes) > 0) query <- paste0(
			query, 'codeSamplingMethod=',
			paste0(methodes[['samplingMethod']], collapse=','),
			'&')
	}

	# exécution de la requête et préparation des données ----------------------

	donnees <- xrGetQuery(conn, query, resv3=TRUE)

	donnees <- donnees[setdiff(names(donnees), 'address')]

	donnees <- lapply(donnees, '[', donnees[['category']] %in% categories)
	donnees <- lapply(donnees, '[', !sapply(donnees[['laboMeasures']], is.null))

	donnees[['laboMeasures']] <- lapply(donnees[['laboMeasures']], function(x)
						return(x[x[['qc']] %in% valid.states, , drop=FALSE]))
	
	donnees <- lapply(donnees, '[', sapply(donnees[['laboMeasures']], nrow) > 0)


	nrows <- sapply(donnees[['laboMeasures']], nrow)

	donnees <- data.frame(
				VALEUR      = unlist(lapply(donnees[['laboMeasures']], '[[', 'value')),
				CODE_QUALITE= unlist(sapply(donnees[['laboMeasures']], '[[', 'qc')),
				SITE        = rep(donnees[['samplingSiteLabel']], nrows),
				METHODE     = rep(donnees[['samplingMethod']], nrows),
				NOPOL       = unlist(lapply(donnees[['laboMeasures']], '[[', 'nopol')),
				DATE_DEB    = as.POSIXct(rep(donnees[['startEpoch']], nrows)),
				DATE_FIN    = as.POSIXct(rep(donnees[['stopEpoch']], nrows)))


	# mise en forme des données -----------------------------------------------

	donnees <- split (donnees, paste(donnees[['DATE_DEB']], donnees[['DATE_FIN']]))

	fun.tmp <- function(x, what) {
		debut <- unique (x[['DATE_DEB']])
		fin   <- unique (x[['DATE_FIN']])

		valeurs <- split (
			x[c('VALEUR', 'CODE_QUALITE')],
			gsub(' ', '.', do.call(paste, c(x[c('SITE', 'METHODE', 'NOPOL')], sep='.'))))

		if( any(sapply(valeurs, nrow) > 1) ) {
			warning("Certains prélèvements sont dupliqués.",
					" L'association entre les différentes mesures risque d'être arbitraire.")
			warning(paste(names(valeurs)[sapply(valeurs, nrow) > 1], sep='\n'))
		
			for (i in which(sapply(valeurs, nrow) > 1))
			for (j in 1:nrow(valeurs[[i]]))
				valeurs[[paste(names(valeurs)[i], j, sep='.')]] <- valeurs[[i]][j, , drop=FALSE]

			valeurs[which(sapply (valeurs, nrow) > 1)] <- NULL
		}

		ret <- data.frame(start=debut, end=fin)

		if (what=='value')
			ret <- data.frame(
				ret,
				lapply(valeurs, function(x) x[['VALEUR']])) else
		if (what=='state')
			ret <- data.frame(
				ret,
				lapply(valeurs, function(x) x[['CODE_QUALITE']])) else{
			noms <- names (valeurs)
			ret <- data.frame(
				ret, lapply (valeurs, function(x) x[['VALEUR']]),
				lapply (valeurs, function(x) x[['CODE_QUALITE']]))
			names(ret)[-(1:2)] <- paste(
				rep (noms, 2),
				rep (c('value', 'state'), each=length(noms)),
				sep='.')
		}

		return (ret)
	}

	donnees <- lapply (donnees, fun.tmp, what)

	while (length (donnees) > 1) {
		donnees[[1]] <- merge (donnees[[1]], donnees[[2]], all=TRUE)
		donnees[[2]] <- NULL
	}

	if (length(donnees) == 0) {
		donnees <- TimeIntervalDataFrame(character(0), character(0), 'UTC')

	} else {
		donnees <- donnees[[1]]

		donnees <- new ('TimeIntervalDataFrame',
				   start   = as.POSIXct(donnees[['start']], 'UTC'),
				   end     = as.POSIXct(donnees[['end']], 'UTC'),
				   timezone= 'UTC',
				   data    = donnees[-(1:2)])
	}

	if( !is.null(cursor) )
		donnees <- as.TimeInstantDataFrame(donnees, cursor)
	timezone(donnees) <- tz

	return (donnees)
}

