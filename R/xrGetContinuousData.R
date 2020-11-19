#' Recuperation de mesure continue dans une base XR
#'
#' La fonction permet de récupérer une ou plusieurs séries 
#' de données stockée(s) dans une base XR entre 2 dates données
#' au format voulu (heure, mois, etc.). Les données sont retournées
#' au format \code{\link[timetools]{TimeIntervalDataFrame-class}} défini dans le 
#' paquet \code{\link[timetools]{timetools}}.
#'
#' L'extraction des séries de données peut être réalisée en recherchant
#' une chaîne de caractères (définie par \code{pattern}) sur 
#' un ou plusieurs champs de la table \code{MESURE} (la fonction 
#' \code{\link{xrGetMesures}} est alors utilisée). Les champs peuvent
#' être précisés par l'argument \code{search.fields}.
#'
#' Il est également possible de récupérer plusieurs séries de données
#' à partir des réseaux, campagnes, stations ou polluants définis dans XR.
#' Il suffit pour
#' cela d'indiquer une ou plusieurs chaînes dans les arguments
#' \code{campagnes}, \code{reseaux}, \code{stations} ou \code{polluants}.
#' Ces arguments sont alors utilisés dans les fonctions \code{xrGet*}
#' adéquates, soit respectivement \code{\link{xrGetCampagnes}}, \code{\link{xrGetReseaux}},
#'  \code{\link{xrGetStations}} et  \code{\link{xrGetPolluants}}.
#'
#' Si plusieurs de ces arguments sont renseignés, la conjonction entre les
#' différents critères est précisée par l'argument \code{collapse}.
#'
#' La timezone des dates de début et de fin (paramètres debut et fin)
#' est prise en compte dans le rappatriement des données. Il faut donc être
#' attentif à la bonne cohésion entre ces dates et les données attendues. Par
#' exemple les données journalières sont stockées en 'UTC' dans XR. Le fait
#' de mettre des dates en 'CET' risque d'aboutir à la récupération inappropriée
#' des données : si fin = POSIXct('2016-02-01', 'CET'), les dernières
#' valeurs récupérées seront celles qui s'arrêtent avant cette date (soit
#' POSIXct('2016-01-01 23:00:00', 'UTC'), c'est-à-dire celles qui s'arrêtent
#' au POSIXct('2016-01-01', 'UTC') alors qu'on peut penser que 
#' seraient attendues celles qui s'arrêtent au POSIXct('2016-02-01', 'UTC').
#'
#' @param conn une connexion valide telle que retournée par \code{\link{xrConnect}}.
#' @param pattern chaînes de caractères utilisées pour la recherche (cf \sQuote{Details}).
#' @param start date initiale de la période à rapatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param end date de fin de la période à rapatrier (au format \code{\link[base]{POSIXct}} ou character
#'	pouvant être converti en \code{\link[base]{POSIXct}}).
#' @param period un des éléments suivant ('h', 'qh', 'd', 'm', 'y') précisant la période
#' 	des données à rapatrier.
#' @param validated booléen. Si TRUE, les données sont récupérées dans la table normale
#'	de stockage de XR ; si FALSE, les données sont récupérées dans la table \code{BRUTE}.
#' @param valid.states liste des codes états d'XR à considérer comme valide. Par défaut
#'	les données dont le code état est'A', 'R', 'O', 'W' ou 'P' sont considérées comme valides.
#'	les autres sont remplacées par NA.
#' @param what un des éléments suivant ('value', 'state', 'validated'). value : seules
#'	les valeurs sont récupérées ; state : seuls les codes états sont récupérés ;
#'	validated : seuls les codes validation sont récupérés ; un mélange des trois :
#'  les infos correspondantes sont récupérées. Pour assurer une rétrocompatiblité 
#'  avec la v2 de Qair, 'both' est également accepté. Il est équivalent à mettre
#'  'value' et 'state'. validated : 1 adval, 2 technique, 3 environnemental.
#' @param search.fields champ de la table dans lesquels \code{pattern} doit être
#' 	recherché.
#' @param campagnes chaînes de caractères correspondant aux campagnes à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetCampagnes}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetCampagnes Si c'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetCampagnes
#'   pattern doit alors être précisé :
#'
#'  \code{... list(pattern='POITIERS_003', search.fields='NOM_COURT_CM') ...}
#' @param reseaux chaînes de caractères correspondant aux réseaux à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetReseaux}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetResaux. Si c'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetReseaux.
#'   pattern doit alors est précisé :
#'
#'  \code{... list(pattern='R-O3', search.fields='NOM_COURT_RES') ...}
#' @param stations chaînes de caractères correspondant aux stations à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetStations}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetStations. Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetStations.
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 'VERDUN', search.fields = 'IDENTIFIANT') ...}
#' @param polluants chaînes de caractères correspondant aux polluants à rapatrier
#' 	(optionnel) (utilisé via la fonction\code{\link{xrGetPolluants}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetPolluants. Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetPolluants.
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 'O3', search.fields = 'CCHIM') ...}
#' @param collapse conjonction à appliquer entre les différents critères de recherche
#'	indiqués.
#' @param XR6 TRUE si la version de XR est supérieure ou égale à 6, FALSE sinon.
#'  Obsolète : n'est plus utilisé.
#' @param tz timezone dans laquelle les données doivent être retournées. Si les dates
#' 	debut et fin sont des chaînes de caractères, tz est utilisé pour les définir.
#' @param cursor détermine le format de sortie des données : NULL pour un 
#' 	TimeIntervalDataFrame (par défaut) une valeur entre 0 et 1 pour un 
#' 	TimeInstantDataFrame (1 pour respecter les conventions XR).
#' @param exact booléen indiquant si les mesures à rapatrier doivent être 
#' 	exactement identiques à \sQuote{pattern} ou si \sQuote{pattern} doit 
#' 	être utilisé dans une expression régulière. Dans le cas d'une connexion v3, 
#'  exact=TRUE prendra en compte les symboles \sQuote{pourcent} et
#'  \sQuote{point d'interrogation} (comme dans la doc de l'API), 
#'  exact=FALSE fera une recherche 'large' exactement comme pour la v2
#'  (concrètement en ajoutant \sQuote{pourcent} avant et après la chaîne recherchée).
#' @param validOnly (v3) La recherche doit-elle porter uniquement sur les
#'  mesures ouvertes ?
#' @param silent Les messages doivent-ils être affichés ? Par défaut FALSE.
#'  Peut être gérée avec l'option Xair.silent. L'ordre d'utilisation est donc
#'  le suivant : si une valeur est renseignée à l'appel de la fonction, elle est 
#'  utilisée ; sinon si l'option Xair.silent est définie, elle est utilisée ;
#'  sinon la valeur FALSE est utilisée.
#'
#' @return un objet de classe \code{\link[timetools]{TimeIntervalDataFrame-class}}
#' 	contenant les données demandées.
#'
#' @seealso \code{\link{xrGetMesures}}, \code{\link{xrGetCampagnes}}, \code{\link{xrGetReseaux}},
#'  \code{\link{xrGetStations}}, \code{\link{xrGetPolluants}}, \code{\link[timetools]{TimeIntervalDataFrame-class}}
#'	\code{\link{xrGetManualData}}

xrGetContinuousData <- function (conn, pattern=NULL, start, end,
		       period = c('h', 'qh', 'd', 'm', 'y', 'scan'),
		       validated = TRUE, valid.states = c("A", "R", "O", "W", "P"),
			   what = c('value', 'state', 'both', 'validated'),
		       search.fields=NULL, campagnes = NULL, reseaux = NULL, stations = NULL,
			   polluants = NULL,
		       collapse = c('AND', 'OR'), XR6 = TRUE,
		       tz='UTC', cursor=NULL, exact=FALSE, validOnly=FALSE, silent) {

	# initialisation ----------------------------------------------------------
	if(missing(silent)) silent <- getOption('Xair.silent', FALSE)
	osaf     <- getOption('stringsAsFactors')
	options(stringsAsFactors = FALSE)

	period   <- match.arg (period)
	collapse <- match.arg (collapse)

	if(missing(what)) what <- 'value'
	what     <- match.arg (what, several.ok = TRUE)
	# Pour maintenir la compatibilité avec la v2, 'both' est accepté.
	# On remplace juste 'both' par 'value' et 'state'
	if('both' %in% what) {
		what <- c('value', 'state', what)
		what <- setdiff(what, 'both')
		what <- unique(what)
		warning("L'argument 'what' est obsolète et remplacé par la combinaision c('value', 'state')")
	}

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('v2/data?showDbRowIds=true&')

	# traitement des dates de debut et de fin ---------------------------------
	# si debut et fin ne sont pas en POSIXct, conversion

	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	# recuperation des noms de mesures à chercher -----------------------------

	mesures <- xrGetMesures(conn = conn,
							pattern = pattern, search.fields = search.fields,
							campagnes = campagnes,
							reseaux   = reseaux,
						   	stations  = stations,
							polluants = polluants,
							collapse  = collapse, exact = exact,
							validOnly = validOnly, resv3 = TRUE,
							silent    = silent)

	# récuperation des données ------------------------------------------------
	
	# si aucune mesure ne correspond aux critères, un tableau vide est
	# directement créé, sinon go

	# nombre de secondes dans la période demandée
	nbsbp <- switch(period,
					h =60*60,
					qh=60*15,
					d =60*60*24,
					m =60*60*24*31,
					y =60*60*24*366,
					scan=10)
	limsupdata <- 100000
	limsupmes  <- 10

	if (nrow(mesures) == 0) {
		donnees <- TimeIntervalDataFrame(character(0), character(0), 'UTC')
	} else if(nrow(mesures) > limsupmes) {
		# si la requete concerne plus de 500 mesures, on fait du 
		# récusrif

		donnees <- xrGetContinuousData(conn=conn,
				start=start, end=end, period=period,
				validated=validated, valid.states=valid.states, what=what,
				pattern=mesures[['dbRowId']][1:limsupmes], search.fields='dbRowId', exact=TRUE,
				validOnly=FALSE, silent=silent)

		donnees <- merge(donnees, xrGetContinuousData(conn=conn,
				start=start, end=end, period=period,
				validated=validated, valid.states=valid.states, what=what,
				pattern=mesures[['dbRowId']][-(1:limsupmes)], search.fields='dbRowId', exact=TRUE,
				validOnly=FALSE, silent=silent))

	} else if(nrow(mesures)*difftime(end, start, units='secs')/nbsbp  > limsupdata) {
		# si la requete concerne plus de 1 million de mesure idem
		datesplit <- trunc(start + POSIXctp(limsupdata*nbsbp/nrow(mesures), 'second'), 'day')
		if(getOption('Xair.debug', FALSE)) message(datesplit)

		if(requireNamespace('parallel', quietly=TRUE)) {
			if(getOption('Xair.debug', FALSE)) message("requêtage parallèle de l'API")

			dates <- seq(start, end, by=paste(limsupdata*nbsbp/nrow(mesures), 'sec'))
			dates <- unique(c(dates, end))
			if(getOption('Xair.debug', FALSE)) message("découpage des dates: ", dates)

			nbattempt <- getOption('Xair.nbattempt', 10)
			options(Xair.nbattempt=100)

			donnees <- parallel::mcmapply(
				SIMPLIFY=FALSE,
				s = dates[-length(dates)],
				e = dates[-1],
				function(s, e) {
					xrGetContinuousData(conn=conn,
							start=s, end=e, period=period,
							validated=validated, valid.states=valid.states, what=what,
							pattern=mesures[['dbRowId']], search.fields='dbRowId',
							exact=TRUE,
							validOnly=FALSE, silent=silent)
				})

			options(Xair.nbattempt=nbattempt)

			donnees <- do.call(rbind.TimeIntervalDataFrame, donnees)

		} else {

			donnees <- xrGetContinuousData(conn=conn,
					start=start, end=datesplit, period=period,
					validated=validated, valid.states=valid.states, what=what,
					pattern=mesures[['dbRowId']], search.fields='dbRowId', exact=TRUE,
					validOnly=FALSE, silent=silent)

			donnees <- merge(donnees, xrGetContinuousData(conn=conn,
					start=datesplit, end=end, period=period,
					validated=validated, valid.states=valid.states, what=what,
					pattern=mesures[['dbRowId']], search.fields='dbRowId', exact=TRUE,
					validOnly=FALSE, silent=silent))
		}

	} else {

		# start et end sont mis en forme pour la requete ----------------------

		dformat <- '%Y-%m-%dT%H:%M:%SZ'
		from    <- format (start, format = dformat, tz='UTC')
		to      <- format (end+POSIXctp('second'), format = dformat, tz='UTC')

		# la requete a proprement parler --------------------------------------

		dataTypes <- switch(period,
							h ='hourly',
							qh='sta',
							d ='daily',
							m ='monthly',
							y ='annual',
							scan='fld')

		query <- sprintf('%sfrom=%s&to=%s&dbRowIdOfMeasures=%s&dataTypes=%s&includeRaw=%s',
						 bquery, from, to,
						 paste (mesures[['dbRowId']], collapse=','),
						 dataTypes,
						 ifelse(validated,'false','true'))

		donnees <- xrGetQuery(conn, query, resv3=TRUE)

		#----------------------------------------------------------------------
		# mise en forme des donnees -------------------------------------------

		if(!validated)
			what <- sub('value', 'rawValue', sub('state', 'rawState', what))
		
		# conservation des colonnes voulues -----------------------------------

		donnees <- lapply(mesures[['dbRowId']], function(drid) {
				i  <- which(donnees[['dbRowId']] == drid)
				r  <- donnees[[dataTypes]][['data']][[i]]
				id <- mesures[['id']][i]

				# remplacement des valeurs non-valides par NA
				r[['value']][!r[['state']] %in% valid.states] <- NA
				
				# quand toutes les colonnes ne sont pas dans le resultat
				r <- r[c('date', intersect(what, names(r)))]
				r[setdiff(what, names(r))] <- NA
				r <- r[c('date', what)]

				# nommage des colonnes
				if(length(what) > 1)
					names(r) <- c('date', paste(sep='.', id, what)) else
					names(r) <- c('date', id)
				return(r)
		})

		# concaténation en une seule data.frame avec une colonne de date ------

		while(length(donnees) > 1) {
			donnees[[1]] <- merge(donnees[[1]], donnees[[2]], all=TRUE)
			donnees[[2]] <- NULL
		}
		donnees <- donnees[[1]]

		# formatage  des dates et création de la TimeIntervalDataFrame --------

		donnees[['date']] <- strptime(donnees[['date']], dformat, 'UTC')
		donnees[['date']] <- as.POSIXct(donnees[['date']])

		pas <- switch(period,
					  h = POSIXctp('hour'),
					  qh= POSIXctp(15, 'minute'),
					  d = POSIXctp('day'),
					  m = POSIXctp('month'),
					  y = POSIXctp('year'),
					  scan=POSIXctp(10, 'second'))

		donnees <- TimeIntervalDataFrame(
			start = donnees[['date']] - pas,
			end   = donnees[['date']],
			data  = donnees[setdiff(names(donnees), 'date')])

	}

	if( !is.null(cursor) ) donnees <- as.TimeInstantDataFrame(donnees, cursor)
	timezone(donnees) <- tz

	options(stringsAsFactors = osaf)
	return (donnees)
}



