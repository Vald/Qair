match.pattern.fields <- function (pattern, search.fields) {
	pattern <- gsub ('_', '!_', pattern)
	sprintf ('(%s)',
		 paste ('(', rep (search.fields, each = length(pattern) ),
			" LIKE '%", pattern, "%' ESCAPE '!')",
			sep = '', collapse = ' OR ') )
}

## pour s'inspirer quand on fera du TS*DataFrame
# print.TimeIntervalDataFrame <- function (x, ...) {
#         to.print <- data.frame (attributes(x)$dates, unclass (x), stringsAsFactors=FALSE)
#         to.print <- data.frame (lapply (to.print, function(x) ifelse (is.na(x), 'NA', as.character(x) ) ), stringsAsFactors=FALSE)
# 
#         to.print.tmp <- attributes(x)$mesures
#         to.print.tmp <- data.frame (start='', end=c('', names(to.print.tmp)),
#                                     t(data.frame (rep ('', nrow(to.print.tmp) ), to.print.tmp@data, stringsAsFactors=FALSE) ),
#                                     stringsAsFactors=FALSE)
#         rownames (to.print.tmp) <- NULL
#         to.print <- rbind (to.print, to.print.tmp, stringsAsFactors=FALSE)
# 
#         to.print.tmp <- coordinates(attributes(x)$mesures)
#         to.print.tmp <- data.frame (start = '',
#                                     end = c('', colnames(to.print.tmp) ),
#                                     t(data.frame (rep('', nrow(to.print.tmp) ), to.print.tmp) ),
#                                     stringsAsFactors=FALSE)
#         rownames (to.print.tmp) <- NULL
#         colnames (to.print.tmp) <- names (to.print)
#         to.print <- rbind (to.print, to.print.tmp, stringsAsFactors=FALSE)
# 
#         print (to.print)
# }



# a deplacer dans un fichier zzz.R
qh <- new_period (minutes=15)
h <- new_period (hours=1)
#---------------------------------

xrGetSitesPrelevement <- function(conn, pattern = NULL, search.fields = c('IDSITEP', 'LIBELLE'),
			  campagnes = NULL, fields = NULL, collapse = c('AND', 'OR')) {
	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- '*'
	
	query <- sprintf ('SELECT %s FROM', paste ('SITE_PRELEVEMENT', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'SITE_PRELEVEMENT'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

	if (!is.null (campagnes) ) {
		q$campagnes <- unique (xrGetCampagnes (conn, pattern = campagnes)$NOM_COURT_CM)
		if (length(q$campagnes) == 0) q$campagnes <- NULL else {
			q$tables <- c(q$tables, 'CAMPMES_SITE_P')
			q$campagnes <- sprintf(
				'SITE_PRELEVEMENT.NSIT=CAMPMES_SITE_P.NSIT AND CAMPMES_SITE_P.NOM_COURT_CM IN (%s)',
				paste ("'", q$campagnes, "'", sep = '', collapse = ", ") )
		}
	}

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	sites <- unique (xrGetQuery (conn, query) )
	sites
}

xrGetMethodesPrelevement <- function(conn, pattern = NULL, search.fields = c('LIBELLE'),
			  fields = NULL, collapse = c('AND', 'OR')) {
	collapse <- match.arg (collapse)
	collapse <- sprintf (' %s ', collapse)

	if (is.null (fields) ) fields <- '*'
	
	query <- sprintf ('SELECT %s FROM', paste ('METH_PRELEVEMENT', fields, sep='.', collapse=', ') )
	q <- list()
	q$tables <- 'METH_PRELEVEMENT'

	if (!is.null (pattern) & length (search.fields) > 0)
		q$pattern <- match.pattern.fields (pattern, search.fields)

	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	methodes <- unique (xrGetQuery (conn, query) )
	methodes
}

# conn <- xrConnect()
# start <- '2011-01-01'
# end <- '2012-01-01'
# sites <- c('COGN_', 'ANGO')
# valid.states <- c("A", "R", "O", "W")
# polluants <- 'NO2'
# campagnes <- methodes <- NULL

xrGetManualData <-
	function (conn, start, end, sites=NULL, polluants=NULL, methodes=NULL,
	   	  valid.states = c("A", "R", "O", "W"), what = c('value', 'state', 'both'),
		  campagnes = NULL) {
	# start et end doivent être des POSIXt (pour la prise en compte des timezones).
	what <- match.arg (what)

	# pour permettre eventuellement d'entrer des chaines de caracteres en start en end
	#	on fait un petit cast
	if (!is.POSIXt (start) ) start <- as.POSIXct (start, tz = 'UTC')
	if (!is.POSIXt (end) ) end <- as.POSIXct (end, tz = 'UTC')

	# recuperation des noms de sites, de polluants, de methode de prelevement
	q <- list ()
	
	if (!is.null (sites) ) {
		sites <- xrGetSitesPrelevement (conn, sites, campagnes=campagnes)
		q$sites <- paste ("'", sites$NSIT, "'", sep='', collapse=', ')
	}
	if (!is.null (polluants) ) {
		polluants <- xrGetPolluants (conn, polluants)
		q$polluants <- paste ("'", polluants$NOPOL, "'", sep='', collapse=', ')
	}
	if (!is.null (methodes) ) {
		methodes <- xrGetMethodesPrelevement (conn, methodes)
		q$methodes <- paste ("'", methodes$CODE_METH_P, "'", sep='', collapse=', ')
	}
	# mise en forme des dates pour la requete
	q$start <- format (start, format = '%Y-%m-%d', tz='UTC')
	q$start <- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$start)
	q$end	<- format (end, format = '%Y-%m-%d', tz='UTC')
	q$end	<- sprintf ("TO_DATE('%s', 'YYYY-MM-DD')", q$end)
	q$valid.states <- paste ("'", valid.states, "'", sep='', collapse=', ')
	
	# création de LA requete de rappatriement
	query <- sprintf (
	"SELECT SITE_PRELEVEMENT.LIBELLE site, LONGI, LATI, LAMBERTX, LAMBERTY, METH_PRELEVEMENT.LIBELLE methode,
		VALEUR, CODE_QUALITE, UNITE, NOPOL, CCHIM,
		PRELEVEMENT.DATE_DEB, PRELEVEMENT.DATE_FIN
	FROM SITE_METH_PRELEV
		JOIN SITE_PRELEVEMENT USING (NSIT)
		JOIN METH_PRELEVEMENT USING (CODE_METH_P)
		JOIN PRELEVEMENT USING (CODE_SMP)
		JOIN ANALYSE USING (CODE_SMP, CODE_PRELEV)
		JOIN MESURE_LABO USING (CODE_SMP, CODE_MES_LABO)
		JOIN NOM_MESURE USING (NOPOL)
	WHERE (PRELEVEMENT.DATE_DEB>=%s AND PRELEVEMENT.DATE_FIN<=%s) AND
		CODE_QUALITE IN (%s)", q$start, q$end, q$valid.states)

	if (!is.null(q$sites)) query <- sprintf ("%s AND NSIT IN (%s)", query, q$sites)
	if (!is.null(q$polluants)) query <- sprintf ("%s AND NOPOL IN (%s)", query, q$polluants)
	if (!is.null(q$methodes)) query <- sprintf ("%s AND CODE_METH_P IN (%s)", query, q$methodes)

	# recuperation des données
	result <- xrGetQuery (conn, query)
	
	# mise en forme des données
	result <- split (result, paste (result$DATE_DEB, result$DATE_FIN) )
	fun.tmp <- function(x, what) {
		debut <- unique (x$DATE_DEB)
		fin <- unique (x$DATE_FIN)
		valeurs <- split (x[c('VALEUR', 'CODE_QUALITE')],
				  gsub(' ', '.', paste (x$SITE, x$METHODE, x$CCHIM, sep='.')) )
		if (any (sapply (valeurs, nrow) > 1) ) {
			warning (sprintf ("\n%s, \nCertains prélèvements sont dupliqués. L'association entre les différentes mesures risque d'être arbitraire.", paste (names(valeurs)[sapply (valeurs, nrow) > 1]) ) )
		
			for (i in which (sapply (valeurs, nrow) > 1))
				for (j in nrow (valeurs[[i]]) )
					valeurs[[paste(names(valeurs)[i], j, sep='.')]] <-
						valeurs[[i]][j, , drop=FALSE]
			valeurs[which (sapply (valeurs, nrow) > 1)] <- NULL
		}
		ret <- data.frame (start=debut, end=fin)
		if (what=='value')
			ret <- data.frame (ret, lapply (valeurs, function(x) x$VALEUR) ) else
		if (what=='state')
			ret <- data.frame (ret, lapply (valeurs, function(x) x$CODE_QUALITE) ) else {
			noms <- names (valeurs)
			ret <- data.frame (ret, lapply (valeurs, function(x) x$VALEUR),
					   lapply (valeurs, function(x) x$CODE_QUALITE) )
			names(ret)[-(1:2)] <- paste (rep (noms, 2),
						     rep (c('value', 'state'), each=length(noms)),
						     sep='.')
			}

		return (ret)
	}
	result <- lapply (result, fun.tmp, what)
	while (length (result) > 1) {
		result[[1]] <- merge (result[[1]], result[[2]], all=TRUE)
		result[[2]] <- NULL
	}
	result <- result[[1]]

	result <- new ('TimeIntervalDataFrame',
		       start=as.POSIXct(result$start, 'UTC'),
		       end=as.POSIXct(result$end, 'UTC'),
		       timezone='UTC', data=result[-(1:2)])
	return (result)
}


