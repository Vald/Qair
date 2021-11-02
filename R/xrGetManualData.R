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
		  categories=as.character(0:4)) {

	what <- match.arg (what)
	categories <- match.arg(categories, several.ok=TRUE)

	# pour permettre eventuellement d'entrer des chaines de caracteres en
	# start en end
	#	on fait un petit cast
	if( inherits(start, 'POSIXlt') ) start <- as.POSIXct(start)
	if( inherits(end, 'POSIXlt') ) end <- as.POSIXct(end)

	if( !inherits(start, 'POSIXct') ) start <- as.POSIXct(start, tz=tz)
	if( !inherits(end, 'POSIXct') ) end <- as.POSIXct(end, tz=tz)

	start <- as.POSIXct(as.POSIXlt(start, tz='UTC'))
	end <- as.POSIXct(as.POSIXlt(end, tz='UTC'))

	# recuperation des noms de sites, de polluants, de methode de prelevement
	q <- list ()
	
	if (!is.null (sites) ) {
		if( !is.list(sites) )
			sites <- xrGetSitesPrelevement (conn, sites, campagnes=campagnes) else {
				sites$campagnes <- unique(c(sites$campagnes, campagnes))
				sites <- do.call(xrGetSitesPrelevement, c(list(conn=conn), sites))
			}
		q$sites <- paste ("'", sites$NSIT, "'", sep='', collapse=', ')
	} else if (!is.null(campagnes)) {
		sites <- xrGetSitesPrelevement (conn, campagnes=campagnes)
		q$sites <- paste ("'", sites$NSIT, "'", sep='', collapse=', ')
	}
	if (!is.null (polluants) ) {
		if( !is.list(polluants) )
			polluants <- xrGetPolluants (conn, polluants) else
			polluants <- do.call(xrGetPolluants, c(list(conn=conn), polluants))
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
	
	# création de LA requete de rapatriement
	query <- sprintf (
	"SELECT SITE_PRELEVEMENT.LIBELLE site, LONGI, LATI, LAMBERTX, LAMBERTY,
		METH_PRELEVEMENT.LIBELLE methode,
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
		CODE_QUALITE IN (%s) AND CATEGORIE IN (%s)",
	q$start, q$end, q$valid.states, paste(categories, collapse=', '))

	if (!is.null(q$sites))
		query <- sprintf ("%s AND NSIT IN (%s)", query, q$sites)
	if (!is.null(q$polluants))
		query <- sprintf ("%s AND NOPOL IN (%s)", query, q$polluants)
	if (!is.null(q$methodes))
		query <- sprintf ("%s AND CODE_METH_P IN (%s)", query, q$methodes)

	# recuperation des données
	result <- xrGetQuery (conn, query)

	# à priori pour airparif, les Q_, H_, etc. sont rappatriés sous forme de
	# char, donc conversion
	# conversion systématique (sans impact si pas nécessaire, corrige à
	# priori non pour XR < 6 mais pour windows >= 7)
	a.convertir <- c('VALEUR', 'LAMBERTX', 'LAMBERTY', 'LONGI', 'LATI')
	result[a.convertir] <- lapply(result[a.convertir], function(x)
		{
			if (is.character(x)) {
				wv <- grepl(',', x)
				x[wv] <- sub(',', '.', x[wv])
			}
			as.numeric(x)
		} )

	# mise en forme des données
	result <- split (result, paste (result$DATE_DEB, result$DATE_FIN) )
	fun.tmp <- function(x, what) {
		debut <- unique (x$DATE_DEB)
		fin <- unique (x$DATE_FIN)
		valeurs <- split (
			x[c('VALEUR', 'CODE_QUALITE')],
			gsub(' ', '.', paste(x$SITE, x$METHODE, x$NOPOL, sep='.')))

		if (any (sapply (valeurs, nrow) > 1) ) {
			warning (sprintf ("\n%s, \nCertains prélèvements sont
dupliqués. L'association entre les différentes mesures risque d'être arbitraire.",
paste (names(valeurs)[sapply (valeurs, nrow) > 1]) ) )
		
			for (i in which (sapply (valeurs, nrow) > 1))
			for (j in 1:nrow (valeurs[[i]]) )
				valeurs[[paste(names(valeurs)[i], j, sep='.')]] <-
						valeurs[[i]][j, , drop=FALSE]
			valeurs[which (sapply (valeurs, nrow) > 1)] <- NULL
		}
		ret <- data.frame (start=debut, end=fin)
		if (what=='value')
			ret <- data.frame(
				ret,
				lapply(valeurs, function(x) x$VALEUR)) else
		if (what=='state')
			ret <- data.frame(
				ret,
				lapply(valeurs, function(x) x$CODE_QUALITE)) else{
			noms <- names (valeurs)
			ret <- data.frame(
				ret, lapply (valeurs, function(x) x$VALEUR),
				lapply (valeurs, function(x) x$CODE_QUALITE))
			names(ret)[-(1:2)] <- paste(
				rep (noms, 2),
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

	if (length(result) == 0) {
		result <- TimeIntervalDataFrame(character(0), character(0), 'UTC')

	} else {
		result <- result[[1]]

		result <- new ('TimeIntervalDataFrame',
				   start=as.POSIXct(result$start, 'UTC'),
				   end=as.POSIXct(result$end, 'UTC'),
				   timezone='UTC', data=result[-(1:2)])
	}

	if( !is.null(cursor) )
		result <- as.TimeInstantDataFrame(result, cursor)
	timezone(result) <- tz

	return (result)
}

