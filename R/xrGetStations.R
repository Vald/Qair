#' Recuperation des stations de mesures definies dans une base XR
#'
#' La fonction permet de lister les stations existantes dans la 
#' base XR référencée par la connexion \code{conn}.
#'
#' @inheritParams xrGetContinuousData
#' @param mesures chaînes de caractères correspondant aux mesures à rapatrier
#' 	(optionnel) (utilisé via la fonction \code{\link{xrGetMesures}}).
#'  si c'est un vecteur, est directement utilisé comme pattern pour la fonction
#'  xrGetMesures Si C'est une liste, les éléments doivent être nommés. Chaque
#'  élément est alors utilisé comme argument pour la fonction xrGetMesures
#'  pattern doit alors être précisé :
#' 
#'  \code{... list(pattern = 'N2_VER', search.fields = 'IDENTIFIANT') ...}
#' @param fields vecteurs indiquant les champs de la table à récupérer.
#'	Tous par défaut.
#' @param resv3 Booléen : faut-il forcer le retour au format v3 ? Par défaut
#'  non, donc si la connexion à XR est en v2, les noms correspondront à la v2.
#'  Seul le résultat est affecté : si la connexion est en v2, les noms des
#'  champs de recherches doivent être spécifiés en v2. Son utilisation devrait
#'  être réservée à des fins de développement.
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les stations trouvées.
#' @export
xrGetStations <- function(conn, pattern = NULL, search.fields = c('IDENTIFIANT', 'NOM_COURT_SIT'),
			  campagnes = NULL, reseaux = NULL, fields = NULL, mesures = NULL,
			  collapse=c('AND', 'OR'), exact=FALSE, resv3=FALSE) {

	nv    <- paste0('nv', xr[['version']])
	query <- sprintf('sites?')

	xrfields <- xrListFields ('sites')
	if(conn[['version']] == 2) {
		search.fields <- intersect(search.fields, xrfields[['nv2']])
		search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	} else 
		search.fields <- intersect(search.fields, xrfields[['nv3']])

	# algo:
	# récupération des sites sur la base de search.fields (si id), de campagne et de reseaux 
	# puis recherche de mesure si != NULL et récup. de id site
	# puis refiltre ssi search.fields contient autre chose que juste id
	# --> agregation des différents tests en fonction de collapse...
	# TODO: si search.fields contient id, on adapte la requete, 

	# recherche sur idsite si idsite in search.fields / IDENTIFIANT
	# faire de même sur ref / NSIT

	if ('id' %in% search.fields){
		idsites <- 1 # TODO: HERE
		# faire la recherche
		search.fields <- setdiff(search.fields, 'id')
	} else idsites <- character()

	# récupération d'idsite sur la base de mesures

	if (!is.null (mesures) ) {
		if( !is.list(mesures) )
			mesures <- xrGetMesures(conn, pattern = mesures, resv3=TRUE) else{
			mesures[['v3']] <- TRUE
			mesures <- do.call(xrGetMesures, c(list(conn=conn), mesures))
			}
		# TODO: gérer si collapse = AND ou OR
		idsites <- unique(mesures[['id_site']])
	}

	# récupération d'idcampaignes (quel champ ?)
	# TODO: gérer si collapse = AND ou OR
	if (!is.null (campagnes) ) {
	#<<<<<<<<<<
		if( !is.list(campagnes) )
			campagnes <- unique (xrGetCampagnes (conn, pattern = campagnes)$NOM_COURT_CM) else
			campagnes <- unique(do.call(xrGetCampagnes, c(list(conn=conn), campagnes))$NOM_COURT_CM)
		campagnes <- unique(campagnes[['']])
		if (length(campagnes) != 0)
			if(length(idsites) == 0) idsites <- campagnes {
			idsites <- if( == 'AND') else
		}
	#>>>>>>>>>>
	}

	# récupération d'idgroups (quel champ ?)
	# TODO: gérer si collapse = AND ou OR
	if (!is.null (reseaux) ) {
	#<<<<<<<<<<
		q$reseaux <- unique (xrGetReseaux (conn, pattern = reseaux)$NOM_COURT_RES)
		if (length(q$reseaux) == 0) q$reseaux <- NULL else {
			q$tables <- c(q$tables, 'RESEAUSTA')
			q$reseaux <- sprintf(
				'STATION.NOM_COURT_SIT=RESEAUSTA.NOM_COURT_SIT AND RESEAUSTA.NOM_COURT_RES IN (%s)',
				paste ("'", q$reseaux, "'", sep = '', collapse = ", ") )
		}
	#>>>>>>>>>>
	}


	# TODO: lancement de la requete avec filtre sur sites, campaigns, groups
	# ajouter stopDate, startDate, validOnly
	#<<<<<<<<<<
	query <- sprintf ('%s %s', query, paste (q$tables, collapse=', ') )
	q$tables <- NULL
	if (length (q) > 0)
		query <- sprintf ('%s WHERE %s', query, paste (q, collapse = collapse) )

	stations <- unique (xrGetQuery (conn, query) )
	#>>>>>>>>>>

	# TODO: filtre sur les autres champs que id_site
	if (!is.null (pattern) & length (search.fields) > 0)
	#<<<<<<<<<<
		q$pattern <- match.pattern.fields (
					pattern, sprintf('STATION.%s', search.fields),
					type=ifelse(exact, 'IN', 'LIKE'))
	#>>>>>>>>>>



	# TODO: typologie : vérifier que les valeurs sont compatibles ...
	#temp <- xrGetQuery (conn, "SELECT CLE, LIBELLE FROM LISTE_META_DONNEES WHERE CODE_ID_LISTE='CL_SITE'")
	#names (temp)[2] <- 'typologie'

	if(!resv3 & nv == 'nv2')
		names(result) <- xrfields[['nv2']][match(names(result), xrfields[['nv3']])]

	if (is.null(fields)) fields <- xrfields[[nv]]
	# TODO: à compléter quand on aura l'équivalent du champ classe_site dans
	# l'API (remplacer 'CLASSE_SITE par sa valeur dans l'API
	#fields <- union(fields, 'CLASSE_SITE')
	fields <- intersect(fields, xrfields[[nv]])

	return(result[fields])
}


