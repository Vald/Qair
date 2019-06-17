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
#' @param validOnly (v3) La recherche doit-elle porter uniquement sur les
#'  stations ouvertes ?
#'
#' @seealso \code{\link{xrGetContinuousData}}
#'
#' @return une data.frame correspondant au contenu de la table 
#'	pour les stations trouvées.
#' @export
xrGetStations <- function(conn, pattern = NULL, search.fields = NULL,
			  campagnes = NULL, reseaux = NULL, fields = NULL, mesures = NULL,
			  collapse=c('AND', 'OR'), exact=FALSE, resv3=FALSE, validOnly=FALSE){#,
			  #startDate=NULL, stopDate=NULL) {
	collapse <- match.arg(collapse)

	# récupération de la version avec laquelle on bosse et initialisation de
	# la requête

	nv     <- paste0('nv', conn[['version']])
	bquery <- sprintf('sites?')

	# récupération des champs possibles de recherches (dépend de la version de
	# Qair)
	# si nv3, on bosse directement avec, sinon on récupére les champs nv2, 
	# on les 'traduit' en nv3, on fait tout le travail en nv3 et à la fin 
	# de la fonction on revient éventuellement en nv2.

	xrfields <- xrListFields ('sites')
	if(is.null(search.fields)){
		search.fields <- c('id')#TODO: mettre le remplacant de NOM_COURT_SIT en plus
	}else{
		search.fields <- match.arg(search.fields, xrfields[[nv]], TRUE)
		if(conn[['version']] == 2)
			search.fields <- xrfields[['nv3']][match(search.fields, xrfields[['nv2']])]
	}

	# algo:
	# récupération des sites sur la base de search.fields, de campagne et de reseaux 
	# puis recherche de mesure si != NULL et récup. de id site
	# puis refiltre ssi search.fields contient autre chose que juste id
	# --> agregation des différents tests en fonction de collapse...

	# FIXME:1 comme la recherche % et ? ne marche pas sur id, on prend toutes
	# les stations et on cherche dedans
	all.stations <- xrGetQuery(conn, bquery, resv3=TRUE)

	# recherche sur idsite si idsite in search.fields / IDENTIFIANT
	# faire de même sur ref / NSIT
	# FIXME:1 en attendant que la recherche % et ? fonctionne 'id' et 'ref'
	# sont gérés comme les autres champs.
	# if('id' in search.fields)
	# if('ref' in search.fields)
		#if(conn[['version']] == 2 & !exact)
		#	query <- paste0('%', pattern, '%', collapse=',') else
		#	query <- paste0(pattern, collapse=',')

	idsites <- NULL
	for (sf in search.fields){
		if(conn[['version']] == 2){
			if(exact)
				selection <- match(pattern, all.stations[[sf]]) else
				selection <- sapply(pattern, grep, all.stations[[sf]])
		} else {
			selection <- gsub('\\%', '.*', pattern)
			selection <- gsub('\\?', '.', selection)
			selection <- paste0('^', selection, '$')
			selection <- sapply(selection, grep, all.stations[[sf]])
		}
		ist <- all.stations[['id']][unique(unlist(selection))]

		if(is.null(idsites)){
			idsites <- ist
		}else if (collapse == 'AND'){
			idsites <- intersect(ist, idsites)
		}else
			idsites <- unique(c(ist, idsites))
	}

	# récupération d'idsite sur la base de mesures

	if (!is.null (mesures) ) {
		if( !is.list(mesures) )
			mesures <- xrGetMesures(conn, pattern = mesures, resv3=TRUE) else{
			mesures[['resv3']] <- TRUE
			mesures <- do.call(xrGetMesures, c(list(conn=conn), mesures))
			}
		ist <- unique(mesures[['id_site']])

		if(is.null(idsites)){
			idsites <- ist
		}else if (collapse == 'AND'){
			idsites <- intersect(ist, idsites)
		}else
			idsites <- unique(c(ist, idsites))
	}

	# récupération d'idcampaignes
	if (!is.null (campagnes) ) {
		# FIXME: il manque une requete qui permettrait d'accéder à CAMPMES_STATION
		if( !is.list(campagnes) )
			campagnes <- xrGetCampagnes(conn, pattern = campagnes, resv3=TRUE) else{
			campagnes[['resv3']] <- TRUE
			campagnes <- do.call(xrGetCampagnes, c(list(conn=conn), campagnes))
			}
		ist <- unique(campagnes[['id_site']])

		if(is.null(idsites)){
			idsites <- ist
		}else if (collapse == 'AND'){
			idsites <- intersect(ist, idsites)
		}else
			idsites <- unique(c(ist, idsites))
	}

	# récupération d'idgroups
	if (!is.null (reseaux) ) {
		# FIXME: il manque une requete qui permettrait d'accéder à RESEAUSTA
		if( !is.list(reseaux) )
			reseaux <- xrGetReseaux(conn, pattern = reseaux, resv3=TRUE) else{
			reseaux[['resv3']] <- TRUE
			reseaux <- do.call(xrGetReseaux, c(list(conn=conn), reseaux))
			}
		ist <- unique(reseaux[['id_site']])


		if(is.null(idsites)){
			idsites <- ist
		}else if (collapse == 'AND'){
			idsites <- intersect(ist, idsites)
		}else
			idsites <- unique(c(ist, idsites))
	}

	# création et exécution de la requête

	query <- bquery
	query <- sprintf('%svalidOnly=%s', query, if(validOnly) 'TRUE' else 'FALSE')
	if(!is.null(idsites))
		query <- sprintf('%s&sites=%s', query, paste(idsites, collapse=','))

	# TODO:  ajouter filtre stopDate, startDate

	stations <- xrGetQuery(conn, query, resv3=TRUE)

	if(!resv3 & nv == 'nv2')
		names(stations) <- xrfields[['nv2']][match(names(stations), xrfields[['nv3']])]

	if (is.null(fields)) fields <- xrfields[[nv]]
	# TODO: à compléter quand on aura l'équivalent du champ classe_site dans
	# l'API (remplacer 'CLASSE_SITE par sa valeur dans l'API
	#fields <- union(fields, 'CLASSE_SITE')
	fields <- intersect(fields, xrfields[[nv]])

	return(stations[fields])
}


