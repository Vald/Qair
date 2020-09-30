#' Recuperation du resultat d'une requete dans une base XR.
#' 
#' @section Details: Cette fonction permet d'envoyer une requête à une connexion
#' sur une base XR et d'en récupérer le résultat.
#' Sous windows, le pilote utilisé est ODBC (nécessite d'avoir
#' installer le paquet \code{\link[RODBC]{RODBC}}), sous linux JDBC
#' (nécessite d'avoir installer le paquet \code{\link[RJDBC]{JDBC}}).
#' 
#' @param conn une connexion à une base XR (cf \code{\link{xrConnect}})
#' @param query chaîne de caractères contenant la reqûete
#'
#' @return une data.frame avec le résultat de la requête ou 
#' 	une erreur si la requête n'a pas abouti.
#' 
#' @seealso \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}}
xrGetQueryBD <- function (conn, query) {

	conn <- conn[['db']]

	# execution normale de la requete

	if(options()$Xair.drv == 'odbc') {
		# note: 'as.is' permet de récupérer les dates directement au
		# format 'character' et empeche que, lorsqu'une variable de type
		# chaine de caractères ne contient que des entiers, cette variable
		# soit convertie (exemple : c('03', '14') converti en c(3, 14) au
		# de rester tel quel).
		result <- RODBC::sqlQuery(conn, query, stringsAsFactors=FALSE, as.is=TRUE)
	} else if(options()$Xair.drv == 'oracle') {
		result <- DBI::dbGetQuery(conn, query)
	} else if(options()$Xair.drv == 'jdbc') {
		result <- DBI::dbGetQuery(conn, query)
	} else stop ('Unrecognized driver.')

	# par rapport au rapatriement des dates, c'est n'importe quoi :
	# elles sont stockées en CET/CEST et sont censées représentées des 
	# dates en UTC, RODBC rapatrie correctement les dates en CET/CEST, 
	# mais comme ça doit être en UTC, ça décale, ROracle croit que c'est en
	# CEST (et rien en CET), du coup il rapatrie tout comme si c'était 
	# du CEST et converti les dates d'été en CET. Et en plus rien ne dit
	# que ça ne dépend pas de la plate-forme. RJDBC retourne pour les dates
	# des strings alors que les 2 autres retournes des POSIXt. Note : 
	# RJDBC ne fait du coup aucune hypothèse et ramène tout en strings
	# ce qui fait qu'il a tout bon.
	# Du coup, on va s'arranger pour que tous les drivers retournent des
	# valeurs de dates comme RJDBC : au format strings. Donc si on détecte
	# dans le tableau de résultats des variables au format POSIXt, on refait
	# la requete, mais uniquement sur les dates

	if( any(sapply(result, inherits, 'POSIXt')) ) {
		var.to.get <- names(result)
		date.var <- names(result)[sapply(result, inherits, 'POSIXt')]
		date.var <- sprintf("TO_CHAR(%s, 'YYYY-MM-DD HH24:MI:SS') %s",
				    date.var, date.var)

		var.to.get[sapply(result, inherits, 'POSIXt')] <- date.var

		var.to.get <- sprintf('SELECT %s FROM TATA',
				    paste( var.to.get, collapse=", " ))

		query <- sprintf("WITH TATA AS (%s) %s", query, var.to.get)

		if(options()$Xair.drv == 'odbc') {
			result <- RODBC::sqlQuery(conn, query, stringsAsFactors=FALSE)
		} else if(options()$Xair.drv == 'oracle') {
			result <- DBI::dbGetQuery(conn, query)
		} else if(options()$Xair.drv == 'jdbc') {
			result <- DBI::dbGetQuery(conn, query)
		} else stop ('Unrecognized driver.')

		for( i in names(date.var) )
			result[[i]] <- ifelse(is.na(date.var[[i]]), 
					      NA,
					      sprintf('%s.0', date.var[[i]]))
	}

	return( result )

}
