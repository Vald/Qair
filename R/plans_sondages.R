#-----------------------------
# METHODE DES PLANS DE SONDAGE
#-----------------------------

# echantillon est data.frame avec des colonnes "dated", "datef", "pol1", "pol2", etc. et une colonne "grappe", et une "strate"
# auxiliaire est une data.frame avec les colonnes "date" et "strate", et si on realise un redressement , "pol1", "pol2", etc. 

estimation_sondage <- function(echantillon, auxiliaire, redressement=NULL, premier=T, iso=NULL){
	if(length(echantillon)==4) return(NULL)
	echantillon <- echantillon[c("dated", "datef", "grappe", "strate", names(echantillon)[!(names(echantillon) %in% c("dated", "datef", "grappe", "strate"))])]
	auxiliaire <- auxiliaire[c("dated", "datef", "strate", names(auxiliaire)[!(names(auxiliaire) %in% c("dated", "datef", "strate"))])]

	# constantes
	#-----------
	duree <- as.numeric(mean(auxiliaire$datef-auxiliaire$dated))
	redressement <- if(any(redressement %in% c("diff", "quot", "reg"))) redressement[which(redressement %in% c("diff", "quot", "reg"))] else NULL
	# nom des variables dont on cherche a estimer la moyenne
	interet <- names(echantillon)[5]
	aux <- names(auxiliaire)[4]

	# mise en forme
	#--------------
	echValide <- echantillon[!is.na(echantillon$grappe) & !is.na(echantillon[[interet]]), 1:5]

	if(!is.null(redressement)){
		grappe_d <- chron(tapply(echValide$dated, echValide$grappe, min, na.rm=T))
		grappe_f <- chron(tapply(echValide$datef, echValide$grappe, max, na.rm=T))
		compa_date <- outer(grappe_d, auxiliaire$dated, "<=") & outer(grappe_f, auxiliaire$datef, ">=")

		if(!is.null(iso)){
			gr_nom <- as.numeric(sapply(strsplit(names(grappe_d), " "), "[[", 1))

			laquelle_grappe <- lapply(lapply(iso+sum(!is.na(unique(iso)))*(auxiliaire$strate-1), "==", gr_nom), which)
			laquelle_grappe <- ifelse(lapply(laquelle_grappe, any), laquelle_grappe, NA)
			laquelle_grappe <- sapply(mapply("==", ifelse(apply(compa_date, 2, any), apply(compa_date, 2, which), NA), laquelle_grappe), which)
			laquelle_grappe <- ifelse(lapply(laquelle_grappe, any), laquelle_grappe, length(grappe_f)+1)

			indices <- unlist(mapply("[", apply(compa_date, 2, which), laquelle_grappe))

			echValide$grappe <- sapply(lapply(echValide$grappe, "==", gr_nom), which)
		}else{
			gr_nom <- as.numeric(sapply(strsplit(names(grappe_d), " "), "[[", 1))
			indices <- unlist(ifelse(apply(compa_date, 2, any), apply(compa_date, 2, which), NA))
			indices <- gr_nom[indices]
			}

		auxValide <- auxiliaire[!is.na(indices), 1:3]
		auxValide <- data.frame(auxValide, grappe=indices[!is.na(indices)])

		auxValide <- data.frame(auxValide, auxiliaire[!is.na(indices), 4])
		names(auxValide)[5] <- aux
		}

	# determination des caracteristiques utiles de chaque grappe
	#-----------------------------------------------------------
	echValide$poids <- as.numeric(echValide$datef)-as.numeric(echValide$dated)
	grappe_ech <- split(echValide[-(1:2)], echValide$grappe)
	grappe_ech <- data.frame(t(rbind(poids = sapply(lapply(grappe_ech, "[[", "poids"), sum), sapply(lapply(grappe_ech, "[", c("grappe", "strate", interet)), colMeans))))
	grappe_ech$poids <- unlist(mapply("/", split(grappe_ech$poids, grappe_ech$strate), as.list(tapply(grappe_ech$poids, grappe_ech$strate, sum)), SIMPLIFY=F))
	names(grappe_ech)[4] <- "moy"

	if(!is.null(redressement)){
		auxValide$poids <- as.numeric(auxValide$datef)-as.numeric(auxValide$dated)
		grappe_aux <- split(auxValide[!is.na(auxValide[5]), -(1:2)], auxValide$grappe[!is.na(auxValide[5])])
		grappe_aux <- data.frame(t(rbind(poids = sapply(lapply(grappe_aux, "[[", "poids"), sum), sapply(lapply(grappe_aux, "[", c("grappe", "strate", aux)), colMeans))))
		grappe_aux$poids <- unlist(mapply("/", split(grappe_aux$poids, grappe_aux$strate), as.list(tapply(grappe_aux$poids, grappe_aux$strate, sum)), SIMPLIFY=F))
		names(grappe_aux)[4] <- "moy"
		}

	# determination des caracteristiques utiles de chaque strate
	#-----------------------------------------------------------
	strate <- data.frame(poids=tapply(auxiliaire$strate, auxiliaire$strate, length)/nrow(auxiliaire))
	strate$m_ech <- tapply(grappe_ech$poids*grappe_ech$moy, grappe_ech$strate, sum)

#	# CAS D'UNE POPULATION INFINIE
#	grappe <- split(grappe_ech, grappe_ech$strate)
#	sommePoidsCarre <- sapply(lapply(lapply(grappe, "[[", "poids"), "^", 2), sum)
#	nbGrappe <- sapply(grappe, nrow)
#	varGrappe <- sapply(lapply(mapply("-", lapply(grappe, "[[", "moy"), as.list(strate$m_ech), SIMPLIFY=F), "^", 2), sum)
#	strate$v_ech <- nbGrappe * sommePoidsCarre/(nbGrappe+nbGrappe*sommePoidsCarre-2) * varGrappe

	# CAS D'UNE POPULATION FINIE
	grappe <- split(grappe_ech, grappe_ech$strate)
	nbGrappeEch <- sapply(grappe, nrow)
	nbGrappePop <- nbGrappeEch*tapply(as.numeric(auxiliaire$datef)-as.numeric(auxiliaire$dated), auxiliaire$strate, sum)/tapply(as.numeric(echValide$datef)-as.numeric(echValide$dated), echValide$strate, sum)
	varGrappe <- sapply(lapply(mapply("-", mapply("*", lapply(grappe, "[[", "moy"), lapply(grappe, "[[", "poids"), SIMPLIFY=F), as.list(strate$m_ech / nbGrappeEch), SIMPLIFY=F), "^", 2), sum)

	strate$v_ech <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe


	if(!is.null(redressement)) {
		if(nrow(grappe_ech) != nrow(grappe_aux)) {
			print("le nombre de grappes dans la variable auxiliaire est insuffisant, le(s) redressement(s) n'est(e sont) pas calcule(s)")
			strate$cov <- NA
		} else {
			strate$m_aux <- tapply(grappe_aux$poids*grappe_aux$moy, grappe_aux$strate, sum)
			strate$m_reelle <- tapply(auxiliaire[[aux]], auxiliaire$strate, mean, na.rm=T)

#			# CAS D'UNE POPULATION INFINIE
#			grappe <- split(grappe_aux, grappe_aux$strate)
#			varGrappe <- sapply(lapply(mapply("-", lapply(grappe, "[[", "moy"), as.list(strate$m_aux), SIMPLIFY=F), "^", 2), sum)
#			strate$v_aux <- nbGrappe*sommePoidsCarre/(nbGrappe+nbGrappe*sommePoidsCarre-2) * varGrappe

			# CAS D'UNE POPULATION FINIE
			grappe <- split(grappe_aux, grappe_aux$strate)
			varGrappe <- sapply(lapply(mapply("-", mapply("*", lapply(grappe, "[[", "moy"), lapply(grappe, "[[", "poids"), SIMPLIFY=F), as.list(strate$m_aux / nbGrappeEch), SIMPLIFY=F), "^", 2), sum)

			strate$v_aux <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe

#			# CAS D'UNE POPULATION INFINIE
#			varGrappe <- sapply(mapply("*", mapply("-", lapply(split(grappe_ech, grappe_ech$strate), "[[", "moy"), as.list(strate$m_ech), SIMPLIFY=F), mapply("-", lapply(split(grappe_aux, grappe_aux$strate), "[[", "moy"), as.list(strate$m_aux), SIMPLIFY=F), SIMPLIFY=F),sum, na.rm=T)
#			strate$cov <- nbGrappe * sommePoidsCarre/(nbGrappe+nbGrappe*sommePoidsCarre-2) * varGrappe

			# CAS D'UNE POPULATION INFINIE
			varGrappe <- sapply(mapply("*", mapply("-", mapply("*", lapply(split(grappe_ech, grappe_ech$strate), "[[", "moy"), lapply(split(grappe_ech, grappe_ech$strate), "[[", "poids"), SIMPLIFY=F), as.list(strate$m_ech/nbGrappeEch), SIMPLIFY=F), mapply("-", mapply("*", lapply(split(grappe_aux, grappe_aux$strate), "[[", "moy"), lapply(split(grappe_aux, grappe_aux$strate), "[[", "poids"), SIMPLIFY=F), as.list(strate$m_aux/nbGrappeEch), SIMPLIFY=F), SIMPLIFY=F),sum, na.rm=T)
			strate$cov <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe
			}
		}



	# estimation des caracteristiques sur la population
	#--------------------------------------------------
	pop <- data.frame(moy=sum(strate$poids*strate$m_ech), var=sum(strate$poids^2*strate$v_ech))
	if("diff" %in% redressement){
		if(all(!is.na(strate$cov))){
			pop$m_diff <- sum(strate$poids*(strate$m_ech-strate$m_aux+strate$m_reelle));pop$v_diff <- sum(strate$poids^2*(strate$v_ech+strate$v_aux-2*strate$cov))
		}else{pop$m_diff <- NA;pop$v_diff <- NA}
		}
	if("quot" %in% redressement){
		if(all(!is.na(strate$cov))){
			r <- pop$moy/sum(strate$poids*strate$m_aux);pop$m_quot <- sum(strate$poids*(strate$m_ech/strate$m_aux*strate$m_reelle));pop$v_quot <- sum(strate$poids^2*(strate$v_ech+r^2*strate$v_aux-2*r*strate$cov))
		}else{pop$m_quot <- NA;pop$v_quot <- NA}
		}
	if("reg" %in% redressement){
		if(all(!is.na(strate$cov))){
			b <- strate$cov/strate$v_aux;pop$m_reg <- sum(strate$poids*(strate$m_ech+b*(strate$m_reelle-strate$m_aux)));roCarre <- strate$cov^2/(strate$v_ech*strate$v_aux);pop$v_reg <- sum(strate$poids^2*strate$v_ech*(1-roCarre))
		}else{pop$m_reg <- NA;pop$v_reg <- NA}
		}

	if(premier){
		estimation <- data.frame(cbind(t(pop), estimation_sondage(echantillon[-5], if(length(auxiliaire)<=4) auxiliaire else auxiliaire[-4], redressement, premier=F, iso)))
		names(estimation) <- names(echantillon)[-(1:4)]
		return(estimation)
	}else{
		return(cbind(t(pop), estimation_sondage(echantillon[-5], if(length(auxiliaire)<=4) auxiliaire else auxiliaire[-4], redressement, premier=F, iso)))
		}
	}

#pop$moy + 1.96*c(-1, 1)*sqrt(pop$var)
#pop$m_diff + 1.96*c(-1, 1)*sqrt(pop$v_diff)
#pop$m_quot + 1.96*c(-1, 1)*sqrt(pop$v_quot)
#pop$m_reg + 1.96*c(-1, 1)*sqrt(pop$v_reg)



#-------------------------------------------------------------------
# METHODE DES PLANS DE SONDAGE EN UTILISANT LA VARIANCE INTRA-GRAPPE
#-------------------------------------------------------------------
# echantillon est data.frame avec des colonnes "date", "pol1", "pol2", etc. et une colonne "grappe", et une "strate"
# auxiliaire non pris en compte
# units est un element de ("secs", "mins", "hours", "days", "weeks")

estimation_covariogramme <- function(echantillon, auxiliaire, units="hours", redressement=NULL){
	# mise en forme
	#--------------
	grappes <- split(echantillon[-which(names(echantillon)=="grappe")], echantillon$grappe)

	# constantes
	#-----------
	taille_grappe <- min(sapply(grappes, nrow))
	nb_grappes <- length(grappes)
	# nom des variables dont on cherche a estimer la moyenne
	interets <- names(echantillon)[-c(1, which(names(echantillon)=="grappe"))]

	# estimation de la moyenne (moyenne arithmetique)
	#------------------------------------------------
	pop <- data.frame(moy=colMeans(echantillon[interets], na.rm=T))

	# calcul de la matrice des coefficients des esperances
	#-----------------------------------------------------
	n <- taille_grappe
	m <- nb_grappes
	alpha <- matrix(0, ncol=n, nrow=n)
	for(j in 1:(n-1))alpha[n, j] <- alpha[n, j] - 2*(n-j)/(m*n^2)
	alpha[n, n] <- alpha[n, n] + (m*n-1)/(m*n)
	for(i in 1:(n-1))for(j in (1:(n-1))[-i])alpha[i, j] <- alpha[i, j] + 2/(m*n*(n-i)) * ((n-i)*(n-j)/n - max(0, n-i-j) - min(n-i, n-j))
	for(i in 1:(n-1))alpha[i, n] <- alpha[i, n] - 1/(m*n)
	for(i in 1:(n-1))alpha[i, i] <- alpha[i, i] + 1 + 2/(m*n*(n-i)) * ((n-i)*(n-i)/n - max(0, n-i-j) - (n-i))

	# calcul des coeff de la variance de l'echantillon
	#-------------------------------------------------
	coefEstVar <- c(2/(m*n^2)*(n-(1:(n-1))), 1/(m*n))

	# calcul de la moyenne par grappe
	#--------------------------------
	grappe_ech <- list()
	grappe_ech$poids <- lapply(grappes, nrow)

	# calcul des covariances/variance de l'echantillon
	#-------------------------------------------------
	varEch <- list()
	for(interet in interets){
		varEch[[interet]] <- NULL
		for(d in c(1:(n-1), 0)){
			varEch[[interet]] <- c(varEch[[interet]], mean(
				mapply("*",
				lapply(mapply("[", lapply(grappes, "[[", interet), mapply(seq, 1, lapply(grappe_ech$poids, "-", d), SIMPLIFY=F), SIMPLIFY=F), "-", pop$moy[rownames(pop)==interet]),
				lapply(mapply("[", lapply(grappes, "[[", interet), mapply(seq, 1+d, grappe_ech$poids, SIMPLIFY=F), SIMPLIFY=F), "-", pop$moy[rownames(pop)==interet]))
				, na.rm=T)
				)
			}
		}
	varEch[is.na(varEch)] <- 0

	# estimation de la variance
	#--------------------------
	pop$var <- sapply(lapply(varEch, solve, a=alpha), "%*%", a=coefEstVar)
	as.data.frame(t(pop))
	}

#-------------------------------------------------------------------
# METHODE DES PLANS DE SONDAGE EN UTILISANT LE VARIOGRAMME
#-------------------------------------------------------------------
# echantillon est data.frame avec des colonnes "date", "pol1", "pol2", etc. et une colonne "grappe", et une "strate"
# auxiliaire non pris en compte
# units est un element de ("secs", "mins", "hours", "days", "weeks")

estimation_variogramme <- function(echantillon, auxiliaire, units="hours", redressement=NULL){
	# mise en forme
	#--------------
	grappes <- split(echantillon[-which(names(echantillon)=="grappe")], echantillon$grappe)

	# constantes
	#-----------
	taille_grappe <- min(sapply(grappes, nrow))
	nb_grappes <- length(grappes)
	# nom des variables dont on cherche a estimer la moyenne
	interets <- names(echantillon)[-c(1, which(names(echantillon)=="grappe"))]

	# estimation de la moyenne (moyenne arithmetique)
	#------------------------------------------------
	pop <- data.frame(moy=colMeans(echantillon[interets], na.rm=T))

	# calcul de la moyenne par grappe
	#--------------------------------
	grappe_ech <- list()
	grappe_ech$poids <- lapply(grappes, nrow)

	# calcul du variogramme de l'echantillon
	#---------------------------------------
	n <- taille_grappe
	m <- nb_grappes
	estVariog <- list()
	for(interet in interets){
		estVariog[[interet]] <- NULL
		for(d in 1:(n-1)){
			estVariog[[interet]] <- c(estVariog[[interet]], 1/2*mean(
				unlist(lapply(mapply("-", 
				mapply("[", lapply(grappes, "[[", interet), mapply(seq, 1, lapply(grappe_ech$poids, "-", d), SIMPLIFY=F), SIMPLIFY=F),
				mapply("[", lapply(grappes, "[[", interet), mapply(seq, 1+d, grappe_ech$poids, SIMPLIFY=F), SIMPLIFY=F), SIMPLIFY=F), "^", 2))
				, na.rm=T)
				)
			}
		}
	estVariog[is.na(estVariog)] <- 0

	# calcul de la variance de l'echantillon
	#---------------------------------------
	varEch <- list()
	for(interet in interets)
		varEch[[interet]] <- mean((unlist(lapply(grappes, "[[", interet))-pop$moy[rownames(pop)==interet])^2, na.rm=T)

	# estimation de la variance
	#--------------------------
	pop$var <- mapply("-", lapply(varEch, "*", 1/(m-1)), lapply(estVariog, "%*%", 2/(m-1)/n^2* ((n-(1:(n-1))))))
	as.data.frame(t(pop))
	}

#-------------------------------------------------------------------
# METHODE DE LA REGRESSION AVEC PRISE EN COMPTE DU VARIOGRAMME
#-------------------------------------------------------------------
# echantillon est data.frame avec des colonnes "date", "pol1", "pol2", etc. et une colonne "grappe", et une "strate"
# auxiliaire non pris en compte
# units est un element de ("secs", "mins", "hours", "days", "weeks")
