'calcul_seuil' <-
function(date, x, seuil){
	if(!is.na(seuil$agregation) && seuil$agregation == seuil$donnees_base)stop("Les donnees de bases et la base d'agregation ne peuvent etre identiques")

	digits <- seuil$precision

	# extraction de la periode dont on a besoin
	#------------------------------------------
	x <- x[as.numeric(months(date-1/24)) %in% eval(parse(text=seuil$periode))]
	date <- date[as.numeric(months(date-1/24)) %in% eval(parse(text=seuil$periode))]

	# calcul des valeurs journalieres si besoin
	#------------------------------------------
	if(seuil$donnees_base=="jour"){
		x <- round.a(slide(x, 24, 24), digits)
		date <- chron(unique(floor(date-1/24)))
		}

	# agregation des donnees (utilise agregation/glissement/operation)
	#-----------------------------------------------------------------
	if(!is.na(seuil$agregation)){
		# quelques tests de validite des operations demandees
		#----------------------------------------------------
		if(as.logical(seuil$glissement) & seuil$operation != "moyenne")stop("aucun seuil ne peut etre defini est 'glissant'\navec une agregation autre que la moyenne")
		if(length(grep("C", seuil$operation)) > 0 & seuil$agregation != "an")stop("un percentile ne peut etre appliquees")
		if(seuil$operation == "AOT40" & seuil$agregation != "an")stop("les AOT40 ne s'agrege que sur une annee complete")
		if(seuil$operation == "8heures" & !as.logical(seuil$glissement))stop("une agregation sur 8 heures ne peut se faire que par glissement")
		if(seuil$operation == "24heures" & !as.logical(seuil$glissement))stop("une agregation sur 24 heures ne peut se faire que par glissement")

		# application de la fonction qui va bien
		#---------------------------------------
		if(seuil$agregation == "an"){
			if(seuil$operation == "moyenne"){
				x <- if(mean(!is.na(x)) >= 0.75) mean(x, na.rm=TRUE) else NA
			}else if(length(grep("C", seuil$operation)) > 0){
				x <- if(mean(!is.na(x)) >= 0.75) quantile.a(x, as.numeric(sub("C", "", seuil$operation))/100) else NA
			}else if(seuil$operation == "AOT40"){
			x <- x[hours(date) %in% 8:19]
				x <- if(mean(!is.na(x)) >= 0.9){sum(ifelse(!is.na(x) & x>80, x-80, 0), na.rm=TRUE) / mean(!is.na(x))}else{NA}
			}else{
				stop("operation impossible pour une agregation 'an'")
				}
		#}else if(seuil$agregation == "8heures"){
		} else if(grepl('heures', seuil$agregation)) {
				fenetre <-
					as.numeric(sub('heures', '', seuil$agregation))
				x <- slide(x, fenetre, 1)
		}else if(seuil$agregation == "jour"){
			if(seuil$operation == "moyenne"){
				x <- slide(x, 24, ifelse(as.logical(seuil$glissement), 1, 24))
			}else if(seuil$operation == "max"){
				options(warn=-1)
				x <- tapply(x, floor(date-1/24), max, na.rm=TRUE)
				options(warn=0)
				x[abs(x)==Inf] <- NA
		}else{stop("operation impossible pour une agregation 'jour'")}
		}else{
			stop(sprintf("%s : type d'agregation non definie", seuil$agregation))
			}
		}

	# arrondi de x selon ADEME
	#-------------------------
	x <- round.a(x, digits)

	# valeur de retour en fonction de ce qui est demande
	#---------------------------------------------------
	if(as.logical(seuil$nb_dep)){
		return(sum(slide(as.numeric(x > seuil$seuil), seuil$remanence, 1)==1, na.rm=TRUE))
	}else if(as.logical(seuil$nb_jour_dep)){
		if(!is.na(seuil$agregation) && !as.logical(seuil$glissement))stop("impossible de calculer un nombre de jours de depassement pour des valeurs aggregees sans glissement")
		return(sum(tapply(slide(as.numeric(x > seuil$seuil), seuil$remanence, 1)==1, floor(date-ifelse(seuil$donnees_base == "heure", 1/24, 0)), any), na.rm=TRUE))
	}else{
		return(x)
		}
	}
