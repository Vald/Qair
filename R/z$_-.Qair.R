`$<-.Qair` <-
function(x, name, value){
# value = list(value, mesure=NA, unite=NA, station=NA)
	donnees <- x
	if(name=="date")stop("impossible de modifier les dates")
	if(!is.list(value)) value <- list(newVar=value)
	if(class(value)!="list")stop("value n'a pas le bon format")
	if(!"newVar" %in% names(value))stop("aucune valeur renseignee")
	if(length(value$newVar)==1){value$newVar <- rep(value$newVar, nrow(donnees))
	}else if(nrow(donnees) != length(value$newVar)){stop("taille des donnees incompatibles")
	}
	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	if(name %in% names(donnees)){
		donnees[[name]] <- value$newVar
#		attributes(donnees)$mesure <- attributesTemp$mesure
#		attributes(donnees)$station <- attributesTemp$station
#		attributes(donnees)$unite <- attributesTemp$unite
#		attributes(conversion)$longitude <- attributesTemp$longitude
#		attributes(conversion)$latitude <- attributesTemp$latitude
#		attributes(conversion)$lambertx <- attributesTemp$lambertx
#		attributes(conversion)$lamberty <- attributesTemp$lamberty

#		if("mesure" %in% names(value))attributes(donnees)$mesure[names(donnees)==name] <- value$mesure
#		if("unite" %in% names(value))attributes(donnees)$unite[names(donnees)==name] <- value$unite
#		if("station" %in% names(value))attributes(donnees)$station[names(donnees)==name] <- value$station
#		if("longitude" %in% names(value))attributes(donnees)$longitude[names(donnees)==name] <- value$longitude
#		if("latitude" %in% names(value))attributes(donnees)$latitude[names(donnees)==name] <- value$latitude
#		if("lambertx" %in% names(value))attributes(donnees)$lambertx[names(donnees)==name] <- value$lambertx
#		if("lamberty" %in% names(value))attributes(donnees)$lamberty[names(donnees)==name] <- value$lamberty
		for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class"))){
			attr(donnees, attTemp) <- attributesTemp[[attTemp]]
			if(attTemp %in% names(value))attr(donnees, attTemp)[names(donnees)==name] <- value[[attTemp]]
			}
	}else{
		donnees[[length(donnees)+1]] <- value$newVar
		names(donnees)[length(donnees)] <- name
#		attributes(donnees)$mesure <- c(attributesTemp$mesure, ifelse("mesure" %in% names(value), value$mesure, NA))
#		attributes(donnees)$station <- c(attributesTemp$station, ifelse("station" %in% names(value), value$station, NA))
#		attributes(donnees)$unite <- c(attributesTemp$unite, ifelse("unite" %in% names(value), value$unite, NA))
#		attributes(donnees)$longitude <- c(attributesTemp$longitude, ifelse("longitude" %in% names(value), value$longitude, NA))
#		attributes(donnees)$latitude <- c(attributesTemp$latitude, ifelse("latitude" %in% names(value), value$latitude, NA))
#		attributes(donnees)$lambertx <- c(attributesTemp$lambertx, ifelse("lambertx" %in% names(value), value$lambertx, NA))
#		attributes(donnees)$lamberty <- c(attributesTemp$lamberty, ifelse("lamberty" %in% names(value), value$lamberty, NA))
		for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class")))
			attr(donnees, attTemp) <- c(attributesTemp[[attTemp]], ifelse(attTemp %in% names(value), value[[attTemp]], NA))
		}
	class(donnees) <- attributesTemp$class
	donnees
	}

