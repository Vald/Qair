`calcRose.rose` <-
function(donnees, FUN, ...){
	if(!is.na(attributes(donnees)$aggregation))stop(paste("la rose est deja aggregee par la fonction", attributes(donnees)$aggregation))
	local.args <- as.list(match.call())
	local.args <- local.args[names(local.args) != ""]
	noms <- names(donnees)
	attributesTemp <- lapply(donnees, "attributes")
	attributesTemp2 <- attributes(donnees)
#	mesureTemp <- mesures(donnees)
#	stationTemp <- stations(donnees)
#	uniteTemp <- unites(donnees)
#	longiTemp <- longitudes(donnees)
#	latiTemp <- latitudes(donnees)
#	xTemp <- lambertx(donnees)
#	yTemp <- lamberty(donnees)
	donnees <- as.list(donnees)
	if(missing(FUN)){
		for(i in 1:length(donnees)){
			if(nrow(donnees[[i]])>0){
				donnees[[i]] <- as.data.frame(do.call("mean", list(donnees[[i]], na.rm=TRUE)))
				if(length(donnees[[i]]) != length(noms))donnees[[i]] <- data.frame(t(donnees[[i]]))
				row.names(donnees[[i]]) <- NULL
#				attributes(donnees[[i]])$mesure <- mesureTemp
#				attributes(donnees[[i]])$station <- stationTemp
#				attributes(donnees[[i]])$unite <- uniteTemp
#				attributes(donnees[[i]])$longitude <- longiTemp
#				attributes(donnees[[i]])$latitude <- latiTemp
#				attributes(donnees[[i]])$lambertx <- xTemp
#				attributes(donnees[[i]])$lamberty <- yTemp
				for(attTemp in setdiff(names(attributesTemp[[i]]), c("names", "row.names", "class")))
					attr(donnees[[i]], attTemp) <- attributesTemp[[i]][[attTemp]]
				class(donnees[[i]]) <- c("Qair", "data.frame")}}
	}else if(exists(paste(FUN, "Qair", sep="."))){
		donnees[sapply(donnees, nrow)>0] <- lapply(donnees[sapply(donnees, nrow)>0], FUN, ...)
	}else if(exists(paste(FUN, "data.frame", sep="."))){
		for(i in 1:length(donnees)){
			if(nrow(donnees[[i]])>0){
				temp <- c(list(NULL), local.args[setdiff(names(local.args), c("donnees", "FUN"))])
				temp[[1]] <- as.data.frame(donnees[[i]])
				temp[[1]]$date <- as.numeric(temp[[1]]$date)
				donnees[[i]] <- as.data.frame(do.call(FUN, temp))
				if(length(donnees[[i]]) != length(noms))donnees[[i]] <- data.frame(t(donnees[[i]]))
				row.names(donnees[[i]]) <- NULL
#				attributes(donnees[[i]])$mesure <- mesureTemp
#				attributes(donnees[[i]])$station <- stationTemp
#				attributes(donnees[[i]])$unite <- uniteTemp
#				attributes(donnees[[i]])$longitude <- longiTemp
#				attributes(donnees[[i]])$latitude <- latiTemp
#				attributes(donnees[[i]])$lambertx <- xTemp
#				attributes(donnees[[i]])$lamberty <- yTemp
				for(attTemp in setdiff(names(attributesTemp[[i]]), c("names", "row.names", "class")))
					attr(donnees[[i]], attTemp) <- attributesTemp[[i]][[attTemp]]
				class(donnees[[i]]) <- c("Qair", "data.frame")}}
	}else{
		for(i in 1:length(donnees)){
			if(nrow(donnees[[i]])>0){
				temp <- c(list(NULL), local.args[setdiff(names(local.args), c("donnees"))])
				temp[[1]] <- as.data.frame(donnees[[i]])
				temp[[1]]$date <- as.numeric(temp[[1]]$date)
				options(warn=-1)
				donnees[[i]] <- as.data.frame(do.call("sapply", temp))
				options(warn=0)
				if(length(donnees[[i]]) != length(noms))donnees[[i]] <- data.frame(t(donnees[[i]]))
				row.names(donnees[[i]]) <- NULL
#				attributes(donnees[[i]])$mesure <- mesureTemp
#				attributes(donnees[[i]])$station <- stationTemp
#				attributes(donnees[[i]])$unite <- uniteTemp
#				attributes(donnees[[i]])$longitude <- longiTemp
#				attributes(donnees[[i]])$latitude <- latiTemp
#				attributes(donnees[[i]])$lambertx <- xTemp
#				attributes(donnees[[i]])$lamberty <- yTemp
				for(attTemp in setdiff(names(attributesTemp[[i]]), c("names", "row.names", "class")))
					attr(donnees[[i]], attTemp) <- attributesTemp[[i]][[attTemp]]
				class(donnees[[i]]) <- c("Qair", "data.frame")}}}
	for(i in 1:length(donnees))
		names(donnees[[i]]) <- noms
	attributes(donnees) <- attributesTemp2
	attributes(donnees)$aggregation <- if(missing(FUN))"mean" else FUN
	donnees
	}


