`toAn.Qair` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", nbIndiv, ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(!class(donnees)[1] %in% c("qh", "heure", "jour", "hebdo", "mois"))stop("cette fonction n'est applicable qu'aux donnees QH, horaires, journalieres, hebdomadaires et mensuelles")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	donnees$date <- as.numeric(as.character(years(donnees$date-ifelse(nbIndiv>1, 1, 0)/nbIndiv)))
	conversion <- lapply(split(data.frame(!is.na(donnees)), donnees$date), colSums)
	nbJourParAn <- chron(paste("12/31", names(conversion), sep="/"))-chron(paste("01/01", names(conversion), sep="/"))+1
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		donnees <- lapply(split(donnees, donnees$date), colMeans, na.rm=TRUE)
	}else{donnees <- lapply(split(donnees, donnees$date), sapply, FUN, ...)}
	conversion <- data.frame(t(mapply(ifelse, mapply(">=", conversion, floor(pc*nbJourParAn*nbIndiv), SIMPLIFY=FALSE), donnees, MoreArgs=list(no=NA))))
	names(conversion) <- attributesTemp$names
#	conversion$date <- as.character(unique(as.numeric(paste(an, sprintf("%02i", mois), sep="."))))
	conversion$date <- chron(paste("07/01", names(donnees), sep="/"), out.format="y-m-d")
	if(any(!is.na(prec))){
		prec <- c(NA, prec)
		whichPrec <- which(!is.na(prec))
		conversion[whichPrec] <- ifelse(
			mapply("*", conversion[whichPrec], as.list(10^prec[whichPrec]), SIMPLIFY=TRUE) %% 1 == 0.5,
			mapply("/", lapply(mapply("*", conversion[whichPrec], as.list(10^prec[whichPrec]), SIMPLIFY=FALSE), ceiling), as.list(10^prec[whichPrec])),
			mapply(round, conversion[whichPrec], as.list(prec[whichPrec])))
		}
	rownames(conversion) <- NULL
#	attributes(conversion)$mesure <- attributesTemp$mesure
#	attributes(conversion)$station <- attributesTemp$station
	attributes(conversion)$unite <- c("an", attributesTemp$unite[-1])
#	attributes(conversion)$longitude <- attributesTemp$longitude
#	attributes(conversion)$latitude <- attributesTemp$latitude
#	attributes(conversion)$lambertx <- attributesTemp$lambertx
#	attributes(conversion)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class", "unite")))
		attr(conversion, attTemp) <- attributesTemp[[attTemp]]
	class(conversion) <- c("an", "Qair", "data.frame")
	conversion
	}

