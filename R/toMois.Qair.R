`toMois.Qair` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", nbIndiv, ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(!class(donnees)[1] %in% c("qh", "heure", "jour"))stop("cette fonction n'est applicable qu'aux donnees QH, horaires et journalieres")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	longMois <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	longMoisBis <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	an <- as.numeric(as.character(years(donnees$date-ifelse(nbIndiv>1, 1, 0)/nbIndiv)))
	mois <- as.numeric(months(as.numeric(donnees$date)-ifelse(nbIndiv>1, 1, 0)/nbIndiv))
	donnees$date <- as.character(paste(an, sprintf("%02i", mois), sep="."))
	conversion <- lapply(split(data.frame(!is.na(donnees)), donnees$date), colSums)
	nbJourParMois <- ifelse(
		chron(paste("12/31", substr(names(conversion), 1, 4), sep="/"))-chron(paste("01/01", substr(names(conversion), 1, 4), sep="/"))==365,
		longMoisBis[as.numeric(substr(names(conversion), 6, 7))], longMois[as.numeric(substr(names(conversion), 6, 7))])
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		donnees <- lapply(split(data.frame(date=as.numeric(donnees[,1]), donnees[,-1]), donnees$date), colMeans, na.rm=TRUE)
	}else{donnees <- lapply(split(data.frame(date=as.numeric(donnees[,1]), donnees[,-1]), donnees$date), sapply, FUN, ...)}
	conversion <- data.frame(t(mapply(ifelse, mapply(">=", conversion, pc*nbJourParMois*nbIndiv, SIMPLIFY=FALSE), donnees, MoreArgs=list(no=NA))))
#	conversion$date <- as.character(unique(as.numeric(paste(an, sprintf("%02i", mois), sep="."))))
	conversion$date <- chron(paste(substr(names(donnees), 6, 7), round(0+nbJourParMois/2), substr(names(donnees), 1, 4), sep="/"), out.format="y-m-d")
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
	attributes(conversion)$unite <- c("mois", attributesTemp$unite[-1])
#	attributes(conversion)$longitude <- attributesTemp$longitude
#	attributes(conversion)$latitude <- attributesTemp$latitude
#	attributes(conversion)$lambertx <- attributesTemp$lambertx
#	attributes(conversion)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class", "unite")))
		attr(conversion, attTemp) <- attributesTemp[[attTemp]]
	class(conversion) <- c("mois", "Qair", "data.frame")
	conversion
	}

