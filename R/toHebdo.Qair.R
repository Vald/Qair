`toHebdo.Qair` <-
function(donnees, pc=0.75, prec, FUN="mean", nbIndiv, ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(!class(donnees)[1] %in% c("qh", "heure", "jour"))stop("cette fonction n'est applicable qu'aux donnees QH, horaires et journalieres")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	datesTemp <- donnees$date
	an <- as.numeric(as.character(years(donnees$date-ifelse(nbIndiv>1, 1, 0)/nbIndiv)))
	jourAn <- (chron(floor(as.numeric(donnees$date)-ifelse(nbIndiv>1, 1, 0)/nbIndiv))-chron(paste("01/01", an ,sep="/"), format="d/m/y"))
	premierDimanche <- (apply(sapply(lapply(lapply(lapply(1:7, paste, "01", an, sep="/"), chron, format=c("d/m/y")), "weekdays"), "==", "Sun"), 1, which))
	semaine <- (jourAn  + 7 - premierDimanche)%/%7+1
	semaine <- paste(an, sprintf("%02i", semaine), sep=".")
	donnees$date <- as.numeric(semaine)
	conversion <- lapply(split(data.frame(!is.na(donnees)), donnees$date), colSums)
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		donnees <- lapply(split(donnees, donnees$date), colMeans, na.rm=TRUE)
	}else{donnees <- lapply(split(donnees, donnees$date), sapply, FUN, ...)}
	conversion <- data.frame(t(mapply(ifelse, lapply(conversion, ">=", pc*7*nbIndiv), donnees, MoreArgs=list(no=NA))))
#	conversion$date <- as.character(unique(as.numeric(paste(an, sprintf("%02i", semaine), sep="."))))
	conversion$date <- chron(floor(tapply(datesTemp, semaine, mean)), out.format=c("y-m-d"))
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
	attributes(conversion)$unite <- c("semaine", attributesTemp$unite[-1])
#	attributes(conversion)$longitude <- attributesTemp$longitude
#	attributes(conversion)$latitude <- attributesTemp$latitude
#	attributes(conversion)$lambertx <- attributesTemp$lambertx
#	attributes(conversion)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class", "unite")))
		attr(conversion, attTemp) <- attributesTemp[[attTemp]]
	class(conversion) <- c("hebdo", "Qair", "data.frame")
	conversion
	}

