`toHeure.Qair` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(class(donnees)[1] != "qh")stop("cette fonction n'est applicable qu'aux donnees QH")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	donnees$date <- ceiling(as.numeric(donnees$date)*24)
	heures <- donnees$date
	conversion <- lapply(split(data.frame(!is.na(donnees)), donnees$date), colSums)
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		donnees <- lapply(split(donnees, donnees$date), colMeans, na.rm=TRUE)
	}else{donnees <- lapply(split(donnees, donnees$date), sapply, FUN, ...)}
#	conversion <- data.frame(t(mapply(ifelse, lapply(conversion, ">=", pc*4), donnees, MoreArgs=list(no=NA))))
	conversion <- data.frame(date=sapply(donnees, "[", "date"), t(mapply(ifelse, lapply(conversion, ">=", pc*4), donnees, MoreArgs=list(no=NA)))[,-1])
	conversion$date <- chron(conversion$date/24)
	conversion$date <- as.character(conversion$date)
	conversion$date <- substr(conversion$date, 2, nchar(conversion$date[1])-1)
	conversion$date <- chron(sapply(strsplit(conversion$date, " "), "[[", 1), sapply(strsplit(conversion$date, " "), "[[", 2), out.format=c("y-m-d", "h:m:s"))
	if(any(!is.na(prec))){
		prec <- c(NA, prec)
		whichPrec <- which(!is.na(prec))
		conversion[whichPrec] <- mapply (
					round.a, conversion[whichPrec],
		       			as.list (prec[whichPrec]), SIMPLIFY=FALSE)
	}
	rownames(conversion) <- NULL
#	attributes(conversion)$mesure <- attributesTemp$mesure
#	attributes(conversion)$station <- attributesTemp$station
	attributes(conversion)$unite <- c("heure", attributesTemp$unite[-1])
#	attributes(conversion)$longitude <- attributesTemp$longitude
#	attributes(conversion)$latitude <- attributesTemp$latitude
#	attributes(conversion)$lambertx <- attributesTemp$lambertx
#	attributes(conversion)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class", "unite")))
		attr(conversion, attTemp) <- attributesTemp[[attTemp]]
	class(conversion) <- c("heure", "Qair", "data.frame")
	conversion
	}
