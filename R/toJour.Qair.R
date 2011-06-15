`toJour.Qair` <-
function(donnees, pc, prec, FUN="mean", nbIndiv, ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(!class(donnees)[1] %in% c("qh", "heure"))stop("cette fonction n'est applicable qu'aux donnees QH et horaires")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	donnees$date <- floor(as.numeric(donnees$date)-ifelse(nbIndiv>1, 1, 0)/nbIndiv)
	jours <- donnees$date
	conversion <- lapply(split(data.frame(!is.na(donnees)), donnees$date), colSums)
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		donnees <- lapply(split(donnees, donnees$date), colMeans, na.rm=TRUE)
	}else{donnees <- lapply(split(donnees, donnees$date), sapply, FUN, ...)}
	conversion <- data.frame(date=unique(jours), t(mapply(ifelse, lapply(conversion, ">=", pc*nbIndiv), donnees, MoreArgs=list(no=NA)))[,-1])
	conversion$date <- chron(conversion$date, out.format="y-m-d")
	if(any(!is.na(prec))){
		prec <- c(NA, prec)
		whichPrec <- which(!is.na(prec))
		conversion[whichPrec] <- mapply (
					round.a, conversion[whichPrec],
		       			as.list (prec[whichPrec]), SIMPLIFY=FALSE)
		}
#	attributes(conversion)$names <- attributesTemp$names
#	attributes(conversion)$mesure <- attributesTemp$mesure
#	attributes(conversion)$station <- attributesTemp$station
	attributes(conversion)$unite <- c("jour", attributesTemp$unite[-1])
#	attributes(conversion)$longitude <- attributesTemp$longitude
#	attributes(conversion)$latitude <- attributesTemp$latitude
#	attributes(conversion)$lambertx <- attributesTemp$lambertx
#	attributes(conversion)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("row.names", "class", "unite")))
		attr(conversion, attTemp) <- attributesTemp[[attTemp]]
	class(conversion) <- c("jour", "Qair", "data.frame")
	conversion
	}

