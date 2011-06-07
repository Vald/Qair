`toPAn.Qair` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", mois, ...){
	if(!inherits(donnees, "Qair"))stop("donnees inadequates (n'est pas de la classe Qair)")
	if(!class(donnees)[1] %in% c("qh", "heure", "jour", "mois"))stop("cette fonction n'est applicable qu'aux donnees QH, horaires, journalieres et mensuelles")
#	if(length(unique(mois)) != 12)stop("pas suffisamment de donnees pour realiser un profil annuel")
	local.call <- match.call()
	if(length(prec) == 1){prec <- rep(prec, length(donnees)-1)
	}else if(length(prec)!=length(donnees)-1){prec[TRUE] <- NA;warning("vecteur des precisions inadequat, donc non utilise")}

	mois <- apply(outer(as.character(mois), c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), "=="), 1, which)
	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	if(eval(local.call$FUN) == "mean" & !"na.rm"%in%names(local.call)){
		profil <- mapply(ifelse,
			lapply(mapply("/", lapply(data.frame(!is.na(donnees)), tapply, mois, sum), lapply(donnees, tapply, mois, length), SIMPLIFY=FALSE), ">=", pc),
			data.frame(date=sort(unique(mois)), lapply(donnees[-1], tapply, mois, "mean", na.rm=TRUE)),
			MoreArgs=list(no=NA), SIMPLIFY=FALSE)
	}else{
		profil <- mapply(ifelse,
			lapply(mapply("/", lapply(data.frame(!is.na(donnees)), tapply, mois, sum), lapply(donnees, tapply, mois, length), SIMPLIFY=FALSE), ">=", pc),
			data.frame(date=sort(unique(mois)), lapply(donnees[-1], tapply, mois, FUN, ...)),
			MoreArgs=list(no=NA), SIMPLIFY=FALSE)}
	profil <- data.frame(profil)
	if(any(!is.na(prec))){
		prec <- c(NA, prec)
		whichPrec <- which(!is.na(prec))
		profil[whichPrec] <- ifelse(
			mapply("*", profil[whichPrec], as.list(10^prec[whichPrec]), SIMPLIFY=TRUE) %% 1 == 0.5,
			mapply("/", lapply(mapply("*", profil[whichPrec], as.list(10^prec[whichPrec]), SIMPLIFY=FALSE), ceiling), as.list(10^prec[whichPrec])),
			mapply(round, profil[whichPrec], as.list(prec[whichPrec])))
		}
	temp <- profil
	profil <- as.data.frame(matrix(NA, nrow=12, ncol=length(temp)))
	names(profil) <- attributesTemp$names
	profil$date <- 1:12
	profil[profil$date %in% temp$date,-1] <- temp[-1]
	rownames(profil) <- NULL
#	attributes(profil)$mesure <- attributesTemp$mesure
#	attributes(profil)$station <- attributesTemp$station
	attributes(profil)$unite <- c("mois", attributesTemp$unite[-1])
#	attributes(profil)$longitude <- attributesTemp$longitude
#	attributes(profil)$latitude <- attributesTemp$latitude
#	attributes(profil)$lambertx <- attributesTemp$lambertx
#	attributes(profil)$lamberty <- attributesTemp$lamberty
	for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class", "unite")))
		attr(profil, attTemp) <- attributesTemp[[attTemp]]
	class(profil) <- c("pannuel", "Qair", "data.frame")
	profil
	}

