'ntf2rgf93' <- 
function(longitudes, latitudes, inv=FALSE){
	data(gr3df97a)
#	grille <- read.table("R:/Etudes/Scripts/Qair/Qair/data/gr3df97a.txt", skip=4, header=FALSE)
#	grille <- grille[-1]
#	names(grille) <- c("longitude", "latitude", "Tx", "Ty", "Tz", "prec", "f50")
	grille <- gr3df97a

	if(!inv){
		Tm <- c(-168, -60, 320)
		lonNTF <- longitudes
		latNTF <- latitudes
		}
	while(TRUE){
		if(!inv){
			coords <- geographic2cartesien(lonNTF, latNTF, ellps="clrk80")
			coords <- coords + Tm
			resultat <- cartesien2geographic(coords, ellps="rgs80")
			if(all(
				(longitudes == resultat$longitude & latitudes == resultat$latitude) |
				(longitudes - resultat$longitude < 1e-15 & latitudes - resultat$latitude < 1e-15) |
				is.na(longitudes) | is.na(latitudes)))break()
			longitudes <- resultat$longitude
			latitudes <- resultat$latitude
			}

		# recherche des mailles correspondant a chaque point
		maille <- list(
			minlon = apply(outer(floor(longitudes*10)/10, grille$longitude, "=="), 1, which),
			maxlon = apply(outer(ceiling(longitudes*10)/10, grille$longitude, "=="), 1, which),
			minlat = apply(outer(floor(latitudes*10)/10, grille$latitude, "=="), 1, which),
			maxlat = apply(outer(ceiling(latitudes*10)/10, grille$latitude, "=="), 1, which))

		mailles <- matrix(NA, ncol=4, nrow=length(longitudes))
		if(length(longitudes) > 1){
			temp <- mapply(intersect, as.data.frame(maille$minlon), as.data.frame(maille$minlat), SIMPLIFY=FALSE)
			mailles[, 1] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- mapply(intersect, as.data.frame(maille$minlon), as.data.frame(maille$maxlat), SIMPLIFY=FALSE)
			mailles[, 2] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- mapply(intersect, as.data.frame(maille$maxlon), as.data.frame(maille$minlat), SIMPLIFY=FALSE)
			mailles[, 3] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- mapply(intersect, as.data.frame(maille$maxlon), as.data.frame(maille$maxlat), SIMPLIFY=FALSE)
			mailles[, 4] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
		}else{
			temp <- intersect(maille$minlon, maille$minlat)
			mailles[, 1] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- intersect(maille$minlon, maille$maxlat)
			mailles[, 2] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- intersect(maille$maxlon, maille$minlat)
			mailles[, 3] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			temp <- intersect(maille$maxlon, maille$maxlat)
			mailles[, 4] <- unlist(ifelse(sapply(temp, length) == 1, temp, NA))
			}
		# interpolation bilineaire
		x <- (longitudes - grille$longitude[mailles[,1]]) / (grille$longitude[mailles[,3]] - grille$longitude[mailles[,1]])
		y <- (latitudes - grille$latitude[mailles[,1]]) / (grille$latitude[mailles[,2]] - grille$latitude[mailles[,1]])
		Tm <- (1 - x) * (1 - y) * grille[mailles[,1], c("Tx", "Ty", "Tz")] + (1 - x) * y * grille[mailles[,2], c("Tx", "Ty", "Tz")] + x * (1 - y) * grille[mailles[,3], c("Tx", "Ty", "Tz")] + x * y * grille[mailles[,4], c("Tx", "Ty", "Tz")]

		if(inv){
			# on passe en cartesien
			coords <- geographic2cartesien(longitudes, latitudes, ellps="rgs80")
			# on corrige
			coords <- coords - Tm
			resultat <- cartesien2geographic(coords, ellps="clrk80")
			break()
			}
		}
	resultat
	}

