'geographic2cartesien'<-
function(longitude, latitude, ellps, altitude = 0){
	if(ellps=="clrk80"){
		a <- 6378249.2;f <- 1/293.466021294
	}else if(ellps=="rgs80"){
		a <- 6378137; f <- 1/298.257222101#257223563
		}
	e <- sqrt(2*f-f**2)
	nu <- a / sqrt(1 - e**2 * sin(latitude * pi/180)**2)
	X <- (nu + altitude) * cos(latitude * pi/180) * cos(longitude * pi/180)
	Y <- (nu + altitude) * cos(latitude * pi/180) * sin(longitude * pi/180)
	Z <- ((1 - e**2) * nu + altitude) * sin(latitude * pi/180)
	data.frame(x=X, y=Y, z=Z)
	}

