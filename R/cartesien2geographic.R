'cartesien2geographic' <- 
function(coords, ellps){
	if(ellps=="clrk80"){
		a <- 6378249.2;f <- 1/293.466021
	}else if(ellps=="rgs80"){
		a <- 6378137; f <- 1/298.257222101#257223563
		}
	e <- sqrt(2*f-f**2)
	p <- sqrt(coords$x**2 + coords$y**2)
	r <- sqrt(p**2 + coords$z**2)
	u <- atan((coords$z/p) * ((1-f) + (e**2*a/r)))
	longitudes <- atan(coords$y/coords$x)
	latitudes <- atan((coords$z * (1-f) + e**2 * a *sin(u)**3) / ((1-f) * (p - e**2 * a * cos(u)**3)))
	altitudes <- p * cos(latitudes) + coords$z * sin(latitudes) - a * sqrt(1 - e**2 * sin(latitudes)**2)
	data.frame(longitude=longitudes*180/pi, latitude=latitudes*180/pi, altitude=altitudes)
	}

