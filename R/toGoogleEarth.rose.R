'toGoogleEarth.rose' <-
function(x, file, ..., r, coeff = 1, rlim, col = hsv(0, 1, 1), transparency = TRUE, axis, altitude=1, type=c("n", "h")){
	#	library(proj4)
	pas = 5
	rmin <- min(rlim)
	rmax <- max(rlim)
	if(missing(axis))axis <- round(seq(0, rmax, length=5)[-1])
	type <- match.arg(type)

	lambertIIx <- lambertx(x[[1]])[names(x)==r]
	lambertIIy <- lamberty(x[[1]])[names(x)==r]

	col <- sub("#", "", col)
	col <- sprintf("%s%s%s%s", ifelse(transparency, "a1", "ff"), substr(col, 5, 6), substr(col, 3, 4), substr(col, 1, 2))

	inter <- attributes(x)$intervalle
	rose <- as.list(x)

	rose <- data.frame(matrix(unlist(lapply(rose, "[", r)), ncol=2, byrow=TRUE))
	names(rose)[2] <- "values"
	rose$angled <- sapply(inter, "[", 1)
	rose$anglef <- sapply(inter, "[", 2)
	rose[nrow(rose), "anglef"] <- rose[2,"angled"]+360

	coord <- list(x=numeric(), y=numeric())
	if(type=="n") {
		for(i in 2:nrow(rose)){
			alpha <- seq(rose$angled[i], rose$anglef[i], by=pas)
			coord$x <- c(coord$x, coeff*cos(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]>rmin, rose$values[i], rmin))-rmin))
			coord$y <- c(coord$y, -coeff*sin(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]>rmin, rose$values[i], rmin))-rmin))
			}
	} else {
		for(i in 2:nrow(rose)){
			alpha <- seq(rose$angled[i], rose$anglef[i], by=pas)
			coord$x <- c(coord$x, coeff*cos(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]>0, rose$values[i], 0))-rmin))
			coord$y <- c(coord$y, -coeff*sin(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]>0, rose$values[i], 0))-rmin))
			}
		for(i in 2:nrow(rose)){
			alpha <- seq(rose$angled[i], rose$anglef[i], by=pas)
			coord$x <- c(coord$x, coeff*cos(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]<0, rose$values[i], 0))-rmin))
			coord$y <- c(coord$y, -coeff*sin(((alpha-90)%%360)*pi/180)*(ifelse(is.na(rose$values[i]), 0, ifelse(rose$values[i]<0, rose$values[i], 0))-rmin))
			}
		}
	coord$x <- coord$x + lambertIIx
	coord$y <- coord$y + lambertIIy
##	coord[c("lat", "lon")] <- lambertII2geo(coord$x, coord$y)
#	temp <- project(list(coord$x, coord$y), inverse=TRUE, "+proj=lcc +lat_1=45.89891889 +lat_2=47.69601444 +lat_0=46.8 +lon_0=2.33722917 +x_0=600000 +y_0=2200000 +a=6378249.145000 +b=6356514.869550")
#	coord[c("lat", "lon")] <- ntf2rgf93(temp$x, temp$y)[c("latitude", "longitude")]
	temp <- proj2geo(coord$x, coord$y, 'Lambert II etendu', 'WGS84')
	coord[c('lat', 'lon')] <- temp[c('latitude', 'longitude')]


	temp1 <- coeff*cos(0:72*5*pi/180)*(rmax-rmin) + lambertIIx
	temp2 <- -coeff*sin(0:72*5*pi/180)*(rmax-rmin) + lambertIIy
##	coord[c("tourlat", "tourlon")] <- lambertII2geo(temp1, temp2)
#	temp <- project(list(temp1, temp2), inverse=TRUE, "+proj=lcc +lat_1=45.89891889 +lat_2=47.69601444 +lat_0=46.8 +lon_0=2.33722917 +x_0=600000 +y_0=2200000 +a=6378249.145000 +b=6356514.869550")
#	coord[c("tourlat", "tourlon")] <- ntf2rgf93(temp$x, temp$y)[c("latitude", "longitude")]
	coord[c("tourlat", "tourlon")] <- proj2geo(temp1, temp2, 'Lambert II etendu', 'WGS84')[c('latitude', 'longitude')]

	for(i in 1:length(axis)){
		temp1 <- coeff*cos(0:72*5*pi/180)*(axis[i]-rmin) + lambertIIx
		temp2 <- -coeff*sin(0:72*5*pi/180)*(axis[i]-rmin) + lambertIIy
##		coord[c(sprintf("lat%i", i), sprintf("lon%i", i))] <- lambertII2geo(temp1, temp2)
#		temp <- project(list(temp1, temp2), inverse=TRUE, "+proj=lcc +lat_1=45.89891889 +lat_2=47.69601444 +lat_0=46.8 +lon_0=2.33722917 +x_0=600000 +y_0=2200000 +a=6378249.145000 +b=6356514.869550")
#		coord[c(sprintf("lat%i", i), sprintf("lon%i", i))] <- ntf2rgf93(temp$x, temp$y)[c("latitude", "longitude")]
		coord[c(sprintf("lat%i", i), sprintf("lon%i", i))] <- proj2geo(temp1, temp2, 'Lambert II etendu', 'WGS84')[c('latitude', 'longitude')]
		}
	cat("<?xml version='1.0' encoding='UTF-8'?>\n<kml xmlns='http://earth.google.com/kml/2.2'>\n", file=file, sep="")
	cat("<Document><open>0</open><name>roses de concentrations</name>\n", file=file, sep="", append=TRUE)
	cat('<Style id="rose">\n', file=file, sep="", append=TRUE)
	cat('<PolyStyle><fill>1</fill><color>', tolower(col), '</color></PolyStyle>\n', file=file, sep="", append=TRUE)
	cat('<LineStyle><color>ff000000</color></LineStyle>\n', file=file, sep="", append=TRUE)
	cat('</Style>\n', file=file, sep="", append=TRUE)

	cat('<Style id="bg">\n', file=file, sep="", append=TRUE)
	cat('<PolyStyle><fill>1</fill><color>a1ffffff</color></PolyStyle>\n', file=file, sep="", append=TRUE)
	cat('<LineStyle><color>ff000000</color></LineStyle>\n', file=file, sep="", append=TRUE)
	cat('</Style>\n', file=file, sep="", append=TRUE)

	cat('<Style id="axe">\n', file=file, sep="", append=TRUE)
	cat('<PolyStyle><fill>0</fill></PolyStyle>\n', file=file, sep="", append=TRUE)
	cat('<LineStyle><color>ffa1a1a1</color></LineStyle>\n', file=file, sep="", append=TRUE)
	cat('</Style>\n', file=file, sep="", append=TRUE)

	cat('<Style id="echelle">\n', file=file, sep="", append=TRUE)
	cat('<IconStyle><scale>0</scale></IconStyle>\n', file=file, sep="", append=TRUE)
	cat('<LabelStyle><color>ffffffff</color></LabelStyle>\n', file=file, sep="", append=TRUE)
	cat('</Style>\n', file=file, sep="", append=TRUE)

	cat("<Placemark>\n", file=file, append=TRUE, sep="")
	cat("<styleUrl>#rose</styleUrl>\n", file=file, append=TRUE, sep="")
	cat("<Polygon><extrude>1</extrude><altitudeMode>relativeToGround</altitudeMode>", file=file, append=TRUE, sep="")
	cat("<outerBoundaryIs><LinearRing><coordinates>\n", file=file, append=TRUE, sep="")
	for(i in c(1, length(coord$x):1))
		cat(coord$lon[i], ",", coord$lat[i], ",", altitude + 2, " ", file=file, append=TRUE, sep="")
	cat("</coordinates></LinearRing></outerBoundaryIs>\n", file=file, append=TRUE, sep="")
	cat("</Polygon></Placemark>\n", file=file, append=TRUE, sep="")

	cat("<Placemark>\n", file=file, append=TRUE, sep="")
	cat("<styleUrl>#bg</styleUrl>\n", file=file, append=TRUE, sep="")
	cat("<Polygon><extrude>0</extrude><altitudeMode>relativeToGround</altitudeMode>", file=file, append=TRUE, sep="")
	cat("<outerBoundaryIs><LinearRing><coordinates>\n", file=file, append=TRUE, sep="")
	for(i in length(coord$tourlat):1)
		cat(coord$tourlon[i], ",", coord$tourlat[i], ",", altitude, " ", file=file, append=TRUE, sep="")
	cat("</coordinates></LinearRing></outerBoundaryIs>\n", file=file, append=TRUE, sep="")
	cat("</Polygon></Placemark>\n", file=file, append=TRUE, sep="")

	cat("<Document>\n", file=file, append=TRUE, sep="")
	for(j in 1:length(axis)){
		cat("<Placemark>\n", file=file, append=TRUE, sep="")
		cat("<styleUrl>#axe</styleUrl>\n", file=file, append=TRUE, sep="")
		cat("<Polygon><extrude>0</extrude><altitudeMode>relativeToGround</altitudeMode>", file=file, append=TRUE, sep="")
		cat("<outerBoundaryIs><LinearRing><coordinates>\n", file=file, append=TRUE, sep="")
		for(i in length(coord$tourlat):1)
			cat(coord[[sprintf("lon%i", j)]][i], ",", coord[[sprintf("lat%i", j)]][i], ",", altitude+1," ", file=file, append=TRUE, sep="")
		cat("</coordinates></LinearRing></outerBoundaryIs>\n", file=file, append=TRUE, sep="")
		cat("</Polygon></Placemark>\n", file=file, append=TRUE, sep="")
		}
	cat("</Document>\n", file=file, append=TRUE, sep="")
	cat("<Document>\n", file=file, append=TRUE, sep="")
	for(j in 1:length(axis)){
		cat("<Placemark>\n", file=file, append=TRUE, sep="")
		cat("<name>", axis[j], " microg/m3</name>\n", file=file, append=TRUE, sep="")
		cat("<styleUrl>#echelle</styleUrl>\n", file=file, append=TRUE, sep="")
		cat("<Point><altitudeMode>relativeToGround</altitudeMode>", file=file, append=TRUE, sep="")
		cat("<coordinates>", coord[[sprintf("lon%i", j)]][5], ",", coord[[sprintf("lat%i", j)]][5], ",", altitude+2, " ", "</coordinates>\n", file=file, append=TRUE, sep="")
		cat("</Point></Placemark>\n", file=file, append=TRUE, sep="")
		}
	cat("</Document>\n", file=file, append=TRUE, sep="")
	cat("</Document>\n", file=file, sep="", append=TRUE)
	cat("</kml>\n", file=file, append=TRUE, sep="")
	}
