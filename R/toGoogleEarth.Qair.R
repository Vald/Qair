toGoogleEarth.Qair <- 
function(x, file, ..., polluant, maxi, coeff=0.5, cote=1/3600/5, cols = colorRampPalette(c("purple", "blue", "cyan", "green", "yellow", "red")), leg.cex=1){
	# coeff : pour la hauteur des barres, 
	if(missing(maxi)){
		maxi <- quantile(x[[polluant]], probs=0.95, na.rm=TRUE)
		truc <- 10^(ceiling(log(maxi, 10))-1)
		maxi <- ceiling(maxi/truc)*truc
		}

	# creation du fichier
	cat("<?xml version='1.0' encoding='UTF-8'?>\n<kml xmlns='http://earth.google.com/kml/2.0'>\n", file=file, sep="")

	# evolution temporelle du polluant

	cat("<Document><open>0</open><name>", stations(x)[names(x)==polluant], ", ",  mesures(x)[names(x)==polluant], " (", unites(x)[names(x)==polluant], ")</name>\n", file=file, sep="", append=TRUE)

	# echelle de couleurs
	png(file=sub(".kml", "echelle.png", file), width=100, height=1000, bg="transparent")
	par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
	plot(NA, xlim=c(0, 100), ylim=c(0, 1000), axes=FALSE, ann=FALSE)
	segments(0, 0:1000, 25, 0:1000, cols(1001))
	text(25, 0:10*100, paste(seq(0, maxi, length=11), "\n", unites(x)[names(x) == polluant]), pos=4, cex=leg.cex)
	dev.off()

	cat('\t\t<ScreenOverlay>\n', file=file, append=T)
	cat('\t\t\t<overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>\n\t\t\t<screenXY x="0" y="0" xunits="fraction" yunits="fraction"/>\n', file=file, append=T)
	cat("\t\t\t<name>echelle</name>\n", file=file, append=T)
	cat('\t\t\t<size x="0" y="1" xunits="fraction" yunits="fraction"/>\n', file=file, append=T)
	cat("\t\t\t<Icon><href>", sub(".kml", "echelle.png", strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])]), "</href></Icon>\n", file=file, append=T, sep="")
	cat("\t\t\t<color>b5ffffff</color>\n", file=file, append=T, sep="")
	cat('\t\t</ScreenOverlay>\n', file=file, append=T)

	# definition des styles
	for(i in 1:101)
		cat('<Style id="conc', i-1, '"><PolyStyle><fill>1</fill><color>ff', substr(cols(101)[i], 6, 7),substr(cols(101)[i], 4, 5),substr(cols(101)[i], 2, 3),'</color></PolyStyle></Style>\n', file=file, sep="", append=TRUE)
	cat('<Style id="concNA"><PolyStyle><color>ffffffff</color><fill>0</fill></PolyStyle></Style>\n', file=file, sep="", append=TRUE)
	cat('<Style id="dot"><IconStyle><scale>1</scale><color>01ffffff</color><Icon><href>http://maps.google.com/mapfiles/kml/shapes/square.png</href></Icon></IconStyle></Style>\n', file=file, append=T)

	# ecriture de chaque 'pas'
	cat("<Folder><open>0</open>\n", file=file, sep="", append=TRUE)

	latitude <- latitudes(x)[names(x) == polluant]
	longitude <- longitudes(x)[names(x) == polluant]
	for(i in 1:nrow(x)){
		if(i == 1){
			debut <- format(as.POSIXlt(x$date[i]-1/24, tz="UTC"), format="%Y-%m-%dT%H:%M:%SZ")
		}else{
			debut <- format(as.POSIXlt(x$date[i-1], tz="UTC"), format="%Y-%m-%dT%H:%M:%SZ")
			}
		fin <- format(as.POSIXlt(x$date[i], tz="UTC"), format="%Y-%m-%dT%H:%M:%SZ")
		when <- format(as.POSIXlt(x$date[i], tz="UTC"), format="%Y%m%d%H")

		donnee <- x[[polluant]][i]

		cat("<Folder><name>", fin, "</name>\n", file=file, append=TRUE)
		cat("<TimeSpan>\n<begin>", debut, "</begin>\n<end>", fin, "</end></TimeSpan>\n", file=file, append=TRUE, sep="")

		cat("<Placemark>\n", file=file, append=TRUE, sep="")
		cat("<name>", donnee, " ", unites(x)[names(x) == polluant], "</name>\n", file=file, append=TRUE, sep="")
		cat("<styleUrl>#dot</styleUrl>\n", file=file, append=TRUE, sep="")
		cat("<Point><coordinates>", longitude, ",", latitude, ",", 1, "</coordinates></Point>\n", file=file, append=TRUE, sep="")
		cat("</Placemark>\n", file=file, append=TRUE, sep="")
	
		cat("<Placemark>", file=file, append=TRUE, sep="")
		cat("<styleUrl>#conc", ifelse(donnee > maxi, 100, round.a(donnee/maxi*100)), "</styleUrl><Polygon><extrude>1</extrude><altitudeMode>relativeToGround</altitudeMode><outerBoundaryIs><LinearRing><coordinates>\n", file=file, append=TRUE, sep="")
		temp <- data.frame(longitude=rep(longitude, 4), latitude=rep(latitude, 4))
		temp <- temp + data.frame(c(-1, 1, 1, -1)*cote, c(-1, -1, 1, 1)*cote)
		for(k in c(1, 2, 3, 4, 1))
			cat(temp$longitude[k], ",", temp$latitude[k], ",", coeff*ifelse(is.na(donnee), 0, donnee) * 1/maxi*100, "\n", file=file, append=TRUE, sep="")
		cat("</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>\n", file=file, append=TRUE, sep="")

		cat("</Folder>\n", file=file, append=TRUE, sep="")
		}
	cat("</Folder>\n", file=file, sep="", append=TRUE)

	cat("</Document>\n", file=file, append=TRUE, sep="")
	cat("</kml>\n", file=file, append=TRUE, sep="")
	}

