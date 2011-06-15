'toGoogleEarth.default'<-
function(x, file, ..., description=NULL, picto=NULL, maxi=NULL, cols = colorRampPalette(c("purple", "blue", "cyan", "green", "yellow", "red")), unite=NULL, coeff=0.5, cote=1/3600/5,
leg.cex=1, leg.width=150, leg.bg='white'){
	longitude<- x$longitude
	latitude <- x$latitude
	# coordonnees : en degrés décimaux
	# extension du fichier : ".kml"
	if(is.null(description))description <- rep(NA, length(longitude))
	
	# debut du fichier
	cat("<?xml version='1.0' encoding='UTF-8'?>\n<kml xmlns='http://earth.google.com/kml/2.0'>\n\t<Folder><open>0</open>\n", file=file)
	cat("\t\t<Document>\n", file=file, append=T, sep="")
	if(!is.null(x$value)){
		if(is.null(maxi)){
			maxi <- quantile(x$value, probs=0.95, na.rm=TRUE)
			truc <- 10^(ceiling(log(maxi, 10))-1)
			maxi <- ceiling(maxi/truc)*truc
			}
		# echelle de couleurs
		png(file=sub(".kml", "echelle.png", file), width=leg.width, height=1000, bg=leg.bg)
		par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
		plot(NA, xlim=c(0, 100), ylim=c(0, 1000), axes=FALSE, ann=FALSE)
		segments(0, 0:1000, 25, 0:1000, cols(1001))
		text(25, 0:10*100, paste(seq(0, maxi, length=11), "\n", if(!is.null(unite)) unite else ""), pos=4, cex=leg.cex)
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

		for(i in 1:length(longitude)){
			# ecriture de chaque 'pas'
			donnee <- x$value[i]

			#cat("<Folder>\n", file=file, append=TRUE)
		
			#cat("<Placemark>\n", file=file, append=TRUE, sep="")
			#cat("<name>", donnee, " ", unites(x)[names(x) == polluant], "</name>\n", file=file, append=TRUE, sep="")
			#cat("<styleUrl>#dot</styleUrl>\n", file=file, append=TRUE, sep="")
			#cat("<Point><coordinates>", longitude[i], ",", latitude[i], ",", 1, "</coordinates></Point>\n", file=file, append=TRUE, sep="")
			#cat("</Placemark>\n", file=file, append=TRUE, sep="")
	
			cat("<Placemark>", file=file, append=TRUE, sep="")
			cat("<styleUrl>#conc", ifelse(donnee > maxi, 100, round.a(donnee/maxi*100)), "</styleUrl><Polygon><extrude>1</extrude><altitudeMode>relativeToGround</altitudeMode><outerBoundaryIs><LinearRing><coordinates>\n", file=file, append=TRUE, sep="")
			temp <- data.frame(longitude=rep(longitude[i], 4), latitude=rep(latitude[i], 4))
			temp <- temp + data.frame(c(-1, 1, 1, -1)*cote, c(-1, -1, 1, 1)*cote)
			for(k in c(1, 2, 3, 4, 1))
				cat(temp$longitude[k], ",", temp$latitude[k], ",", coeff*ifelse(is.na(donnee), 0, donnee) * 1/maxi*100, "\n", file=file, append=TRUE, sep="")
			cat("</coordinates></LinearRing></outerBoundaryIs></Polygon></Placemark>\n", file=file, append=TRUE, sep="")
			#cat("</Folder>\n", file=file, append=TRUE, sep="")
			}
		cat("\t\t</Document>\n", file=file, append=T, sep="")
		cat("</Folder>\n", file=file, sep="", append=TRUE)
		#cat("</Document>\n", file=file, append=TRUE, sep="")
		cat("</kml>\n", file=file, append=TRUE, sep="")

	}else{
		if(!is.null(picto) && file.exists(picto))
			cat('\t\t\t<Style id="picto"><IconStyle><Icon><href>', picto, '</href></Icon></IconStyle></Style>\n', file=file, append=T, sep="")

		# affichage des points
		cat("\t\t\t<name>points</name>\n", file=file, append=T)
		for(i in 1:length(longitude)){
			cat("\t\t\t<Placemark><name>", if(is.na(description[i])) "" else description[i], "</name>", file=file, append=T, sep="")
			if(!is.null(picto))cat("<styleUrl>#picto</styleUrl>", file=file, append=T, sep="")
			cat("<Point><coordinates>", longitude[i], ",", latitude[i], "</coordinates></Point></Placemark>\n", file=file, append=T, sep="")
			}
		cat("\t\t</Document>\n", file=file, append=T, sep="")
		cat("\t</Folder>\n</kml>", file=file, append=T)
		}
	}
