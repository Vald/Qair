'Xair2R' <-
function(polluants, dated, datef, dt = c("qh", "heure", "jour", "mois", "an"), merging, codeV=c("A", "R", "O", "W"), dsn=NULL, uid=NULL, pwd=NULL, brute=FALSE, reseaux=NULL, stations=NULL, campagnes=NULL, host=NULL, keep.state=FALSE, XR6=TRUE){
#172.16.19.33:1521:N09

	if(missing(merging)) {
		dt <- match.arg(dt)
		existMerging <- FALSE
	} else {
		existMerging <- TRUE
		dt <- unites(merging)[1]
		if(missing(dated))
			dated <- as.character(chron(floor(merging$date[1]), out.format="y-m-d"))
		if(missing(datef))
			datef <- as.character(chron(floor(merging$date[nrow(merging)]-ifelse(dt=="jour", 0, 1)), out.format="y-m-d"))
		}

	dated <- format(as.Date(chron(dated, format="y-m-d")), format="%Y-%m-%d")
	datef <- as.character(chron(datef, format="y-m-d")+1)
	datef <- format(as.Date(chron(datef, format="y-m-d")), format="%Y-%m-%d")

	# definition de l'hote (unix like system)
	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption("Xair.host")) & .Platform$OS.type=="unix") {
		cat("hote hebergeant la base de donnees :\n");options(Xair.host=scan(what="character", nlines=1));cat("\n")
		}

	# definition de la base de donnees (dsn)
	if(!is.null(dsn)) {
		options(Xair.dsn=dsn)
	} else if(is.null(getOption("Xair.dsn")) & .Platform$OS.type=="unix") {
		cat("nom de la base de donnees (DataSourceName) :\n");options(Xair.dsn=scan(what="character", nlines=1));cat("\n")
	} else if(is.null(getOption("Xair.dsn")) & .Platform$OS.type=="windows") {
		cat("nom de la base de donnees (DataSourceName) :\n", names(odbcDataSources("system")), "\n");options(Xair.dsn=scan(what="character", nlines=1));cat("\n")
		}

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption("Xair.uid"))) {
		cat("identifiant pour la connection :\n");options(Xair.uid=scan(what="character", nlines=1));cat("\n")
		}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption("Xair.pwd"))) {
		cat("mot de passe pour la connection :\n");options(Xair.pwd=scan(what="character", nlines=1));cat("\n")
		}

	# connection a la base
	if(.Platform$OS.type=="windows") {
		conxair <- try(odbcConnect(getOption("Xair.dsn"), uid = getOption("Xair.uid"), pwd = getOption("Xair.pwd"), case = "nochange", believeNRows = TRUE))
		if(inherits(conxair, "try-error")) stop("echec de la connection a la base Xair.")
	} else if(.Platform$OS.type=="unix") {
#		drv <- JDBC("oracle.jdbc.driver.OracleDriver", "/usr/lib/jvm/java-6-openjdk/jre/lib/ext/ojdbc14.jar")
		if(!is.null(options()$Xair.ojdbc.file)) {
			drv <- JDBC("oracle.jdbc.OracleDriver", options()$Xair.ojdbc.file)
		} else {
			cat('Veuillez entrer le chemin vers le fichier java ojdbc*.jar\nAfin de ne pas avoir a renseigner ce chemin a chaque session\nvous pouvez definir la variable options(Xair.ojdbc.file = ...) dans le fichier ~/.Rprofile\n')
			drv <- scan(nmax = 1, what = 'character')
			}
		conxair <- try(dbConnect(drv, paste("jdbc:oracle:thin:@", getOption("Xair.host"), ":1521:", getOption("Xair.dsn"), sep=""), getOption("Xair.uid"), getOption("Xair.pwd"), identifer.quote="'"))
		if(inherits(conxair, "try-error")) stop("echec de la connection a la base Xair.")
	} else {
		stop("platforme non reconnue")}

	# recuperation des donnees a partir de reseaux de mesures ou de stations
	if(missing(polluants)) {
		if(is.null(reseaux) & is.null(stations) & is.null(campagnes))stop("pas de donnees specifiees")
		polluants <- NULL
		}

	# recuperation des mesures par reseau si 'reseaux' defini
	if(!is.null(reseaux)) {
		query <- paste("select NOM_COURT_MES from RESEAUMES where NOM_COURT_RES IN('", paste(reseaux, collapse="', '"), "')", sep="")
		if(.Platform$OS.type=="windows") {
			polluants <- c(polluants, as.character(sqlQuery(conxair, query)$NOM_COURT_MES))
		} else if(.Platform$OS.type=="unix") {
			polluants <- c(polluants, as.character(dbGetQuery(conxair, query)$NOM_COURT_MES))
			}
		}
	# recuperation des mesures par campagne si 'campagnes' defini
	if(!is.null(campagnes)) {
		query <- paste("select NOM_COURT_SIT from CAMPMES_STATION where NOM_COURT_CM IN('", paste(campagnes, collapse="', '"), "')", sep="")
		if(.Platform$OS.type=="windows") {
			stations <- c(stations, as.character(sqlQuery(conxair, query)$NOM_COURT_SIT))
		} else if(.Platform$OS.type=="unix") {
			stations <- c(stations, as.character(dbGetQuery(conxair, query)$NOM_COURT_SIT))
			}
		}
	# recuperation des mesures par station si 'stations' defini
	if(!is.null(stations)) {
		query <- paste("select NOM_COURT_MES from STATION, MESURE where STATION.NOM_COURT_SIT IN ('", paste(stations, collapse="', '"), "') and STATION.NOM_COURT_SIT = MESURE.NOM_COURT_SIT", sep="")
		if(.Platform$OS.type=="windows") {
			polluants <- c(polluants, as.character(sqlQuery(conxair, query)$NOM_COURT_MES))
		} else if(.Platform$OS.type=="unix") {
			polluants <- c(polluants, as.character(dbGetQuery(conxair, query)$NOM_COURT_MES))
		}
	}

	#recuperation des donnees
	nomsPol <- paste("'", paste(polluants, collapse="', '"), "'", sep="")
	if(dt=="qh") {
		champs <- paste("Q_M", sprintf("%02i", 1:96), sep="", collapse=", ")
		query <- paste("SELECT NOM_COURT_MES, TO_CHAR(J_DATE, 'YYYY-MM-DD'), Q_ETATV, ", champs, " FROM ", ifelse(brute, "BRUTE", "JOURNALIER"), " WHERE NOM_COURT_MES IN (", nomsPol, ") AND J_DATE BETWEEN TO_DATE('", dated, "', 'YY-MM-DD') AND TO_DATE('", datef, "', 'YY-MM-DD')", sep="")
	} else if(dt=="heure") {
		champs <- paste("H_M", sprintf("%02i", 1:24), sep="", collapse=", ")
		query <- paste("SELECT NOM_COURT_MES, TO_CHAR(J_DATE, 'YYYY-MM-DD'), H_ETAT, ", champs, " FROM ", ifelse(brute, "BRUTE", "JOURNALIER"), " WHERE NOM_COURT_MES IN (", nomsPol, ") AND J_DATE BETWEEN TO_DATE('", dated, "', 'YY-MM-DD') AND TO_DATE('", datef, "', 'YY-MM-DD')", sep="")
	} else if(dt=="jour") {
		champs <- "J_M01"
		query <- paste("SELECT NOM_COURT_MES, TO_CHAR(J_DATE, 'YYYY-MM-DD'), J_ETAT, ", champs, " FROM ", ifelse(brute, "BRUTE", "JOURNALIER"), " WHERE NOM_COURT_MES IN (", nomsPol, ") AND J_DATE BETWEEN TO_DATE('", dated, "', 'YY-MM-DD') AND TO_DATE('", datef, "', 'YY-MM-DD')", sep="")
	} else if(dt=="mois") {
		if(brute)cat("Il n'y a pas de donnees brutes mensuelles\n")
		datedT <- as.character(chron(paste("01/01", strsplit(dated, "-")[[1]][1], sep="/"), out.format="y-m-d"))
		datedT <- format(as.Date(chron(datedT, format="y-m-d")), format="%Y-%m-%d")
		champs <- paste("M_M", sprintf("%02i", 1:12), sep="", collapse=", ")
		query <- paste("SELECT NOM_COURT_MES, TO_CHAR(M_DATE, 'YYYY-MM-DD'), M_ETAT, ", champs, " FROM MOIS WHERE NOM_COURT_MES IN (", nomsPol, ") AND M_DATE BETWEEN TO_DATE('", datedT, "', 'YY-MM-DD') AND TO_DATE('", datef, "', 'YY-MM-DD')", sep="")
	} else if(dt=="an") {
		if(brute)cat("Il n'y a pas de donnees brutes annuelles\n")
		datedT <- as.character(chron(paste("01/01", strsplit(dated, "-")[[1]][1], sep="/"), out.format="y-m-d"))
		datedT <- format(as.Date(chron(datedT, format="y-m-d")), format="%Y-%m-%d")
		champs <- "A_M01"
		query <- paste("SELECT NOM_COURT_MES, TO_CHAR(M_DATE, 'YYYY-MM-DD'), A_ETAT, ", champs, " FROM MOIS WHERE NOM_COURT_MES IN (", nomsPol, ") AND M_DATE BETWEEN TO_DATE('", datedT, "', 'YY-MM-DD') AND TO_DATE('", datef, "', 'YY-MM-DD')", sep="")}

	if(.Platform$OS.type=="windows") {
		donnees <- sqlQuery(conxair, query)
	} else if(.Platform$OS.type=="unix") {
		donnees <- dbGetQuery(conxair, query)
		}

	# mise en forme des donnees
	names(donnees)[1:3] <- c("nom_court_mes", "date", "etat")
	donnees[1:3] <- lapply(donnees[1:3], as.character)
	donnees <- split(donnees[-1], donnees$nom_court_mes)
	donnees <- lapply(donnees, as.list)

	for(i in names(donnees)) {
		nb <- length(donnees[[i]])-2
		nb2 <- length(donnees[[i]][[1]])
		nb3 <- length(unique(years(donnees[[i]]$date)))

		donnees[[i]]$mes <- c(t(as.data.frame(donnees[[i]][-(1:2)])))
		
		if(dt=="qh") {
			donnees[[i]]$date <- chron(rep(donnees[[i]]$date, each=nb), rep(paste(rep(0:23, each=4), rep(0:3*15, 24), "00", sep=":"), nb2), format=c("y-m-d", "h:m:s"))
			options(warn=-1)
			donnees[[i]]$date <- donnees[[i]]$date + chron(times="00:15:00", format="h:m:s")
			options(warn=0)
		} else if(dt=="heure") {
			donnees[[i]]$date <- chron(rep(donnees[[i]]$date, each=nb), rep(paste(0:23, "00:00", sep=":"), nb2), format=c("y-m-d", "h:m:s"))
			options(warn=-1)
			donnees[[i]]$date <- donnees[[i]]$date + chron(times="01:00:00", format="h:m:s")
			options(warn=0)
		} else if(dt=="jour") {
			donnees[[i]]$date <- chron(rep(donnees[[i]]$date, each=nb), format="y-m-d")
		} else if(dt=="mois") {
			options(warn=-1)
#donnees[[i]]$date <- chron(rep(donnees[[i]]$date, each=nb), format="y-m-d") + 15 + 0:11*30
			donnees[[i]]$date <- chron(paste(rep(as.numeric(as.character(years(donnees[[i]]$date))), each=12), rep(1:12, nb3), "15", sep="-"), rep("00:00:00", nb3*12), format=c("y-m-d", "h:m:s"))
			options(warn=0)
		} else if(dt=="an") {
			donnees[[i]]$date <- chron(donnees[[i]]$date, format="y-m-d")
		}
	
		donnees[[i]]$etat <- unlist(strsplit(donnees[[i]]$etat, ""))
	}

	temp <- lapply(lapply(donnees, "[", c("date", "etat", "mes")), as.data.frame)
	if (keep.state) {
		donnees <- data.frame (
			date=temp[[1]]$date,
			temp[[1]]$mes,
			temp[[1]]$etat)
		names(donnees)[2:3] <-
			paste (names(temp)[1], c('mes', 'etat'), sep='.')
	} else {
		donnees <- data.frame (
			date=temp[[1]]$date,
			ifelse (temp[[1]]$etat %in% codeV,
				temp[[1]]$mes,
				NA) )
		names(donnees)[2] <- names(temp)[1]
	}

	if(length(temp)>1)
	for(i in 2:length(temp)) {
		if (keep.state) {
			donnees.tmp <- data.frame (
				date=temp[[i]]$date,
				temp[[i]]$mes,
				temp[[i]]$etat)
			names(donnees.tmp)[2:3] <-
				paste (names(temp)[i], c('mes', 'etat'), sep='.')
		} else {
			donnees.tmp <- data.frame (
				date=temp[[i]]$date,
				ifelse (temp[[i]]$etat %in% codeV,
					temp[[i]]$mes,
					NA) )
			names(donnees.tmp)[2] <- names(temp)[i]
		}
		donnees <- merge (donnees, donnees.tmp, by='date', all=TRUE)
		#names(donnees)[i+1] <- names(temp)[i]
	}

	if(dt %in% c("qh", "heure")) {
		donnees$date <- substr(as.character(donnees$date), 2, 18)
		donnees$date <- chron(sapply(strsplit(donnees$date, " "), "[", 1), sapply(strsplit(donnees$date, " "), "[", 2), format=c("y-m-d", "h:m:s"))
	}

	# remplissage des donnees eventuellement manquantes
	if(dt %in% c("qh", "heure")) {
		fullDate <- data.frame(date=chron(seq(as.numeric(min(donnees$date)), as.numeric(max(donnees$date)), by=c(1/96, 1/24)[which(c("qh", "heure")==dt)]), format=c("y-m-d", "h:m:s")))
		fullDate$date <- substr(as.character(fullDate$date), 2, 18)
		fullDate$date <- chron(sapply(strsplit(fullDate$date, " "), "[", 1), sapply(strsplit(fullDate$date, " "), "[", 2), format=c("y-m-d", "h:m:s"))
	} else if(dt == "jour") {
		fullDate <- data.frame(date=chron(seq(as.numeric(min(donnees$date)), as.numeric(max(donnees$date)), by=1), format="y-m-d"))
	} else if(dt == "mois") {
		nb <- diff(range(as.numeric(as.character(years(donnees$date)))))+1
		fullDate <- data.frame(date = chron(paste(rep(as.numeric(as.character(unique(years(donnees$date)))), each=12), rep(1:12, nb), 15, sep="-"), rep("00:00:00", 12*nb), format=c("y-m-d", "h:m:s")))
	} else if(dt == "an") {
		fullDate <- data.frame(date=chron(paste(as.character(unique(years(donnees$date))), "01-01", sep="-"), rep("00:00:00", length(unique(years(donnees$date)))), format=c("y-m-d", "h:m:s")))
	}

	donnees <- merge(fullDate, donnees, by="date", all=TRUE)
	donnees <- donnees[donnees$date >= chron(dated, format="y-m-d") & donnees$date <= chron(datef, format="y-m-d"), ]

	# recuperation des infos (unite, mesure, station)
	query <- paste("SELECT MESURE.NOM_COURT_MES, MESURE.UNITE, NOM_MESURE.NCON, STATION.ISIT, STATION.LONGI, STATION.LATI, STATION.LAMBERTX, STATION.LAMBERTY, MESURE.FMUL FROM MESURE, NOM_MESURE, STATION WHERE MESURE.NOM_COURT_MES IN (", nomsPol, ") AND MESURE.NOPOL = NOM_MESURE.NOPOL AND MESURE.NOM_COURT_SIT=STATION.NOM_COURT_SIT", sep="")

	if(.Platform$OS.type=="windows") {
		attributs <- sqlQuery(conxair, query)
	} else if(.Platform$OS.type=="unix") {
		attributs <- dbGetQuery(conxair, query)
		}
	attributs[-length(attributs)] <- lapply(attributs[-length(attributs)], as.character)
	attributs <- attributs[attributs$NOM_COURT_MES %in% sub('\\.mes$', '', names(donnees)),]
	attributs$LATI <- as.numeric(sub(",", ".", attributs$LATI))*180/pi
	attributs$LONGI <- as.numeric(sub(",", ".", attributs$LONGI))*180/pi

	# prise en compte du fmul
	selection <- paste (attributs$NOM_COURT_MES,
			ifelse (keep.state, '.mes', ''),
			sep = '')
	if (!XR6)
	donnees [selection] <- mapply ("*", donnees[selection], 10^attributs$FMUL)

	ordre <- match (	sub ('\\.(mes|etat)$', '', names  (donnees)[-1] ),
				attributs$NOM_COURT_MES )
	attributes(donnees)$station <- c("date", attributs$ISIT[ordre])
	attributes(donnees)$mesure <- c("date", attributs$NCON[ordre])
	attributes(donnees)$unite <- c(dt, attributs$UNITE[ordre])
	attributes(donnees)$longitude <- c(NA, attributs$LONGI[ordre])
	attributes(donnees)$latitude <- c(NA, attributs$LATI[ordre])
	attributes(donnees)$lambertx <- c(NA, as.numeric(attributs$LAMBERTX[ordre]))
	attributes(donnees)$lamberty <- c(NA, as.numeric(attributs$LAMBERTY[ordre]))

	class(donnees) <- c(dt, "Qair", "data.frame")
	if(existMerging) {
		donnees <- merge(donnees, merging);polluants <- c(polluants, setdiff(names(merging), "date"))
		}

	if(.Platform$OS.type=="windows") {
		odbcClose(conxair)
	} else if(.Platform$OS.type=="unix") {
		dbDisconnect(conxair)
		}
	if (keep.state) {
		noms <- setdiff (
				polluants,
				unique (sub ('\\.(mes|etat)$', '', names(donnees))))
		for (i in  noms) {
			donnees[[paste (i, 'mes', sep='.')]] <- NA
			donnees[[paste (i, 'etat', sep='.')]] <- NA
		}
		return (donnees)
	} else {
		for (i in setdiff(polluants, names(donnees)))
			donnees[[i]] <- NA
		return (donnees[c("date", polluants)])
	}

}
