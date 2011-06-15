'Xair2bilan' <-
function(dated, datef, seuils, parAn=TRUE, fichier=TRUE, interactive.seuils=FALSE, ...){

	# recuperation des donnees sur lesquelles le bilan doit etre realise
	#-------------------------------------------------------------------
	donneesCompletes <- Xair2R(dated=dated, datef=datef, dt="heure", ...)

	# recuperation du 'type' de polluant de chaque mesure
	base <- data.frame(nopol=c("01", "03", "04", "08", "12", "24", "AA", "AC", "AH", "AJ", "P6", "V4", "39"), type=c("SO2", "NO2", "CO", "O3", "Nox", "PM10", "Pb", "Ni", "As", "Cd", "BaP", "C6H6", "PM2.5"))
	query <- paste("select NOM_COURT_MES, NOPOL FROM MESURE WHERE NOM_COURT_MES IN ('", paste(names(donneesCompletes)[-1], collapse="', '"),"')", sep="")
	if(.Platform$OS.type=="windows"){
		conxair <- odbcConnect(getOption("Xair.dsn"), uid = getOption("Xair.uid"), pwd = getOption("Xair.pwd"), case = "nochange", believeNRows = TRUE)
		type <- sqlQuery(conxair, query, as.is=1:2)
		odbcClose(conxair)
	}else if(.Platform$OS.type=="unix"){
		drv <- JDBC("oracle.jdbc.driver.OracleDriver", "/usr/lib/jvm/java-6-openjdk/jre/lib/ext/ojdbc14.jar")
		conxair <- try(dbConnect(drv, paste("jdbc:oracle:thin:@", getOption("Xair.host"), ":1521:", getOption("Xair.dsn"), sep=""), getOption("Xair.uid"), getOption("Xair.pwd"), identifer.quote="'"))
		type <- dbGetQuery(conxair, query)
		for(i in 1:length(type))
			type[[i]] <- as.character(type[[i]])
	}else{
		stop("plateforme non-reconnue")
		}
	type <- type$NOPOL[match(names(donneesCompletes)[-1], type$NOM_COURT_MES)]
	type <- as.character(base$type[match(type, base$nopol)])


	# recuperation de la definition des seuils regelementaires
	#---------------------------------------------------------
	if(missing(seuils)){
		if(!exists('seuils.Qair'))data(seuils.Qair)
		seuils <- seuils.Qair
		}
	if(interactive.seuils){
		seuils <- seuils[seuils$id_poll %in% unique(type), ]
		seuils <- edit(seuils)
		}

	# realisation des bilans de maniere 'historique' (une valeur par annee, par polluant et par seuil, toutes les annees sont donnees dans le meme tableau)
	#------------------------------------------------------------------------------------------------------------------------------------------------------
	tps <- Sys.time()
	if(parAn) {
		temp <- lapply(lapply(type, "==", seuils$id_poll), subset, x=seuils[,c("enteteLigne1", "enteteLigne2", "diffusion")])
		resultat <- data.frame(id = character(0), enteteLigne1=character(0), enteteLigne2=character(0))
		resultat$id <- as.character(resultat$id)
		resultat$enteteLigne1 <- as.character(resultat$enteteLigne1)
		resultat$enteteLigne2 <- as.character(resultat$enteteLigne2)
		for(i in 1:length(temp)){
			i.nom <- names(donneesCompletes)[-1][i]
			resultat <- rbind(resultat, list(id=i.nom, enteteLigne1=i.nom, enteteLigne2=""))
			resultat <- rbind(resultat, data.frame(id=rep(i.nom, sum(temp[[i]]$diffusion==1)), temp[[i]][temp[[i]]$diffusion==1, c("enteteLigne1", "enteteLigne2")]))
			resultat$id <- as.character(resultat$id)
			resultat$enteteLigne1 <- as.character(resultat$enteteLigne1)
			resultat$enteteLigne2 <- as.character(resultat$enteteLigne2)
			}
		annee <- sort(as.numeric(as.character(unique(years(donneesCompletes$date-1/24)))))
		for(i in annee){
			donnees <- donneesCompletes[as.numeric(as.character(years(donneesCompletes$date-1/24))) == i,]
			if(nrow(donnees)!=0){
				temp <- mapply(calcul_seuils, x=as.data.frame(donnees)[-1], id_poll=as.list(type), MoreArgs=list(date=donnees$date, seuils=seuils, pourDiffusion=TRUE), SIMPLIFY=FALSE)
				temp <- mapply(c, rep(NA, length(donnees)-1), temp, SIMPLIFY=FALSE)
				names(temp) <- names(donnees)[-1]
				resultat[[sprintf("a%i", i)]] <- unlist(temp)
			}else{
				resultat[[sprintf("a%i", i)]] <- rep(NA, nrow(resultat))
				}
			cat(sprintf("annee : %i", i), difftime(Sys.time(), tps, units="secs"), "secondes\n")
			}
	} else {
	# realisation des bilans sur la periode specifiee, en globale (une seule valeur par mesure et par seuil)
	#-------------------------------------------------------------------------------------------------------
		resultat <- list()
		for(i in unique(type)){
			cat("polluant :", i, "\n-----------------------------\n")
			resultat[[i]] <- seuils[seuils$id_poll==i & seuils$diffusion==1, c("enteteLigne1", "enteteLigne2")]
			donnees <- donneesCompletes[c(T, type==i)]
			for(j in names(donnees)[-1]){
				resultat[[i]][[j]] <- calcul_seuils(donnees$date, donnees[[j]], i, seuils, pourDiffusion=TRUE)
				cat(sprintf("mesure : %s", j), difftime(Sys.time(), tps, units="secs"), "secondes\n")
				}
			}
		}

	# sauvegarde dans un fichier, si necessaire et renvoi du resultat
	#----------------------------------------------------------------
	if(is.logical(fichier) && !fichier){
		return(resultat)
	}else if(is.logical(fichier) && fichier){
		fichier <- choose.files(multi=F, filter=".csv")
		}
	if(!is.data.frame(resultat)) {
		for(i in 1:length(resultat))
			write.csv(resultat[[i]], fichier, app=if(i==1) FALSE else TRUE, row.names=FALSE, na="")
	} else {
		write.csv(resultat, fichier, row.names=FALSE, na="")
		}
	invisible(resultat)
	}
