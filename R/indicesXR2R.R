# retourne uniquement l'indice diffuse pour l'instant (le champ C_IND_DIFFUSE'
'indicesXR2R' <-
function (agglos, dated, datef, dsn=NULL, uid=NULL, pwd=NULL, host=NULL) {
	# definition de l'hote (unix like system)
	if(!is.null(host)) {
		options(Xair.host=host)
	} else if(is.null(getOption('Xair.host')) & .Platform$OS.type=='unix') {
		cat('hote hebergeant la base de donnees :\n')
		options(Xair.host=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition de la base de donnees (dsn)
	if(!is.null(dsn)) {
		options(Xair.dsn=dsn)
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='unix') {
		cat('nom de la base de donnees (DataSourceName) :\n')
		options(Xair.dsn=scan(what='character', nlines=1))
		cat('\n')
	} else if(is.null(getOption('Xair.dsn')) & .Platform$OS.type=='windows') {
		cat(	'nom de la base de donnees (DataSourceName) :\n',
			names(odbcDataSources('system')), '\n')
		options(Xair.dsn=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du login
	if(!is.null(uid)) {
		options(Xair.uid=uid)
	} else if(is.null(getOption('Xair.uid'))) {
		cat('identifiant pour la connection :\n')
		options(Xair.uid=scan(what='character', nlines=1))
		cat('\n')
	}

	# definition du mot de passe
	if(!is.null(pwd)) {
		options(Xair.pwd=pwd)
	} else if(is.null(getOption('Xair.pwd'))) {
		cat('mot de passe pour la connection :\n')
		options(Xair.pwd=scan(what='character', nlines=1))
		cat('\n')
	}

	# connection a la base
	if(.Platform$OS.type=="windows") {
		conxair <- try (odbcConnect (
			getOption("Xair.dsn"),
			uid = getOption("Xair.uid"), pwd = getOption("Xair.pwd"),
			case = "nochange", believeNRows = TRUE) )
		if(inherits(conxair, "try-error"))
			stop("echec de la connection a la base Xair.")
	} else if(.Platform$OS.type=="unix") {
		if(!is.null(options()$Xair.ojdbc.file)) {
			drv <- JDBC (	"oracle.jdbc.OracleDriver",
					options()$Xair.ojdbc.file)
		} else {
			cat('Veuillez entrer le chemin vers le fichier java ojdbc*.jar\nAfin de ne pas avoir a renseigner ce chemin a chaque session\nvous pouvez definir la variable options(Xair.ojdbc.file = ...) dans le fichier ~/.Rprofile\n')
			drv <- scan(nmax = 1, what = 'character')
		}
		conxair <- try (dbConnect (
			drv,
			paste (	"jdbc:oracle:thin:@",
				getOption("Xair.host"),
				":1521:",
				getOption("Xair.dsn"), sep=""),
			getOption("Xair.uid"), getOption("Xair.pwd"),
			identifer.quote="'") )
		if(inherits(conxair, "try-error"))
			stop("echec de la connection a la base Xair.")
	} else {
		stop("platforme non reconnue")
	}

	if (missing (agglos) ) {
		query <- 'SELECT NOM_AGGLO FROM GROUPE_ATMO'
		if(.Platform$OS.type=="windows") {
			print (sqlQuery (conxair, query) )
			odbcClose(conxair)
		} else if(.Platform$OS.type=="unix") {
			print (dbGetQuery (conxair, query) )
			dbDisconnect(conxair)
		}
		return ()
	}

	dated <- format(as.Date(chron(dated, format='y-m-d')), format='%Y-%m-%d')
	datef <- as.character(chron(datef, format='y-m-d'))
	datef <- format(as.Date(chron(datef, format='y-m-d')), format='%Y-%m-%d')

	query <- sprintf (
		"SELECT NOM_AGGLO, J_DATE, C_IND_DIFFUSE
			FROM	RESULTAT_INDICE JOIN
				GROUPE_ATMO USING (NOM_COURT_GRP)
			WHERE	NOM_AGGLO IN('%s') AND
				J_DATE BETWEEN
			       		TO_DATE ('%s', 'YYYY-MM-DD') AND
					TO_DATE ('%s', 'YYYY-MM-DD')",
		paste (agglos, collapse="', '"), dated, datef)


	if(.Platform$OS.type=="windows") {
		indices <- sqlQuery (conxair, query)
	} else if(.Platform$OS.type=="unix") {
		indices <- dbGetQuery (conxair, query)
	}
	indices$NOM_AGGLO <- as.character (indices$NOM_AGGLO)
	indices$J_DATE <- substr (as.character (indices$J_DATE), 1, 10)
	names (indices)[names (indices) == 'J_DATE'] <- 'date'

	indices$C_IND_DIFFUSE[indices$C_IND_DIFFUSE == 0] <- NA
	temp <- split(indices[c("date", "C_IND_DIFFUSE")], indices$NOM_AGGLO)
	
	indices <- temp[[1]]
	names (indices)[2] <- names (temp)[1]
	if (length (temp) > 1)
	for (i in 2:length(temp)) {
		names (temp[[i]])[2] <- names (temp)[i]
		indices <- merge (indices, temp[[i]], all=TRUE, by='date')
	}
	rm(temp)
	indices$date <- chron(indices$date, format="y-m-d")

	indices <- as.Qair(indices, dt='jour')
	class (indices) <- c('indice', class (indices) )

	if(.Platform$OS.type=="windows") {
		odbcClose(conxair)
	} else if(.Platform$OS.type=="unix") {
		dbDisconnect(conxair)
	}
	
	return (indices)
}

plot.indice <- function (x, y, cex=1.7) {
	commentaires <- factor(
		c(	'Tres bon', 'Tres bon', 'Bon', 'Bon', 'Moyen',
			'Mediocre', 'Mediocre', 'Mauvais', 'Mauvais', 'Tres mauvais'),
		c(	'Tres bon', 'Bon', 'Moyen', 'Mediocre', 'Mauvais',
			'Tres mauvais'),,TRUE)
	indices <- x[[y]]

	plot (	NA, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5),
		axes=FALSE, ann=FALSE, asp=1)
	temp <- data.frame (nb = tapply (
					indices,
					commentaires[indices],
					length) )
	temp$pc <- temp$nb/sum (!is.na (indices) )*100
	cols <- colorRampPalette(c(hsv(0.33, , 0.768), "yellow", "orange", "red"))(6)
	names(cols) <- levels(commentaires)
	
	theta <- 0
	for (j in 1:nrow (temp) ) {
		if (is.na (temp$nb[j]) ) next
		theta <- (seq(360*theta/100, 360*(theta + temp$pc[j])/100))*pi/180
		x.coord <- c(sin(theta), 0.25*sin(theta[length(theta):1]))
		y.coord <- c(cos(theta), 0.25*cos(theta[length(theta):1]))
		polygon(x.coord, y.coord, col=cols[rownames(temp)[j]], density=-1)

		theta <- sum(temp$pc[1:j])
	}
	tempI <- abs (sapply (
			lapply (rownames (temp), nchar),
			'-',
			max(sapply(rownames(temp), nchar) ) ) )
	par(font=2)
	text(-2, cex=cex, seq(1.5, 0.8, length=6), rownames(temp), col=cols, pos=4)
	text(-0.7, cex=cex, seq(1.5, 0.8, length=6), paste(ifelse(is.na(temp$nb), 0, temp$nb), ' jour', ifelse(!is.na(temp$nb) & temp$nb > 1, 's', ''), sep=''), col=cols, pos=2)
	par(font=3)
	text (	-2, cex=cex/1.7*1.4, -1.3, pos=4,
		sprintf (
			"Nombre de jours sans indice : %i",
			sum (is.na (indices) ) ) )
	box(col='white')
}

