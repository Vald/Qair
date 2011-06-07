\name{indicesXR2R}
\alias{indicesXR2R}
\alias{plot.indice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Indices de la qualité de l'air.
}
\description{
Fonctions en relation avec le chargement et le traitement des indices
de la qualité de l'air.
}
\usage{
indicesXR2R (
  agglos, dated, datef,
  dsn = NULL, uid = NULL, pwd = NULL, host = NULL)

plot.indice(x, y, cex = 1.7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{agglos}{
%%     ~~Describe \code{agglos} here~~
}
  \item{dated}{
%%     ~~Describe \code{dated} here~~
}
  \item{datef}{
%%     ~~Describe \code{datef} here~~
}
  \item{dsn}{
%%     ~~Describe \code{dsn} here~~
}
  \item{uid}{
%%     ~~Describe \code{uid} here~~
}
  \item{pwd}{
%%     ~~Describe \code{pwd} here~~
}
  \item{host}{
%%     ~~Describe \code{host} here~~
}


  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
			print (sqLQuery (conxair, query) )
			odbcClose(conxair)
		} else if(.Platform$OS.type=="unix") {
			print (dbGetQuery (conxair, query) )
			dbDisconnect(conxair)
		}
	}

	dated <- format(as.Date(chron(dated, format='y-m-d')), format='\%Y-\%m-\%d')
	datef <- as.character(chron(datef, format='y-m-d')+1)
	datef <- format(as.Date(chron(datef, format='y-m-d')), format='\%Y-\%m-\%d')

	query <- sprintf (
		"SELECT NOM_AGGLO, J_DATE, C_IND_DIFFUSE
			FROM	RESULTAT_INDICE JOIN
				GROUPE_ATMO USING (NOM_COURT_GRP)
			WHERE	NOM_AGGLO IN('\%s') AND
				J_DATE BETWEEN
			       		TO_DATE ('\%s', 'YYYY-MM-DD') AND
					TO_DATE ('\%s', 'YYYY-MM-DD')",
		paste (agglos, collapse="', '"), dated, datef)


	if(.Platform$OS.type=="windows") {
		indices <- sqLQuery (conxair, query)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
