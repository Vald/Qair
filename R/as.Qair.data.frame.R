'as.Qair.data.frame' <- 
function(x, dt=NULL, debut=NULL, stations=NULL, mesures=NULL, unites=NULL, longitudes=NULL, latitudes=NULL, lambertx=NULL, lamberty=NULL, ...) {

# s'il n'y a pas de dates dans la data.frame, on ajoute la colonne si c'est possible. Sinon erreur.
if(!'date' %in% names(x) & (is.null(dt) | is.null(debut))) {
	stop(paste("Il n'y a pas de colonne 'date' dans la data.frame, le(s) argument(s) obligatoire(s) suivant(s) est(sont) manquant(s) :", paste(c('dt', 'debut')[c(is.null(dt), is.null(debut))], collapse=", ")))
} else if(!'date' %in% names(x)) {
	if(dt == 'qh') {
		if(nchar(debut) == 8) {
			debut <- chron(debut, format="y-m-d")
		} else if (nchar(debut) == 17) {
			debut <- chron(substr(debut, 1, 8), substr(debut, 10, 17), format=c("y-m-d", "h:m:s"))
		} else {
			stop("pour un dt = 'qh', 'debut' doit etre au format 'y-m-d h:m:s' ou 'y-m-d'")
			}	
		x$date <- debut + (1:nrow(x)-1)/(24*4)
	} else if(dt == 'heure') {
		if(nchar(debut) == 8) {
			debut <- chron(debut, format="y-m-d")
		} else if (nchar(debut) == 17) {
			debut <- chron(substr(debut, 1, 8), substr(debut, 10, 17), format=c("y-m-d", "h:m:s"))
		} else {
			stop("pour un dt = 'heure', 'debut' doit etre au format 'y-m-d h:m:s' ou 'y-m-d'")
			}	
		x$date <- debut + (1:nrow(x)-1)/24
	} else if(dt == 'jour') {
		if(nchar(debut) == 8) {
			debut <- chron(debut, format="y-m-d")
		} else {
			stop("pour un dt = 'jour', 'debut' doit etre au format 'y-m-d'")
			}	
		x$date <- debut + (1:nrow(x)-1)
	} else if(dt == 'hebdo') {
		if(nchar(debut) == 8) {
			debut <- chron(debut, format="y-m-d")
		} else {
			stop("pour un dt = 'hebdo', 'debut' doit etre au format 'y-m-d'")
			}
		x$date <- debut + (1:nrow(x)-1)*7
	} else if(dt == 'mois') {
		if(nchar(debut) == 8) {
			debut <- chron(sprintf("%s-15", substr(debut, 1, 5)), format="y-m-d")
			if(substr(as.character(debut), 7, 8) != '15') warning('Les mois sont designes par le 15eme jour du mois. La date initiale est donc modifiee en consequences')
		} else {
			stop("pour un dt = 'mois', 'debut' doit etre au format 'y-m-d'")
			}
		annee <- as.numeric(as.character(years(debut)))
		mois <- as.numeric(months(debut))
		x$date <- chron(paste(rep(annee, nrow(x)) + c(rep(0, min(c(12 - mois + 1, nrow(x)))), if(12 - mois + 1 < nrow(x)) rep(1:ceiling(nrow(x)/12), each=12)[1:(nrow(x) - 12 + mois - 1)] else NULL), (mois+(1:nrow(x)-1)-1)%%12+1, "15", sep="-"), format="y-m-d")
	} else if(dt == 'an') {
		if(nchar(debut) == 8) {
			debut <- chron(sprintf("%s-01-01", substr(debut, 1, 2)), format="y-m-d")
			if(substr(as.character(debut), 4, 5) != '01' | substr(as.character(debut), 7, 8) != '01') warning('Les annees sont representees par leur premier jour. La date initiale est donc modifiee en consequences')
		} else {
			stop("pour un dt = 'an', 'debut' doit etre au format 'y-m-d'")
			}
		x$date <- chron(sprintf("%02i-01-01", as.numeric(as.character(years(debut))) + (1:nrow(x)-1)), format="y-m-d")
	} else if(dt == 'pjour') {
		x$date <- debut + (1:nrow(x)-1)
	} else if(dt == 'phebdo') {
		x$date <- debut + (1:nrow(x)-1)
	} else if(dt == 'pmois') {
		x$date <- debut + (1:nrow(x)-1)
	} else if(dt == 'pannuel') {
		x$date <- debut + (1:nrow(x)-1)
	} else {
		stop("La variable 'dt' doit prendre l'une des valeurs suivantes :\n\t'qh', 'heure', 'jour', 'hebdo', 'mois', 'an', 'pjour', 'phebdo', 'pmois', 'pannuel'")
		}
	
	# pour les chrons, on verifie qu'il n'y a pas d'arrondis dans les valeurs de date
	if(dt %in% c('qh', 'heure', 'jour')) {
		x$date <- sprintf("%s-%02i-%02i %02i:%02i:%02i",
			as.character(years(x$date)), as.numeric(months(x$date)), as.numeric(days(x$date)),
			hours(x$date), minutes(x$date), seconds(x$date))
		x$date <- chron(substr(x$date, 1, 10), substr(x$date, 12, 19), format=c("y-m-d", "h:m:s"))
		}
} else if(!is.null(dt)) {
	# on verifie la coherence entre dt et les dates de 'x'. Si incoherent, on averti l'utilisateur
	if(dt == 'qh' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)]))*24*4 >= 2 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)]))*24*4 <= 0.9)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees quart-horaires')
	} else if(dt == 'heure' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)]))*24 >= 2 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)]))*24 <= 0.9)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees horaires')
	} else if(dt == 'jour' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) >= 2 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) <= 0.9)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees journalieres')
	} else if(dt == 'hebdo' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) >= 10 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) <= 5)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees hebdomadaires')
	} else if(dt == 'mois' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) >= 40 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) <= 20)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees mensuelles')
	} else if(dt == 'an' & (mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) >= 400 | mean(as.numeric(x$date[-1] - x$date[-nrow(x)])) <= 200)) {
		warning('Il semble que les donnees ne soient pas exactement des donnees annuelles')
	} else if(dt == 'pjour' & (min(x$date) < 0 | max(x$date) > 24)) {
		warning('Il semble que les donnees ne soient pas exactement des profils journaliers')
	} else if(dt == 'phebdo' & (min(x$date) <= 0 | max(x$date) > 7)) {
		warning('Il semble que les donnees ne soient pas exactement des profils hebdomadaires')
	} else if(dt == 'pmois' & (min(x$date) <= 0 | max(x$date) > 31)) {
		warning('Il semble que les donnees ne soient pas exactement des profils mensuels')
	} else if(dt == 'pannuel' & (min(x$date) <= 0 | max(x$date) > 12)) {
		warning('Il semble que les donnees ne soient pas exactement des profils annuel')
		}
	}

x <- x[c("date", setdiff(names(x), "date"))]

if(!is.null(stations) && length(stations) == length(x)-1) {
	attributes(x)$station <- c(NA, stations)
} else {
	attributes(x)$station <- rep(NA, length(x))
	}
if(!is.null(mesures) && length(mesures) == length(x)-1) {
	attributes(x)$mesure <- c('date', mesures)
} else {
	attributes(x)$mesure <- c('date', rep(NA, length(x)-1))
	}
if(!is.null(unites) && length(unites) == length(x)-1) {
	attributes(x)$unite <- c(NA, unites)
} else {
	attributes(x)$unite <- rep(NA, length(x))
	}
if(!is.null(longitudes) && length(longitudes) == length(x)-1) {
	attributes(x)$longitude <- c(NA, longitudes)
} else {
	attributes(x)$longitude <- rep(NA, length(x))
	}
if(!is.null(latitudes) && length(latitudes) == length(x)-1) {
	attributes(x)$latitude <- c(NA, latitudes)
} else {
	attributes(x)$latitude <- rep(NA, length(x))
	}
if(!is.null(lambertx) && length(lambertx) == length(x)-1) {
	attributes(x)$lambertx <- c(NA, lambertx)
} else {
	attributes(x)$lambertx <- rep(NA, length(x))
	}
if(!is.null(lamberty) && length(lamberty) == length(x)-1) {
	attributes(x)$lamberty <- c(NA, lamberty)
} else {
	attributes(x)$lamberty <- rep(NA, length(x))
	}

class(x) <- c(dt, "Qair", "data.frame")
return(x)
}
