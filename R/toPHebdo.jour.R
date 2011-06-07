`toPHebdo.jour` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", ...){
	jours <- (as.numeric(weekdays(donnees$date))-1)%%7
	jours[jours == 0] <- 7
	NextMethod(object=donnees, pc=pc, prec=prec, FUN=FUN, jours=jours, ...)}

