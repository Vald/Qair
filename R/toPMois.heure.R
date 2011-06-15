`toPMois.heure` <-
function(donnees, pc=0.75, prec=NA, FUN="mean", ...){
	NextMethod(object=donnees, pc=pc, prec=prec, FUN=FUN, jours=as.numeric(substr(as.character(chron(donnees$date-1/24)), 5, 6)), ...)}

