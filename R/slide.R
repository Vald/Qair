slide <- function(x, fenetre, pas, pc=0.75, FUN=c("mean", 'min', 'max', "var")){
	FUN <- match.arg(FUN)
	nbOk <- ceiling(fenetre * pc)
	.C("slide", as.double(x), as.integer(length(x)), 
		as.integer(fenetre), as.integer(pas), 
		resultat = as.double(rep(NA, ceiling(length(x) / pas))), as.integer(nbOk), as.character(FUN),
		NAOK=TRUE, PACKAGE="Qair")$resultat
	}
