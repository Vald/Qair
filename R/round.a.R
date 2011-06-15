round.a <- function(x, digits=0){
	x <- x * (10**digits)
	ifelse (sapply (x%%1, function(x) isTRUE ( all.equal (x, 0.5) ) ),
		ceiling(x)/(10**digits),
		round(x)/(10**digits) )
	}
