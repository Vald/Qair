round.a <- function(x, digits=0){
	x <- x * (10**digits)
	ifelse(x%%1 == 0.5, ceiling(x)/(10**digits), round(x)/(10**digits))
	}
