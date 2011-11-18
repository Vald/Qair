round.a <- function(x, digits=0){
	.local <- function(x, digits) {
		x <- x * (10**digits)
		ifelse (sapply (x%%1, function(x) isTRUE ( all.equal (x, 0.5) ) ),
			ceiling(x)/(10**digits),
			round(x)/(10**digits) )
	}
	if (is.data.frame (x)) return (data.frame (lapply (x, .local, digits) ) )
	.local (x, digits)
}
