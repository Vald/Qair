`points.Qair` <-
function(x, which=names(x), col, ...){
	which <- setdiff(which, "date")
	nb <- length(which)
	if(missing(col)){
		col <- arcenciel(nb)}
	arguments <- as.list(match.call())[-1]
	arguments <- c(col=list(col), arguments[setdiff(names(arguments), c("", "x", "which", "col"))])
	arguments <- lapply(arguments, eval)
	arguments <- mapply("rep", arguments, as.list(ceiling(nb/sapply(arguments, length))), SIMPLIFY=FALSE)
	arguments <- lapply(arguments, "[", i=1:nb)
	for(i in 1:nb)
		do.call("points", c(x=list(x$date), y=list(x[[which[i]]]), lapply(arguments, "[", i)))
	}

