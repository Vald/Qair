`merge.Qair` <-
function(x, y, by="date", all=TRUE, ...){
	if(!isTRUE(all.equal(class(x), class(y))))stop("l'argument y n'est pas de la bonne classe")
	attributesTemp <- list(x=attributes(x), y=attributes(y))
	x <- as.data.frame(x)
	y <- as.data.frame(y)
	donnees <- merge(x, y, by=unique(c("date", by)), all=all, ...)
#	attributes(donnees)$mesure <- c("date", attributesTemp$x$mesure[-1], attributesTemp$y$mesure[-1])
#	attributes(donnees)$station <- c("date", attributesTemp$x$station[-1], attributesTemp$y$station[-1])
#	attributes(donnees)$unite <- c(attributesTemp$x$unite[1], attributesTemp$x$unite[-1], attributesTemp$y$unite[-1])
#	attributes(donnees)$longitude <- c(attributesTemp$x$longitude[1], attributesTemp$x$longitude[-1], attributesTemp$y$longitude[-1])
#	attributes(donnees)$latitude <- c(attributesTemp$x$latitude[1], attributesTemp$x$latitude[-1], attributesTemp$y$latitude[-1])
#	attributes(donnees)$lambertx <- c(attributesTemp$x$lambertx[1], attributesTemp$x$lambertx[-1], attributesTemp$y$lambertx[-1])
#	attributes(donnees)$lamberty <- c(attributesTemp$x$lamberty[1], attributesTemp$x$lamberty[-1], attributesTemp$y$lamberty[-1])
#	class(donnees) <- c("heure", "Qair", "data.frame")
	for(attTemp in setdiff(union(names(attributesTemp$x), names(attributesTemp$y)), c("names", "row.names", "class")))
		if(!attTemp %in% names(attributesTemp$x)) {
			attr(donnees, attTemp) <- c(attributesTemp$y[[attTemp]][1], rep(NA, length(x)-1), attributesTemp$y[[attTemp]][-1])
		} else if(!attTemp %in% names(attributesTemp$y)) {
			attr(donnees, attTemp) <- c(attributesTemp$x[[attTemp]][1], attributesTemp$x[[attTemp]][-1], rep(NA, length(y)-1))
		} else {
			attr(donnees, attTemp) <- c(attributesTemp$x[[attTemp]][1], attributesTemp$x[[attTemp]][-1], attributesTemp$y[[attTemp]][-1])
			}
		class(donnees) <- attributesTemp$x$class
		donnees
	}
	