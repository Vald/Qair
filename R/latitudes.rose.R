`latitudes.rose` <-
function(donnees) {
	lapply(as.list(donnees), latitudes)[[1]]
	names(res) <- names(as.list(donnees)[[1]])
	res
	}

