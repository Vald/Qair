`lambertx.rose` <-
function(donnees) {
	lapply(as.list(donnees), lambertx)[[1]]
	names(res) <- names(as.list(donnees)[[1]])
	res
	}

