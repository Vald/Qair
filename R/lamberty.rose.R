`lamberty.rose` <-
function(donnees) {
	lapply(as.list(donnees), lamberty)[[1]]
	names(res) <- names(as.list(donnees)[[1]])
	res
	}

