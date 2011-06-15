`longitudes.rose` <-
function(donnees) {
	lapply(as.list(donnees), longitudes)[[1]]
	names(res) <- names(as.list(donnees)[[1]])
	res
	}
