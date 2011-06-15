`longitudes.Qair` <-
function(donnees) {
	res <- attributes(donnees)$longitude
	names(res) <- names(donnees)
	res
	}

