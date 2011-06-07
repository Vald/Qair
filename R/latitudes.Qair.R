`latitudes.Qair` <-
function(donnees) {
	res <- attributes(donnees)$latitude
	names(res) <- names(donnees)
	res
	}

