`stations.Qair` <-
function(donnees) {
	res <- attributes(donnees)$station
	names(res) <- names(donnees)
	res
	}

