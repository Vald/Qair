`unites.Qair` <-
function(donnees) {
	res <- attributes(donnees)$unite
	names(res) <- names(donnees)
	res
	}

