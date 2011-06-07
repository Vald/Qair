`lamberty.Qair` <-
function(donnees) {
	res <- attributes(donnees)$lamberty
	names(res) <- names(donnees)
	res
	}

