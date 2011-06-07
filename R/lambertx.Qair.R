`lambertx.Qair` <-
function(donnees) {
	res <- attributes(donnees)$lambertx
	names(res) <- names(donnees)
	res
	}
