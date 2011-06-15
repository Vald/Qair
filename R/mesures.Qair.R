`mesures.Qair` <-
function(donnees) {
	res <- attributes(donnees)$mesure
	names(res) <- names(donnees)
	res
	}

