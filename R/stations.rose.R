`stations.rose` <-
function(donnees) {
	res <- lapply(as.list(donnees), stations)[[1]]
	names(res) <- names(as.list(donnees)[[1]])
	res
	}