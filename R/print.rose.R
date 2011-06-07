`print.rose` <-
function(x, ...){
	donnees <- x
	cat("nombre de donnees disponibles par secteur et par variable :\n")
	temp <- cbind(t(data.frame(attributes(donnees)$intervalle)), t(sapply(lapply(lapply(lapply(donnees, as.data.frame), is.na), "!"), colSums)))
	rownames(temp) <- NULL
	colnames(temp) <- c("angleInf", "angleSup", names(donnees))
	print(temp)
	cat("\nformule utilisee (~direction + vitesse) \n")
	cat("\t", paste(as.character(attributes(donnees)$formula), collapse=""), "\n\n")
	cat(ifelse(is.na(attributes(donnees)$aggregation), "pas d'", ""), "aggregation de donnees", ifelse(is.na(attributes(donnees)$aggregation), "", paste(" :\n\t", attributes(donnees)$aggregation)), "\n", sep="")
	}

