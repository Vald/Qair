`[.Qair` <-
function(x, i, j, drop) {
	donnees <- x
	nbArgs <- nargs()
	k <- 0
	neg <- NULL
	if(missing(i)) {
	} else if(is.character(i)) {
		if(length(i) == 1 & isTRUE(all.equal(nchar(unlist(strsplit(i, "-"))), c(2, 2, 2)))) {
			dated <- chron(i, format=c("y-m-d"));k <- k+1
		} else if(length(i) == 2 & isTRUE(all.equal(nchar(unlist(strsplit(i[1], "-"))), c(2, 2, 2))) & isTRUE(all.equal(nchar(unlist(strsplit(i[2], ":"))), c(2, 2, 2)))) {
			dated <- chron(i[1], i[2], format=c("y-m-d", "h:m:s"));k <- k+1 }
		}
	if(missing(j)) {
	} else if(is.character(j)) {
		if(length(j) == 1 & isTRUE(all.equal(nchar(unlist(strsplit(j, "-"))), c(2, 2, 2)))) {
			datef <- chron(j, format=c("y-m-d"));k <- k+10
		} else if(length(j) == 2 & isTRUE(all.equal(nchar(unlist(strsplit(j[1], "-"))), c(2, 2, 2))) & isTRUE(all.equal(nchar(unlist(strsplit(j[2], ":"))), c(2, 2, 2)))) {
			datef <- chron(j[1], j[2], format=c("y-m-d", "h:m:s"));k <- k+10 }
		}
	if(k>0) {
		if(k%%10==0)dated <- donnees$date[1]
		if(k%/%10==0)datef <- donnees$date[nrow(donnees)]
		attributesTemp <- attributes(donnees)
		donnees <- as.data.frame(donnees)[donnees$date >= dated & donnees$date <= datef,]
#		attributes(donnees)$mesure <- attributesTemp$mesure
#		attributes(donnees)$station <- attributesTemp$station
#		attributes(donnees)$unite <- attributesTemp$unite
#		attributes(donnees)$longitude <- attributesTemp$longitude
#		attributes(donnees)$latitude <- attributesTemp$latitude
#		attributes(donnees)$lambertx <- attributesTemp$lambertx
#		attributes(donnees)$lamberty <- attributesTemp$lamberty
#		class(donnees) <- attributesTemp$class
		for(l in setdiff(names(attributes(x)), c("names", "row.names")))
			attr(donnees, l) <- attr(x, l)
	} else {
		if(nbArgs==2 & missing(j)) {
			if(is.logical(i)) {
				if(length(i) == length(donnees)) { i <- c(TRUE, rep(FALSE, length(i)-1)) | i
				} else if(length(i) == length(donnees)-1) { i <- c(TRUE, i)
				} else { stop("le nombre de logicals ne correspond pas au nombre de colonnes") }
			} else if(is.numeric(i)) {
				if(all(i>0)) { i <- if(1 %in% i) i else c(1, i)
				} else if(all(i<0)) { neg <- unique(i)# on ne fait rien, on laisse i tel qu'il est
				} else { stop("les indices numeriques de colonnes doivent etre de meme signe")}
			} else if(is.character(i)) { i <- if("date" %in% i) i else c("date", i) }
		} else if(missing(j)) {
		} else if(is.logical(j)) {
			if(length(j) == length(donnees)) { j <- c(TRUE, rep(FALSE, length(j)-1)) | j
			} else if(length(j) == length(donnees)-1) { j <- c(TRUE, j)
			} else { stop("le nombre de logicals ne correspond pas au nombre de colonnes") }
		} else if(is.numeric(j)) {
			if(all(j>0)) { j <- if(1 %in% j) i else c(1, j)
			} else if(all(j<0)) { neg <- unique(j)# on ne fait rien, on laisse j tel qu'il est
			} else { stop("les indices numeriques de colonnes doivent etre de meme signe") }
		} else if(is.character(j)) { j <- if("date" %in% j) j else c("date", j) }

		datesTemp <- donnees$date
		unite <- attributes(donnees)$unite[1]#unites(donnees)[1]
		attributesTemp <- attributes(donnees)
		donnees <- if(length(neg) == length(x)-1) NextMethod(drop=FALSE) else NextMethod()

		class(donnees) <- "data.frame"
		if("date" %in% names(donnees)) donnees <- donnees[c("date", setdiff(names(donnees), "date"))]
		ordre <- apply(outer(names(donnees), attributesTemp$names, "=="), 1, which)
#		attributes(donnees)$mesure <-attributesTemp$mesure[unlist(ordre)]
#		attributes(donnees)$station <- attributesTemp$station[unlist(ordre)]
#		attributes(donnees)$unite <- attributesTemp$unite[unlist(ordre)]
#		attributes(donnees)$longitude <- attributesTemp$longitude[unlist(ordre)]
#		attributes(donnees)$latitude <- attributesTemp$latitude[unlist(ordre)]
#		attributes(donnees)$lambertx <- attributesTemp$lambertx[unlist(ordre)]
#		attributes(donnees)$lamberty <- attributesTemp$lamberty[unlist(ordre)]
		for(attTemp in setdiff(names(attributesTemp), c("names", "row.names", "class")))
			attr(donnees, attTemp) <- attributesTemp[[attTemp]][unlist(ordre)]
		class(donnees) <- attributesTemp$class
		}
	donnees
	}
