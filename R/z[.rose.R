`[.rose` <-
function(x, i, j, drop){
	rose <- x
	nbArgs <- nargs()
	attributesTemp <- attributes(rose)
	if(missing(i)){rose <- lapply(as.list(rose), "[", , j)
	}else if(missing(j) & nbArgs == 2){rose <- lapply(as.list(rose), "[", i)
	}else if(missing(j)){rose <- lapply(as.list(rose), "[", i, )
	}else{rose <- lapply(as.list(rose), "[", i, j)}
	attributes(rose) <- attributesTemp
	rose
	}

