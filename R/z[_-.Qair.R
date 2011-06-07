`[<-.Qair` <-
function(x, i, j, value){
	attributesTemp <- attributes(x)
	x <- as.data.frame(x)
	x[i,j] <- value
	attributes(x) <- attributesTemp
	x
	}
