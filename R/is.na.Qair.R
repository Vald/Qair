`is.na.Qair` <-
function(x){
	attributesTemp <- attributes(x)
	x <- as.data.frame(x)
	x <- data.frame(date=x$date, is.na(x[-1]))
	attributes(x) <- attributesTemp
	x
	}
