quantile.a <- function(x, probs, pc=0.75, ...)
	ifelse(mean(!is.na(x))>=pc, sort(x)[round.a(probs * sum(!is.na(x)))], NA)
