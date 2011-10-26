compute.rosevent <- function(dv, fv, breaks=0:17*22.5+22.5/2, vlims) {
	if (missing (vlims) )
		vlims <- c(2, 5, 8, max (fv, na.rm=TRUE) )
	vlims <- sort (unique (vlims) )
	null.is.zero <- 0 %in% vlims
	fv[!is.na(fv) & fv <= min (vlims)] <- 0
	vlims <- vlims[vlims != 0]

	select <- !is.na (fv) & !is.na (dv)
	cent <- sum (select)
	f.zero <- sum (select & fv == 0)

	select <- select & fv > 0
	dv <- dv[select]
	fv <- fv[select]
	if (max (vlims) < max (fv) )
		warning("The 'fv' max is higher than the 'vlims' max. All values won't be taken into account.")

	fun.tmp <- function (vlim, dv, fv, breaks) {
		res <- compute.rose (fv, dv, fv, breaks,
			      function(fv, vlim) sum (fv <= vlim),
			      vlim=vlim)
		res$vlim <- vlim
		return (res)
	}

	roses <- lapply (vlims, fun.tmp, dv, fv, breaks)
	if (null.is.zero) {
		roses[[length(roses)+1]] <- list()
		roses[[length(roses)]]$breaks <- roses[[1]]$breaks
		roses[[length(roses)]]$values <- rep (0, length (roses[[1]]$values) )
		roses[[length(roses)]]$vlim <- 0
		roses <- roses[order (sapply(roses, '[[', 'vlim') )]
	} else {
		roses[[1]]$values[TRUE] <- 0#sum (roses[[1]]$values, na.rm=TRUE)/length (roses[[1]]$values)/cent*100
	}
	for (i in 1:length (roses))
		roses[[i]]$values <- (roses[[i]]$values + f.zero/length (roses[[1]]$values))/cent*100
	return (roses)
}

plot.rosevent <- function (x, y, col, border, density=-1, rlim=NULL,
			   centre=list (x=0, y=0), pas.rose=1, facteur=1, add=FALSE, ...) {
	rose <- x

	if (missing (col) )
		col <- sample (colors(), length (rose) ) else
		col <- rep (col, length(rose))[1:length(rose)]
	if (is.null(rlim) )
		rlim <- compute.lim (unlist (lapply (rose, '[[', 'values') ), na.rm=TRUE)
	density <- rep (density, length(rose))[1:length(rose)]
	if (missing (border) )
		border <- par()$col
	border <- rep (border, length(rose))[1:length(rose)]

	for (j in 1:length (rose) ) {
		if (j == 1) {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, at.rho=NA, at.theta=NA, add=add)
		} else if (j == length (rose) ) {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, add=TRUE, ...)
		} else {
			plot.rose (rose[[order(sapply(rose, '[[', 'vlim'), decreasing=TRUE)[j]]],
				   col=col[j], density=density[j], border=border[j],
				   rlim=rlim, at.rho=NA, at.theta=NA, add=TRUE)
		}
	}
}


