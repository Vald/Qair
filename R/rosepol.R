compute.rose <- function(values, dv, fv, breaks=0:17*22.5+22.5/2, FUN, ...) {
	dots <- list (...)
	if (missing (FUN) ) {
		FUN <- mean
		if (length (dots) == 0)
			dots$na.rm=TRUE
	}
	FUN <- match.fun(FUN)
	# les trois vecteurs values, dv et fv doivent avoir la meme longeur
	if(length(values) != length(dv) || length(values) != length(fv)) stop('values, dv and fv should have the same length.')
	
	# coherence de breaks
	while(any(breaks < 0))
		breaks[breaks < 0] <- breaks[breaks < 0] + 360
	while(any(breaks > 360))
		breaks[breaks > 360] <- breaks[breaks > 360] - 360
	breaks <- sort (unique (breaks%%360) )
	if (min (breaks) == 0)
		breaks <- c(breaks, 360) else
		breaks <- c(max(breaks)-360, breaks)

	# repartition des mesures dans les intervalles
	dv <- dv %% 360
	temp <- findInterval(dv, breaks)%%length(unique(breaks%%360))
	temp <- ifelse(temp != 0, temp, length(unique(breaks%%360)))

	at.rose <- rep(NA, length(unique(breaks%%360)))
	
	dots$X <- ifelse(!is.na(fv) & fv > 0, values, NA)
	dots$INDEX <- temp
	dots$FUN <- FUN
	
	at.rose[sort(unique(temp))] <- do.call (tapply, dots)
	at.rose[is.na(at.rose)] <- 0

	return(list(breaks = breaks, values=at.rose, opened=factor(c('left', 'right'), ordered=FALSE)[2]))
}



plot.rose <- function(x, y,
	col, border, density=-1,
	rlim=NULL,
	at.rho = NULL, unite = '', labels.rho=NULL, col.rho, lwd.rho=1, lty.rho=2, expr.rho=FALSE,
	at.theta = 0:7*45, labels.theta = c('N', 'NE', 'E', 'SE', 'S', 'SO', 'O', 'NO'), nb.theta = 360, col.theta, lwd.theta=1, lty.theta=2,
	centre = list(x=0, y=0), pas.rose = 1, facteur=1, add=FALSE) {

	rose <- x
	breaks <- rose$breaks
	if(missing(y)) at.rose <- rose$values else at.rose <- sapply(rose$values, '[[', y)

	if(is.null(rlim)) {
		rlim <- c(min(c(at.rose, 0), na.rm = TRUE), max(at.rose, na.rm = TRUE))
		fact <- ifelse(log(abs(rlim), 10) >= 1, 10^floor(log(abs(rlim), 10)), 10^ceiling(log(abs(rlim), 10)))
		rlim <- ceiling(abs(rlim/fact)) * fact * sign(rlim)
		rlim <- ifelse(is.na(rlim), 0, rlim)
	}

	if(is.null(at.rho)) at.rho <- seq(min(rlim), max(rlim), length=3)[-1]
	if(is.null(labels.rho)) labels.rho <- paste(at.rho, unite, sep='')

	if(length(labels.theta) != length(at.theta)) {
		warning("Les labels de directions ne correspondent pas en nombre avec leurs emplacements.\nIls sont remplaces par des valeurs par defaut.")
		labels.theta <- paste(round(at.theta), 'deg')
	}

	if(!add) {
		#par(mar=c(0, 0, 0, 0) + 0.1)
		plot(NA, asp=1, axes=FALSE, xlim=c(-1, 1)*abs(diff(rlim))*1.1+centre$x, ylim=c(-1, 1)*abs(diff(rlim))*1.1+centre$y, ann=FALSE)
	}

	if(missing(col)) col <- par()$bg
	if(missing(border)) border <- par()$col
	if(missing(col.rho)) col.rho <- par()$col.axis
	if(missing(col.theta)) col.theta <- par()$col.axis

	#         theta <- mapply(seq, breaks[-(length(breaks)-0:1)], breaks[-c(1, length(breaks))], by=pas.rose, SIMPLIFY=FALSE)
	theta <- mapply(seq, breaks[-length(breaks)], breaks[-1], by=pas.rose, SIMPLIFY=FALSE)
	theta <- lapply(theta, '*', pi/180)
	x <- facteur * unlist(mapply('*', at.rose - min(rlim), lapply(theta, sin), SIMPLIFY=FALSE)) + centre$x
	y <- facteur * unlist(mapply('*', at.rose - min(rlim), lapply(theta, cos), SIMPLIFY=FALSE)) + centre$y
	polygon(x[c(1:length(x), 1)], y[c(1:length(y), 1)], density=density, col=col, border=border)

	if(!all(is.na(at.rho))){
		theta <- seq(0, 360, length=nb.theta)
		mapply(polygon, as.data.frame(outer(cos(theta * pi/180), at.rho - min(rlim), '*') * facteur +centre$x), as.data.frame(outer(sin(theta * pi/180), at.rho - min(rlim), '*') * facteur +centre$y), MoreArgs=list(lty=lty.rho, lwd=lwd.rho, col=col.rho, density=0))
		theta <- sample(0:360, 1)
		text((at.rho - min(rlim))*sin(theta) * facteur + centre$x, (at.rho - min(rlim))*cos(theta) * facteur + centre$y, if(expr.rho) parse(text=labels.rho) else labels.rho)
	}

	if(!all(is.na(at.theta))) {
		text(sin(at.theta * pi/180) * abs(diff(rlim)) * 1.1 * facteur + centre$x, cos(at.theta * pi/180) * abs(diff(rlim)) * 1.1 * facteur + centre$y, labels.theta, col=col.theta)
		segments(x0=centre$x, y0=centre$y, x1=sin(at.theta * pi/180) * abs(diff(rlim)) * facteur + centre$x, y1=cos(at.theta * pi/180) * abs(diff(rlim)) * facteur + centre$y, col=col.theta, lty=lty.theta, lwd=lwd.theta)
	}
	
}


rosepol <- function(
	values, dv, fv, breaks=0:17*22.5+22.5/2, FUN = 'mean',
	col, border, density=-1,
	rlim=NULL,
	at.rho = NULL, unite = '', labels.rho=NULL, col.rho, lwd.rho=1, lty.rho=2, expr.rho=FALSE,
	at.theta = 0:7*45, labels.theta = c('N', 'NE', 'E', 'SE', 'S', 'SO', 'O', 'NO'), nb.theta = 360, col.theta, lwd.theta=1, lty.theta=2,
	centre = list(x=0, y=0), pas.rose = 1, facteur=1, add=FALSE,
	...) {
	rose <- compute.rose(values, dv, fv, breaks, FUN, ...)
	plot.rose(x=rose, col=col, border=border, density=density, rlim=rlim, at.rho=at.rho, unite=unite, labels.rho=labels.rho, at.theta=at.theta, labels.theta=labels.theta, nb.theta=nb.theta, centre=centre, pas.rose=pas.rose, facteur=facteur, add=add, col.rho=col.rho, col.theta=col.theta, expr.rho=expr.rho)
invisible(rose)
}





