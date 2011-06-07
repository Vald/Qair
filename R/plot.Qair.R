`plot.Qair` <-
function(x, y, xlim, ylim, ..., axes=TRUE, x.leg, y.leg, cex.leg=0.7, legend=c("abrev", "complet", "none"), ask.leg=TRUE, out.format="y-m-d"){
	arguments <- as.list(match.call())[-1]
	legend <- match.arg(legend)
	if(missing(y))y <- names(x)
	which <- setdiff(y, "date")
	nb <- length(which)
	if(missing(xlim))xlim <- range(x$date)
	if(missing(ylim)){
		ylim <- c(min(c(unlist(as.data.frame(x)[which]), 0), na.rm=TRUE), max(as.data.frame(x)[which], na.rm=TRUE))
		fact <- ifelse(log(abs(ylim), 10)>=1, 10^floor(log(abs(ylim), 10)), 10^ceiling(log(abs(ylim), 10)))
		ylim <- ceiling(abs(ylim/fact))*fact*sign(ylim)
		ylim <- ifelse(is.na(ylim), 0, ylim)
		}
	argsPlot <- setdiff(intersect(names(arguments), names(as.list(args(plot.default)))), c("x", "y", "xlim", "ylim", "axes", "ann"))
	do.call("plot", c(x=NA, xlim=list(xlim), ylim=list(ylim), axes=FALSE, ann=FALSE, arguments[argsPlot]))
	if(axes){
		axis(2)
		if(inherits(x, "qh")) {
			temp <- seq(as.numeric(x$date[1]), as.numeric(x$date[nrow(x)]), length=10)
			at <- round(temp)
			labels <- as.character(chron(round(temp), out.format=out.format))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "heure")) {
			temp <- seq(as.numeric(x$date[1]), as.numeric(x$date[nrow(x)]), length=10)
			at <- round(temp)
			labels <- as.character(chron(round(temp), out.format=out.format))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "jour")) {
			temp <- seq(as.numeric(x$date[1]), as.numeric(x$date[nrow(x)]), length=10)
			at <- round(temp)
			labels <- as.character(chron(round(temp), out.format=out.format))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "hebdo")) {
			temp <- seq(as.numeric(x$date[1]), as.numeric(x$date[nrow(x)]), length=10)
			at <- round(temp)
			labels <- as.character(chron(round(temp), out.format=out.format))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "mois")) {
			temp <- seq(as.numeric(x$date[1]), as.numeric(x$date[nrow(x)]), length=10)
			at <- round(temp)
			labels <- as.character(chron(round(temp), out.format=out.format))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "an")) {
			at <- as.numeric(as.character(years(x$date)))
			labels <- as.character(years(x$date))
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "pjour")) {
			axis(1, at=x$date, labels=as.character(x$date))
		} else if(inherits(x, "phebdo")) {
			axis(1, at=x$date, labels=c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")[x$date])
		} else if(inherits(x, "pmois")) {
			at <- labels <- 1:31
			axis(1, at=at, labels=labels)
		} else if(inherits(x, "pannuel")) {
			at <- 1:12
			labels <- c("Janv", "Fevr", "Mars", "Avr", "Mai", "Juin", "Juil", "Aout", "Sept", "Octo", "Nove", "Decem")
			axis(1, at=at, labels=labels)}
		box() }
	argsTitle <- setdiff(names(as.list(args(title))), c("...", ""))
	if(any(names(arguments) %in% argsTitle))do.call("title", arguments[intersect(names(arguments), argsTitle)])

	argsLines <- setdiff(intersect(names(arguments), names(as.list(args(plot.xy)))), c("x", "y"))
	temp <- c(x=list(), arguments[argsLines])
	temp$x <- x
	temp$which <- which
	if(!"col" %in% names(temp)){temp$col <- arcenciel(nb)}
	temp$col <- eval(temp$col)
	cols <- temp$col

	do.call("lines", temp)
	
	if(legend != "none"){
		if(ask.leg){temp <- locator(1);x.leg <- temp$x; y.leg <- temp$y}
		if(missing(x.leg))x.leg <- xlim[1]
		if(missing(y.leg))y.leg <- ylim[2]
		legend(x.leg, y.leg, fill=cols, if(legend=="abrev") which else paste(mesures(x)[which], " (", unites(x)[which], "), a ", stations(x)[which], sep=""), cex=cex.leg, bty="n")
#		legend(x.leg, y.leg, fill=cols, if(legend=="abrev") which else paste(mesures(x), " (", unites(x), "), a ", stations(x), sep="")[names(x) %in% which], cex=cex.leg, bty="n")
		}
	}
