`plot.rose` <-
function(x, ..., r, rlim, add=FALSE, thetaPas=1,
	density=NULL, col, border, lwd.border=1, lty.border=1,
	at.axis, lwd.axis=1, lty.axis=2, col.axis="grey", label.axis, alpha.axis=45, cex.axis=0.7,
	ray=TRUE, lwd.ray=0.5*lwd.border, lty.ray=1, col.ray="black",
	typeRose=c("centre", "zero", "boite"),
	deg=0:11*30, cex.deg=0.7, main){
#	parMar <- par()$mar
	rose <- x
	if(missing(col)){
		col <- chaud(nrow(as.list(rose)[[1]])+3)[1:nrow(as.list(rose)[[1]])+3]
		col <- col[length(col):1]}
	if(missing(border)){
		border <- ifelse(rep(!is.null(density) && density==0, length(col)), col, "black")}
	if(is.null(density) || density!=0)border <- border[length(border):1]
	typeRose <- match.arg(typeRose)
	missRlim <- FALSE
	if(missing(rlim)){missRlim <- TRUE}
	missAtAxis <- FALSE
	if(missing(at.axis)){missAtAxis <- TRUE}
	missLabelAxis <- FALSE
	if(missing(label.axis)){missLabelAxis <- TRUE}
	if(missing(r)){r <- setdiff(names(rose), c("date", attributes(terms(attributes(rose)$formula))$term.labels))}
	missMain <- FALSE
	if(missing(main)){missMain <- TRUE}

	# A TRAITER EN AGREGEANT PAR DEFAUT A LA MOYENNE
	###
	if(is.na(attributes(rose)$aggregation))stop("la rose n'est aggregee par aucune fonction")
	###
	infTemp <- lapply(attributes(rose)$intervalle, "[", 1)[-1]
	supTemp <- lapply(attributes(rose)$intervalle, "[", 2)[-1]
	supTemp <- ifelse(mapply("<", supTemp, infTemp, SIMPLIFY=FALSE), lapply(supTemp, "+", 360) ,supTemp)
	theta <- mapply(seq, infTemp, supTemp, MoreArgs = list(by = thetaPas), SIMPLIFY=FALSE)


	# RECUPERATION des RAYONS a TRACER
	toPlot <- array(NA, dim=c(length(infTemp), mean(sapply(as.list(rose), nrow)), length(r)), dimnames=list(secteur=NULL, colonnes=NULL, r=r))
	for(i in r){
		if(dim(toPlot)[2] == 1){toPlot[,,i] <- sapply(lapply(rose, as.data.frame), "[[", i)[-1]
		}else{
			temp <- t(sapply(lapply(rose, as.data.frame), "[[", i))[-1,]
			toPlot[,,i] <- temp[, order(colMeans(temp, na.rm=TRUE), decreasing=TRUE)]}}

	# TRACAGE
	for(i in r){
		if(missRlim){
			rlim <- c(min(toPlot[,,i], 0, na.rm=TRUE), max(toPlot[,,i], 0, na.rm=TRUE))
			fact <- ifelse(log(abs(rlim), 10)>=1, 10^floor(log(abs(rlim), 10)), 10^ceiling(log(abs(rlim), 10)))
			rlim <- ceiling(abs(rlim/fact))*fact*sign(rlim)
			rlim <- ifelse(is.na(rlim), 0, rlim)}
		if(i == r[1]){rlimSvg <- rlim}
		if(is.list(rlimSvg)){rlim <- rlimSvg[[ifelse(which(i==r)%%length(rlimSvg)==0, length(rlimSvg), which(i==r)%%length(rlimSvg))]]}
		par(mar=c(0, 0, 4.1, 0))
		if(!add){
			plot(NA, asp=1, ann=FALSE, axes=FALSE, xlim=c(-1.1, 1.1)*diff(rlim), ylim=c(-1.1, 1.1)*diff(rlim))
			segments(0, 0, diff(rlim)*sin(deg*pi/180), diff(rlim)*cos(deg*pi/180), lty=lty.axis, lwd=lwd.axis, col=col.axis)
			text(1.1*diff(rlim)*sin(0:3*90*pi/180), 1.1*diff(rlim)*cos(0:3*90*pi/180), paste(0:3*90, "deg."), cex=cex.deg, col=col.axis)}
		if(typeRose=="centre" | (!is.null(density) && density==0)){
			rayons <- data.frame(apply(array(c(toPlot[, , i] - rlim[1], matrix(0, nrow = dim(toPlot)[1], ncol = dim(toPlot)[2])), dim = c(dim(toPlot)[1], dim(toPlot)[2], 2)), c(1, 2), max))
			rayons[is.na(rayons)] <- 0
			x <- list()
			y <- list()
			for(j in 1:length(rayons)){
				x[[length(x)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), sin), SIMPLIFY=FALSE))
				y[[length(y)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), cos), SIMPLIFY=FALSE))}
			nb <- length(x)
			trash <- mapply(polygon, x=lapply(x, t), y=lapply(y, t), col=rep(col, ceiling(nb/length(col)))[1:nb], border=rep(border, ceiling(nb/length(border)))[1:nb], lty=rep(lty.border, ceiling(nb/length(lty.border)))[1:nb], lwd=rep(lwd.border, ceiling(nb/length(lwd.border)))[1:nb], MoreArgs=list(density=density))
			if((is.null(density) || density!=0) & ray){
				rayons <- array(toPlot[,,i], dim=c(dim(toPlot)[1], dim(toPlot)[2]))
				rayons[is.na(rayons)] <- 0
				secteur <- list( x1=rep(0, dim(rayons)[1]), y1=rep(0, dim(rayons)[1]), x2=sapply(apply(rayons, 1, max)-rlim[1], max, 0), y2=sapply(apply(rayons, 1, max)-rlim[1], max, 0))
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(infTemp)*pi/180), cos(unlist(infTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(supTemp)*pi/180), cos(unlist(supTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)}
		}else if(typeRose=="zero"){
			rayons <- data.frame(apply(array(c(toPlot[,,i]-rlim[1], matrix(-rlim[1], nrow=dim(toPlot)[1], ncol=dim(toPlot)[2])), dim=c(dim(toPlot)[1], dim(toPlot)[2], 2)), c(1, 2), max))
			rayons[is.na(rayons)] <- -rlim[1]
			x <- list()
			y <- list()
			for(j in 1:length(rayons)){
				x[[length(x)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), sin), SIMPLIFY=FALSE))
				y[[length(y)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), cos), SIMPLIFY=FALSE))}
			nb <- length(x)
			trash <- mapply(polygon, x=lapply(x, t), y=lapply(y, t), col=rep(col, ceiling(nb/length(col)))[1:nb], border=rep(border, ceiling(nb/length(border)))[1:nb], lty=rep(lty.border, ceiling(nb/length(lty.border)))[1:nb], lwd=rep(lwd.border, ceiling(nb/length(lwd.border)))[1:nb], MoreArgs=list(density=density))
			rayons <- apply(array(c(toPlot[,,i]-rlim[1], matrix(-rlim[1], nrow=dim(toPlot)[1], ncol=dim(toPlot)[2])), dim=c(dim(toPlot)[1], dim(toPlot)[2], 2)), c(1, 2), min)
			rayons <- data.frame(rep(-rlim[1], dim(toPlot)[1]), apply(array(c(rayons, matrix(0, nrow=dim(toPlot)[1], ncol=dim(toPlot)[2])), dim=c(dim(toPlot)[1], dim(toPlot)[2], 2)), c(1, 2), max))
			rayons[is.na(rayons)] <- -rlim[1]
			x <- list()
			y <- list()
			for(j in 1:length(rayons)){
				x[[length(x)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), sin), SIMPLIFY=FALSE))
				y[[length(y)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), cos), SIMPLIFY=FALSE))}
			nb <- length(x)
			trash <- mapply(polygon, x=lapply(x, t), y=lapply(y, t), col=c(rep(col, ceiling(nb/length(col)))[1:(nb-1)], "white"), border=rep(border, ceiling(nb/length(border)))[1:nb], lty=rep(lty.border, ceiling(nb/length(lty.border)))[1:nb], lwd=rep(lwd.border, ceiling(nb/length(lwd.border)))[1:nb], MoreArgs=list(density=density))
			if(ray){
				rayons <- array(toPlot[,,i], dim=c(dim(toPlot)[1], dim(toPlot)[2]))
				rayons[is.na(rayons)] <- -rlim[1]
				secteur <- list(x1=rep(-rlim[1], dim(rayons)[1]), y1=rep(-rlim[1], dim(rayons)[1]), x2=sapply(apply(rayons, 1, max)-rlim[1], max, 0), y2=sapply(apply(rayons, 1, max)-rlim[1], max, 0))
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(infTemp)*pi/180), cos(unlist(infTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(supTemp)*pi/180), cos(unlist(supTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)
				rayons <- array(toPlot[,,i], dim=c(dim(toPlot)[1], dim(toPlot)[2]))
				secteur <- list(x1=rep(-rlim[1], dim(rayons)[1]), y1=rep(-rlim[1], dim(rayons)[1]), x2=sapply(apply(rayons, 1, min)-rlim[1], max, 0), y2=sapply(apply(rayons, 1, min)-rlim[1], max, 0))
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(infTemp)*pi/180), cos(unlist(infTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)
				rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(supTemp)*pi/180), cos(unlist(supTemp)*pi/180)), 2), SIMPLIFY=FALSE)
				segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)}
		}else if(typeRose=="boite"){
			rayons <- data.frame(apply(array(c(toPlot[,,i]-rlim[1], matrix(0, nrow=dim(toPlot)[1], ncol=dim(toPlot)[2])), dim=c(dim(toPlot)[1], dim(toPlot)[2], 2)), c(1, 2), max))
			rayons[is.na(rayons)] <- 0
			x <- list()
			y <- list()
			for(j in 1:length(rayons)){
				x[[length(x)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), sin), SIMPLIFY=FALSE))
				y[[length(y)+1]] <- unlist(mapply("*", rayons[[j]], lapply(lapply(theta, "*", pi/180), cos), SIMPLIFY=FALSE))}
			nb <- length(x)
			trash <- mapply(polygon, x=lapply(x, t), y=lapply(y, t), col=c(rep(col, ceiling(nb/length(col)))[1:(nb-1)], "white"), border=rep(border, ceiling(nb/length(border)))[1:nb], lty=rep(lty.border, ceiling(nb/length(lty.border)))[1:nb], lwd=rep(lwd.border, ceiling(nb/length(lwd.border)))[1:nb], MoreArgs=list(density=density))
			rayons <- array(toPlot[,,i], dim=c(dim(toPlot)[1], dim(toPlot)[2]))
			rayons[is.na(rayons)] <- 0
			secteur <- list(x1=sapply(apply(rayons, 1, min)-rlim[1], max, 0), y1=sapply(apply(rayons, 1, min)-rlim[1], max, 0), x2=sapply(apply(rayons, 1, max)-rlim[1], max, 0), y2=sapply(apply(rayons, 1, max)-rlim[1], max, 0))
			rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(infTemp)*pi/180), cos(unlist(infTemp)*pi/180)), 2), SIMPLIFY=FALSE)
			segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)
			rayons <- mapply("*", secteur, rep(data.frame(sin(unlist(infTemp)*pi/180), cos(unlist(infTemp)*pi/180)), 2), SIMPLIFY=FALSE)
			segments(rayons$x1, rayons$y1, rayons$x2, rayons$y2, col=col.ray, lwd=lwd.ray, lty=lty.ray)}
		if(missAtAxis){
			if(missRlim){
				temp <- ceiling(abs(rlim/10^floor(log(abs(rlim), 10))))*10^floor(log(abs(rlim), 10))*sign(rlim)
				temp <- ifelse(is.na(temp), 0, temp)
				at.axis <- seq(temp[1], temp[2], length=5)
			}else{
				at.axis <- seq(rlim[1], rlim[2], length=5)}}
		x <- as.data.frame(outer(cos(seq(0, 360, by=thetaPas)*pi/180), at.axis-rlim[1], "*"))
		y <- as.data.frame(outer(sin(seq(0, 360, by=thetaPas)*pi/180), at.axis-rlim[1], "*"))
		trash <- mapply(polygon, x=lapply(x, t), y=lapply(y, t), MoreArgs=list(density=0, border=col.axis, lwd=lwd.axis, lty=lty.axis))

		if(missLabelAxis){label.axis <- paste(at.axis, unites(rose)[names(rose) == i])}
		text(	(at.axis-rlim[1])*sin(rep(alpha.axis, ceiling(length(at.axis)/length(alpha.axis)))[1:length(at.axis)]*pi/180),
			(at.axis-rlim[1])*cos(rep(alpha.axis, ceiling(length(at.axis)/length(alpha.axis)))[1:length(at.axis)]*pi/180),
			label.axis, cex=cex.axis)
		if(missMain){title(main=i)}else if(is.null(main)){}else{title(main=main)}
		par(ask=TRUE)
		}
	par(ask=FALSE)
#	par(mar=parMar)
	}
