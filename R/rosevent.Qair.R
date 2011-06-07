`rosevent.Qair` <-
function(donnees, formula,  breaksVent, vlim, leg=locator(1), col, cex.leg=0.7, ...){
	local.call <- match.call()
	local.args <- as.list(local.call)
	local.args <- local.args[names(local.args) != ""]
	dv <- attributes(terms(formula))$term.labels[1]
	vv <- attributes(terms(formula))$term.labels[2]
	if (!dv %in% names(donnees) | !vv %in% names(donnees)) 
		stop("donnees de vent non-definies")

	if(missing(vlim)){vlim <- c(quantile(donnees[[vv]], probs=1:4/4, na.rm=TRUE))
	}else if(max(donnees[[vv]],na.rm=T) > max(vlim)){vlim <- unique(c(eval(vlim), ceiling(max(donnees[[vv]],na.rm=T))))}
	vlim <- sort(vlim)
	if(missing(col)){col <- chaud(length(vlim)+3)[length(vlim):1+3]}

	argsTemp <- c(donnees=NULL, local.args[setdiff(names(local.args), c("donnees", "vlim"))])
	argsTemp$donnees <- donnees[c(dv, vv)]
	argsTemp$FUN <- "none"
	rose <- do.call("calcRose", argsTemp)[vv]
	tot <- sum(!is.na(donnees[[vv]]))
	argsTemp <- list(donnees=NULL, FUN="calcRoseVent", vlim=vlim, tot=tot)
	argsTemp$donnees <- rose
	rose <- do.call("calcRose", argsTemp)

	attributes(rose)$unite[2] <- "%"
	for(i in 1:length(rose))
		attributes(rose[[i]])$unite[2] <- "%"
	plot(rose, r=vv, col=col, ...)
	if(!is.null(leg))legend(leg, fill=col, paste("]", c(vlim[(length(vlim)-1):1],0),",", vlim[length(vlim):1], "] m/s",sep=""), cex=cex.leg)
#legend(leg, fill=col, paste("<", vlim[length(vlim):1], "m/s"), cex=0.7)
	invisible(rose)
	}
