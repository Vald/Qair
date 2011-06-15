`rosepol.Qair` <-
function(donnees, formula, breaksVent=0:36*10-5, FUN, ...){
	local.args <- as.list(match.call())
	local.args <- local.args[names(local.args) != ""]
	if(missing(FUN)){local.args$FUN="mean";local.args$na.rm=TRUE}

	plot.args <- names(local.args)[names(local.args)%in%names(as.list(args(plot.rose)))]
	calcRose.args <- setdiff(names(local.args), plot.args)

	rose <- do.call("calcRose.Qair", local.args[calcRose.args])
	temp <- local.args[plot.args]
	temp$x <- rose
	do.call("plot.rose", temp)

	invisible(rose)
	}
