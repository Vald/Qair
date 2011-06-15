`calcRose.Qair` <-
function(donnees, formula, breaksVent=0:36*10-5, FUN, ...){
#	reponse <- attributes(terms(formula))$factors
#	if(any(apply(reponse==0, 1, all))){
#		reponse <- sub(" ", "", unlist(strsplit(rownames(reponse)[1], "+", fixed=T)))
#		formula <- formula(paste("~", paste(attributes(terms(formula))$term.labels, collapse="+")))
#	}else{
#		reponse <- setdiff(names(donnees), "date")}
	local.call <- match.call()
	local.args <- as.list(local.call)
	local.args <- local.args[names(local.args) != ""]
	if(missing(FUN)){local.args$FUN="mean";local.args$na.rm=TRUE}
	dv <- attributes(terms(formula))$term.labels[1]
	vv <- attributes(terms(formula))$term.labels[2]
	if(!dv %in% names(donnees) | !vv %in% names(donnees))stop("donnees de vent non-definies")
	breaksVent <- sort(unique(breaksVent %% 360))
	dv <- donnees[[dv]]
	vv <- donnees[[vv]]
	attributesTemp <- attributes(donnees)
	donnees <- as.data.frame(donnees)
	notNA <- !is.na(dv) & !is.na(vv)
	donnees <- donnees[notNA, ]
	vv <- vv[notNA]
	dv <- dv[notNA]
	rose <- list()
	rose$ventNul <- donnees[vv==0,]
	if(nrow(rose$ventNul)==0){
		rose$ventNul <- donnees[1,]
		rose$ventNul[TRUE] <- NA}
	donnees <- donnees[vv!=0, ]
	dv <- dv[vv!=0] %% 360
	vv <- vv[vv!=0]
	repartition <- rowSums(outer(dv, breaksVent, ">="))
	repartition[repartition==0] <- length(breaksVent)
	rose <- c(rose, split(donnees, repartition))
	notIn <- as.character(which(!1:length(breaksVent) %in% as.numeric(names(rose)[-1])))
	for(i in notIn){
		rose[[i]] <- donnees[1,]
		rose[[i]][TRUE] <- NA}
	rose <- c(rose["ventNul"], rose[order(as.numeric(names(rose)[-1]))+1])

	lapply(rose, "class<-", attributesTemp$class)
	attributes(rose)$intervalle <- c(ventNul=NA, data.frame(rbind(breaksVent, c(breaksVent[-1], breaksVent[1]))))
	attributes(rose)$formula <- formula
	attributes(rose)$aggregation <- NA
	class(rose) <- c("rose", "list")
	if(local.args$FUN=="none"){
	}else{
		temp <- local.args[setdiff(names(local.args), c("donnees", "formula", "breaksVent"))]
		temp$donnees <- rose
		rose <- do.call("calcRose.rose", temp)
		}
#	rose[reponse]
	rose
	}
