calcRoseVent <- function(vv, vlim, tot){colSums(outer(vv, vlim, "<="), na.rm=TRUE)/tot*100}
