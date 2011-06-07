`as.list.rose` <-
function(x, ...){class(x) <- "list";temp <- names(x);attributes(x) <- NULL;names(x) <- temp;x}

