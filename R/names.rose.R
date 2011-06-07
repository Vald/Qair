`names.rose` <-
function(x){unique(unlist(sapply(lapply(x, attributes), "[", "names")))}

