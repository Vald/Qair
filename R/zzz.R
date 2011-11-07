# .First.lib <-function (lib, pkg) { 
.onLoad <- function (lib, pkg) {
library.dynam("Qair", pkg, lib) 
} 
