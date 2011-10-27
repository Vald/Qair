# .First.lib <-function (lib, pkg) { 
.onLoad (lib, pkg) {
library.dynam("Qair", pkg, lib) 
} 
