#' definition d'un raccourci pour les heures et les quart-d'heures
#' 
#' Pour faciliter la manipulation des heures et des quart-heures
#' et permettre une utilisation comme c'est le cas pour les mois
#' les années etc. dans \code{\link[lubridate:seconds]{lubridate}}.
#' 
#' @seealso \code{\link[lubridate]{seconds}}
qh <- new_period (minutes=15)
#' @rdname qh
h <- new_period (hours=1)

# .First.lib <-function (lib, pkg) { 
.onLoad <- function (lib, pkg) {
library.dynam("Qair", pkg, lib) 
} 
