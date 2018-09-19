#' definition d'un raccourci pour les heures et les quart-d'heures
#' 
#' Pour faciliter la manipulation des heures et des quart-heures
#' et permettre une utilisation comme c'est le cas pour les mois
#' les années etc. dans \code{\link[lubridate:seconds]{lubridate}}.
#' 
#' @seealso \code{\link[lubridate]{seconds}}
qh <- POSIXctp (15, 'minute')
#' @rdname qh
h <- POSIXctp (1, 'hour')
#' @rdname qh
d <- POSIXctp (1, 'day')
#' @rdname qh
m <- POSIXctp (1, 'month')
#' @rdname qh
y <- POSIXctp (1, 'year')

