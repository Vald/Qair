match.pattern.fields <- function (pattern, search.fields, type=c('LIKE', 'IN')) {
	type <- match.arg(type)
	if (type == 'LIKE') {
		pattern <- gsub ('_', '!_', pattern)
		return (sprintf ('(%s)',
			paste ( rep (search.fields, each = length(pattern) ),
				" LIKE '%", pattern, "%' ESCAPE '!'",
				sep = '', collapse = ' OR ') ) )
	} else if (type == 'IN') {
		return(paste('(', search.fields, ' IN (',
			     paste("'", pattern, "'", collapse=", ", sep=''),
			     ') )', sep='', collapse = ' OR '))
	}
}



