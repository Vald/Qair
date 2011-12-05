match.pattern.fields <- function (pattern, search.fields) {
	pattern <- gsub ('_', '!_', pattern)
	sprintf ('(%s)',
		 paste ('(', rep (search.fields, each = length(pattern) ),
			" LIKE '%", pattern, "%' ESCAPE '!')",
			sep = '', collapse = ' OR ') )
}



