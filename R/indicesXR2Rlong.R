indicesXR2Rlong <- function(xr, debut, fin, agglos, type='C') {
	debut	<- as.POSIXct(debut, 'UTC')-POSIXctp(2, 'day')
	fin		<- as.POSIXct(fin, 'UTC')

	dformat <- '%Y-%m-%dT%H:%M:%SZ'
	from    <- format (debut, format = dformat, tz='UTC')
	to      <- format (fin+POSIXctp('second'), format = dformat, tz='UTC')

	# récupération de la version avec laquelle on bosse et initialisation de --
	# la requête

	nv     <- paste0('nv', xr[['version']])
	bquery <- 'v2/disclosedAQI?'

	# liste des agglos disponibles --------------------------------------------
	# FIXME:ISEO à corriger chez ISEO (résultat vide)
	#agglos <- xrGetQuery(xr, 'v2/aqiGroups')
	# en attendant : attention solution partielle, les groupes pour lesquels pas 
	# d'indice sur la periode demandée n'apparaitront pas
	lagglos <- xrGetQuery(xr,
		'v2/disclosedAQI?from=2020-01-01T00:00:00Z&to=2020-01-02T00:00:00Z')
	lagglos <- lagglos[['group']]
	lagglos <- lagglos[c('idAqiGroup','idAqiIndex','idMeasureGroup','idCommune','areaName')]
	lagglos <- unique(lagglos)
	lagglos <- lagglos[order(lagglos[['idAqiIndex']],lagglos[['idMeasureGroup']]),]

	# idAqiGroups demandés
	iag <- lagglos[['idAqiGroup']][match(agglos, lagglos[['areaName']])]

	# creation de la requête et exécution -------------------------------------

	query <- sprintf('%sidAqiGroups=%s&withSubIndexes=true&from=%s&to=%s',
		bquery, paste(iag, collapse=','), from, to)

	indices <- xrGetQuery(xr, query)

	indices[['nom']] <- lagglos[['areaName']][
		match(indices[['idAqiGroup']], lagglos[['idAqiGroup']])]
	indices[['date']] <- as.POSIXct(strptime(indices[['date']], dformat, 'UTC'))

	# restriction aux seules données utiles -----------------------------------

	indices <- cbind(stringsAsFactors=FALSE,
		indices[c('date', 'nom')],
		indice  =indices[['full']][['disclose']][['disclosedValue']],
		SO2     =indices[['full']][['subIndexes']][['01']][['subIndexDisclosedValue']],
		NO2     =indices[['full']][['subIndexes']][['03']][['subIndexDisclosedValue']],
		O3      =indices[['full']][['subIndexes']][['08']][['subIndexDisclosedValue']],
		PM10    =indices[['full']][['subIndexes']][['24']][['subIndexDisclosedValue']],
		C_IND_PRV_J =indices[['full']][['forecast']][['forecastDp0Value']],
		C_POL_PRV_J =indices[['full']][['forecast']][['forecastDp0Pollutant']],
		C_IND_PRV_J1=indices[['full']][['forecast']][['forecastDp1Value']],
		p_ind   =indices[['anticipated']][['disclose']][['disclosedValue']],
		p_SO2   =indices[['anticipated']][['subIndexes']][['01']][['subIndexDisclosedValue']],
		p_NO2   =indices[['anticipated']][['subIndexes']][['03']][['subIndexDisclosedValue']],
		p_O3    =indices[['anticipated']][['subIndexes']][['08']][['subIndexDisclosedValue']],
		p_PM10  =indices[['anticipated']][['subIndexes']][['24']][['subIndexDisclosedValue']],
		p_ind_j =indices[['anticipated']][['forecast']][['forecastDp0Value']],
		p_pol_j =indices[['anticipated']][['forecast']][['forecastDp0Pollutant']],
		p_ind_j1=indices[['anticipated']][['forecast']][['forecastDp1Value']])

	# transformation en TIntervalDF -------------------------------------------
	# qu'on complète si ça ne va pas jusqu'à 'fin'

	indices <- TimeIntervalDataFrame(
		indices[['date']], indices[['date']]+POSIXctp('day'), 'UTC', indices)

	if(!fin %in% start(indices)) {
		indices <- split(indices, indices[['nom']])

		indices <- lapply(indices, function(i) {
			i <- i[order(start(i)),]

			f <- RegularTimeIntervalDataFrame(
				end(i)[nrow(i)], fin+POSIXctp('day'), POSIXctp('day'))

			f[['date']] <- start(f)
			f[['nom']]  <- i[['nom']][1]

			merge(i, f, all=TRUE)
		})

		indices <- do.call(rbind, indices)
	}

	# si indice partiel demandé, on remplace les valeurs du dernier jour ------
	# par les valeurs du partiel

	if (type == 'P')
		indices[start(indices) == fin,c('indice', 'SO2', 'NO2', 'O3', 'PM10',
										'C_IND_PRV_J', 'C_POL_PRV_J',
										'C_IND_PRV_J1')] <- 
		indices[start(indices) == fin,c('p_ind', 'p_SO2', 'p_NO2', 'p_O3',
										'p_PM10', 'p_ind_j', 'p_pol_j',
										'p_ind_j1')]

	# Utilisation des valeurs les plus récentes pour J+1 ----------------------

	# création d'identifiants temporels

	indices[['idjourmeme']] <- paste0(indices[['nom']], indices[['date']])
	indices[['idjoursuiv']] <- strptime(indices[['date']], '%Y-%m-%d', 'UTC')
	indices[['idjoursuiv']] <- as.POSIXct(indices[['idjoursuiv']])+POSIXctp('day')
	indices[['idjoursuiv']] <- paste0(indices[['nom']],
									  strftime(indices[['idjoursuiv']], '%Y-%m-%d'))

	# affectation des bonnes valeurs aux colonnes

	indices[['IJ1']] <- indices[['indice']][
		match(indices[['idjoursuiv']], indices[['idjourmeme']])]

	indices[['IJ1']] <- ifelse(
		is.na(indices[['IJ1']]),
		indices[['C_IND_PRV_J']][match(indices[['idjoursuiv']], indices[['idjourmeme']])],
		indices[['IJ1']])

	indices[['IJ1']] <- ifelse(
		is.na(indices[['IJ1']]),
		indices[['C_IND_PRV_J1']],
		indices[['IJ1']])

	# détermination des polluants faisant l'indice du jour --------------------

	indices[['PJ']] <- ifelse(
		is.na(indices[['indice']]),
		indices[['C_POL_PRV_J']],
		apply(indices@data[c('SO2', 'NO2', 'O3', 'PM10')], 1,
			  function(x) {
				  m <- which(x == max(x, na.rm=TRUE))
				  paste(c('01', '03', '08', '24')[m], collapse=',')
			  }))

	# si on a pas d'indice pour le jour-même on met la prévision --------------

	indices[['indice']] <- ifelse(is.na(indices[['indice']]),
								  indices[['C_IND_PRV_J']],
								  indices[['indice']])


	# finlaisation et renvoi du résultat --------------------------------------

	indices[['commentaire']] <- NA

	indices[c('indice', 'SO2', 'NO2', 'O3', 'PM10', 'IJ1')] <- lapply(
		indices[c('indice', 'SO2', 'NO2', 'O3', 'PM10', 'IJ1')],
		function(x) return(ifelse(x==0, NA, x)))

	return(indices@data[c('date','nom','indice','SO2','NO2','O3','PM10',
						  'IJ1','PJ','commentaire')])
}

