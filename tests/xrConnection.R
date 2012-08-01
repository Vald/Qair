library (Qair)
options(Xair.host = '172.16.19.33',
	Xair.dsn = 'N09',
	Xair.uid = 'vlad',
	Xair.pwd = 'vlad',
	Xair.ojdbc.file = '/usr/share/java/ojdbc14.jar',
	Xair.drv='jdbc')

# test de la connection  à XR
#============================

xr <- xrConnect()

# test des différentes fonctions xrGet*
#======================================

summary(xrGetPolluants(xr))
xrGetPolluants(xr, 'N2')
xrGetPolluants(xr, 'und', 'NCON')

xrGetMethodesPrelevement(xr)

xrGetCampagnes(xr, 'LARO', end='2011-01-01')

xrGetReseaux(xr, 'R_SO2')

colonnes <- c(63, 64, 70, 92)
xrGetStations(xr, 'Chas')[-colonnes]
xrGetStations(xr, mesures='SO2')[-colonnes]
xrGetStations(xr, reseaux='R_SO2')[-colonnes]
xrGetStations(xr, campagnes='ANG12')[-colonnes]
xrGetStations(xr, campagnes='ANG12', mesures='24')[-colonnes]
xrGetStations(xr, campagnes='ANG12', mesures='SO2', collapse='OR')[-colonnes]

summary(xrGetSitesPrelevement(xr, 'TLAROC_L'))
xrGetSitesPrelevement(xr, campagnes='TLAROC2009')

# Test de la fonction xrGetMesures
#---------------------------------

# les arguments suivant vont être combinés pour vérifier la compatibilité
# entre leurs usages

arguments <- list (
		list	(campagnes = 'ANG12',
			 stations = 'ANG_14',
			 polluants = '24'),
		list 	(pattern = 'VER',
			 reseaux = 'INDICE',
			 polluants = '03'))

dbDisconnect(xr)
for (args in arguments)
{
	xr <- xrConnect()

	# determination de toutes les combinaisons possibles d'arguments

	tmp <- unlist( lapply (FUN = combn,
			1:length(args),
			x=args, simplify=FALSE),
	       recursive=FALSE)

	# on restreint les tests aux listes d'arguments composés de 2 éléments 
	# minimum (ça évite de demander tous les PM10 qui évoluent sans cesse)
	
	tmp <- tmp[sapply(tmp, length) > 1]

	# ajout de la connection à XR dans chacune des combinaisons d'arugments

	for (i in 1:length(tmp) ) {
		tmp[[i]]$conn <- xr
	}

	# tests

	print( lapply(lapply (FUN = do.call, tmp, what=xrGetMesures), function(x) summary(x[setdiff(names(x), c('NO_APPAREIL', 'TYPE_RUE'))])))

	dbDisconnect(xr)
}
rm(args, tmp)
xr <- xrConnect()

# test de la fonction xrGetContinuousData
#----------------------------------------

# les arguments suivant vont être combinés pour vérifier la compatibilité
# entre leurs usages

arguments <- list (
		list	(campagnes = 'ANG12',
			 stations = '14',
			 polluants = '24'),
		list 	(pattern = 'VER',
			 reseaux = 'INDICE',
			 polluants = '03'))

dbDisconnect(xr)
for (args in arguments)
{
	xr <- xrConnect()

	# determination de toutes les combinaisons possibles d'arguments

	tmp <- unlist( lapply (FUN = combn,
			1:length(args),
			x=args, simplify=FALSE),
	       recursive=FALSE)

	# ajout de valeurs pour les arguments 'period', 'what', 'conn',
	# 'start' et 'end'
	
	tmp <- mapply (SIMPLIFY=FALSE, c, tmp,
		       period = rep( c('h', 'qh', 'd', 'm', 'y'),
				     length.out=length(tmp)),
		       what = rep( c('value', 'state', 'both'),
				   length.out=length(tmp) ),
		       start = '2010-12-06',
		       end = rep(c('2011-03-05', '2011-01-18', '2011-09-24',
				   '2012-02-05', '2012-02-05'),
				 length.out=length(tmp)))

	# ajout de la connection à XR dans chacune des combinaisons d'arugments

	for (i in 1:length(tmp) ) {
		tmp[[i]]$conn <- xr
	}

	# tests

	print(lapply(lapply (FUN = do.call, tmp, what=xrGetContinuousData), summary))

	dbDisconnect(xr)
}
rm(args, tmp)
xr <- xrConnect()

# test de la fonction xrGetManualData
#------------------------------------

# les arguments suivant vont être combinés pour vérifier la compatibilité
# entre leurs usages

arguments <-list(campagnes = 'LEROY_1009',
		 sites = 'PLEROY_002',
		 polluants = '87')

dbDisconnect(xr)
xr <- xrConnect()

# determination de toutes les combinaisons possibles d'arguments

tmp <- unlist( lapply (FUN = combn,
		1:length(arguments),
		x=arguments, simplify=FALSE),
       recursive=FALSE)

# ajout de valeurs pour les arguments 'period', 'what', 'conn',
# 'start' et 'end'

tmp <- mapply (SIMPLIFY=FALSE, c, tmp,
	       what = rep( c('value', 'state', 'both'),
			   length.out=length(tmp) ),
	       MoreArgs=list(
			     start = '2009-12-06',
			     end = '2011-02-05'))
for (i in 1:length(tmp) ) {
	tmp[[i]]$conn <- xr
}

# tests

print(lapply( lapply (FUN = do.call, tmp, what=xrGetManualData), summary))

dbDisconnect(xr)
rm(tmp)

