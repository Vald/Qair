library (Qair)
options(Xair.host='xair.atmo-na.org', Xair.version=2, Xair.port=8443,
		Xair.dsn='N09', Xair.uid='vlad', Xair.pwd='vlad', Xair.drv='oracle')

# test des calculs de valeurs réglementaires
#===========================================
# les calculs tests sont basés sur le réseau de mesure
# de la qualité de l'air de La Rochelle au cours de l'année 2011
# (à l'exception du CO qui n'est plus mesuré depuis longtemps)

stations<- c('VERDUN', 'AYTRE', 'VAUGOI', 'STLOUIS_LR')
sites	<- 'PLAROC_023'
debut	<- '2011-01-01'
fin	<- '2012-01-01'
xr	<- xrConnect()

# Les polluants 'continus' (or CO)
#=================================

polluants <- c('03', '12', '24', '39', '08', '01')
for (polluant in polluants)
{
	donnees <- xrGetContinuousData (xr, stations=stations, start=debut, end=fin,
					polluants=polluant)
	tmp <- validation.reglementaire (donnees, seuils(polluant=polluant),
					 resultat='comparaison')
	tmp <- lapply (tmp, function(x)
		       if (nrow(x) > 1) lapply (x, sum, na.rm=TRUE) else x)

	# affichage
	print (xrGetPolluants(xr, search.fields='NOPOL', polluant)$NCON)
	print (tmp)
}

# le monoxyde de carbone CO
#==========================

polluant <- '04'
donnees <- xrGetContinuousData (xr, stations=stations, start='2004-01-01',
				end='2005-01-01', polluants=polluant)
tmp <- validation.reglementaire (donnees, seuils(polluant=polluant),
				 resultat='comparaison')
tmp <- lapply (tmp, function(x)
	       if (nrow(x) > 1) lapply (x, sum, na.rm=TRUE) else x)
print (xrGetPolluants(xr, search.fields='NOPOL', polluant)$NCON)
print(tmp)

# les polluants manuels
#======================

polluants <- c('19', 'V4', '80', '82', '87', 'P6')
for (polluant in polluants)
{
	donnees <- xrGetManualData (xr, start=debut, end=fin, sites=sites,
					polluants=polluant)
	tmp <- validation.reglementaire (donnees, seuils(polluant=polluant),
					 etapes=c('preparation.comparaison',
						  'comparaison'),
					 resultat='detail',
					 rep.b.comparaison = 0.1)
	# on met 0.1 à la place de 0.14 parce que dans le cas présent
	# on est entre 0.11 et 0.13

	tmp <- lapply (tmp, function(x)
		       if (nrow(x) > 1) lapply (x, sum, na.rm=TRUE) else x)

	# affichage
	print (xrGetPolluants(xr, search.fields='NOPOL', polluant)$NCON)
	print (tmp)
}

# On ferme
#=========

xrDisconnect(xr)
