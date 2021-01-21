library (Qair)
options(Xair.host='xair.atmo-na.org', Xair.version=2, Xair.port=8443, 
		Xair.dsn='N09', Xair.uid='vlad', Xair.pwd='vlad', Xair.drv='oracle')

# test de la connexion  à XR
#============================

xr <- xrConnect()

#==============================================================================
# DONNEES CONTINUES
#==============================================================================

# test simple de récupération de données
#=======================================

mes <- c('O3_AYTRE', 'PM10_NICOLAS', 'NO2_DALTON', 'SO2_LACQ', 'PM25_BILLERE')
# note le [mes] est pour assurer l'ordre des mesures
# horaires
xrGetContinuousData(xr, mes, '2018-06-07 08:00:00', '2019-07-01')[mes]
# QH
xrGetContinuousData(xr, mes, '2018-06-07 08:00:00', '2018-07-01', period='qh')[mes]
# journalières
xrGetContinuousData(xr, mes, '2018-06-07 08:00:00', '2019-07-01', period='d')[mes]
# mensuelles
xrGetContinuousData(xr, mes, '2018-06-07 08:00:00', '2019-07-01', period='m')[mes]
# annuelles
xrGetContinuousData(xr, mes, '2018-06-07 08:00:00', '2019-07-01', period='y')[mes]
# scan
xrGetContinuousData(xr, 'NO_BASTI', '2019-01-01 09:15:00', '2019-01-01 09:30:00', period='scan')
# exact
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04')
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04', search.fields='IDENTIFIANT')
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04', search.fields='NOM_COURT_MES')
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04', exact=TRUE)
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04', search.fields='IDENTIFIANT', exact=TRUE)
xrGetContinuousData(xr, 'O3_VERDUN', '2019-02-03', '2019-02-04', search.fields='IDENTIFIANT', exact=TRUE)
xrGetContinuousData(xr, 'O3_VER', '2019-02-03', '2019-02-04', search.fields='NOM_COURT_MES', exact=TRUE)
# validated / what
d <- '2020-02-19 23:00:00'
f <- '2020-02-20 02:00:00'
xrGetContinuousData(xr, mes[3], d, f, period='qh', what='state')
xrGetContinuousData(xr, mes[3], d, f, period='qh', what='validated')
xrGetContinuousData(xr, mes[3], d, f, period='qh', what='value')
xrGetContinuousData(xr, mes[3], d, f, validated=TRUE, period='qh')
xrGetContinuousData(xr, mes[3], d, f, validated=FALSE, period='qh')
xrGetContinuousData(xr, mes[3], d, f, validated=TRUE, period='qh', what=c('value', 'state', 'validated'))
xrGetContinuousData(xr, mes[3], d, f, validated=FALSE, period='qh', what=c('value', 'state', 'validated'))

# valid.states
xrGetContinuousData(xr, mes[1], '2019-09-24', '2019-09-24 08:30:00',
					what=c('value', 'state'), period='qh')
xrGetContinuousData(xr, mes[1], '2019-09-24', '2019-09-24 08:30:00',
					what=c('value', 'state'), period='qh', valid.states='I')
xrGetContinuousData(xr, mes[1], '2019-09-24', '2019-09-24 08:30:00',
					what=c('value', 'state'), period='qh', valid.states=c('Z', 'C'))

xrGetContinuousData(xr, mes[1], '2019-01-09', '2019-01-09 08:30:00',
					what=c('value', 'state'), period='qh')
xrGetContinuousData(xr, mes[1], '2019-01-09', '2019-01-09 08:30:00',
					what=c('value', 'state'), period='qh', valid.states='A')
xrGetContinuousData(xr, mes[1], '2019-01-09', '2019-01-09 08:30:00',
					what=c('value', 'state'), period='qh', valid.states=c('Z', 'C'))

# cursor/timezone
xrGetContinuousData(xr, mes[4], '2019-05-07', '2019-05-08')
xrGetContinuousData(xr, mes[4], '2019-05-07', '2019-05-08', cursor=0)
xrGetContinuousData(xr, mes[4], '2019-05-07', '2019-05-08', cursor=0.5)
xrGetContinuousData(xr, mes[4], '2019-05-07', '2019-05-08', cursor=1)

xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d')
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=0)
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=0.5)
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=1)
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=0, tz='CET')
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=0.5, tz='CET')
xrGetContinuousData(xr, mes[5], '2019-05-07', '2019-05-09', period='d', cursor=1, tz='CET')

# test sur les fonctions de recherches seules et dans xrGetContinuousData
#========================================================================

# stations --------------------------------------------------------------------
xrGetStations(xr, 'D87LIM_PRESID')
xrGetStations(xr, 'D87LIM_PRESID', search.fields='IDENTIFIANT')
xrGetStations(xr, 'D87LIM_PRESID', search.fields='NOM_COURT_SIT')
xrGetStations(xr, 'PRESID', search.fields='NOM_COURT_SIT')
xrGetStations(xr, '44803', search.fields='NSIT')
xrGetStations(xr, '35003', search.fields='NSIT_PUBLIC')
xrGetStations(xr, 'D87LIM_PRESID', exact=TRUE)
xrGetStations(xr, 'D87LIM_PRESID', search.fields='IDENTIFIANT', exact=TRUE)
xrGetStations(xr, 'D87LIM_PRESID', search.fields='NOM_COURT_SIT', exact=TRUE)
xrGetStations(xr, '7LIM_PRE')
xrGetStations(xr, '7LIM_PRE', search.fields='IDENTIFIANT')
xrGetStations(xr, '7LIM_PRE', exact=TRUE)
xrGetStations(xr, '7LIM_PRE', search.fields='IDENTIFIANT', exact=TRUE)
xrGetStations(xr, 'D87LIM_C')
xrGetStations(xr, c('D87LIM_C', 'D17LAR_C'))

xrGetStations(xr, campagnes='EUROC_20')
xrGetStations(xr, campagnes='EUROC_2020')
xrGetStations(xr, campagnes=list(pattern='EUROC_2020', exact=TRUE))
# FIXME: si aucune campagne ne correspond, retourne toutes les stations. Devrait n'en renvoyer aucune
xrGetStations(xr, campagnes=list(pattern='EUROC_20', exact=TRUE))

xrGetStations(xr, reseaux='RALQ1')
xrGetStations(xr, reseaux=list(pattern='RALQ1', exact=TRUE))
# FIXME: si aucune campagne ne correspond, retourne toutes les stations. Devrait n'en renvoyer aucune
xrGetStations(xr, reseaux=list(pattern='RALQ', exact=TRUE))

xrGetStations(xr, mesures='O3_PRE')
xrGetStations(xr, mesures=list(pattern='O3_PRE', exact=TRUE))
xrGetStations(xr, mesures=list(pattern='O3_PR', exact=TRUE))

# mesures ---------------------------------------------------------------------
xrGetMesures(xr, 'PM10_MARMANDE')
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='IDENTIFIANT')
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='NOM_COURT_MES')
xrGetMesures(xr, ':01908', search.fields='NOM_COURT_MES')
xrGetMesures(xr, 'PM10_MARMANDE', exact=TRUE)
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='IDENTIFIANT', exact=TRUE)
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='NOM_COURT_MES', exact=TRUE)
xrGetMesures(xr, 'PM10_MARM')
xrGetMesures(xr, 'PM10_MARM', search.fields='IDENTIFIANT')
xrGetMesures(xr, 'PM10_MARM', exact=TRUE)
xrGetMesures(xr, 'PM10_MARM', search.fields='IDENTIFIANT', exact=TRUE)
xrGetMesures(xr, 'PM10_MAR')
xrGetMesures(xr, c('PM10_MAR', 'PM10_VER'))

xrGetMesures(xr, campagnes='EUROC_20')
xrGetMesures(xr, campagnes='EUROC_2020')
xrGetMesures(xr, campagnes=list(pattern='EUROC_2020', exact=TRUE))
# FIXME: si aucune campagne ne correspond, plante (pbm de renvoi de tout ?)
xrGetMesures(xr, campagnes=list(pattern='EUROC_20', exact=TRUE))

xrGetMesures(xr, reseaux='RALQ1')
xrGetMesures(xr, reseaux=list(pattern='RALQ1', exact=TRUE))
# FIXME: si aucun reseau ne correspond, plante (pbm de renvoi de tout ?)
xrGetMesures(xr, reseaux=list(pattern='RALQ', exact=TRUE))

xrGetMesures(xr, stations='D87LIM_PRESID')
xrGetMesures(xr, stations=list(pattern='D87LIM_PRESID', exact=TRUE))
# FIXME: si aucune station ne correspond, plante (pbm de renvoi de tout ?)
xrGetMesures(xr, stations=list(pattern='D87LIM_PRESI', exact=TRUE))

xrGetMesures(xr, polluants='H2S')
xrGetMesures(xr, polluants=list(pattern='H2S', exact=TRUE))

xrGetMesures(xr, polluants='H2S', campagnes='EUROC_20')
xrGetMesures(xr,
			 polluants=list(pattern='05', exact=TRUE),
			 campagnes=list(pattern='EUROC_2019', exact=TRUE))
# FIXME: si pas de correspondance, renvoie tous les EUROC_2019
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 campagnes=list(pattern='EUROC_2019', exact=TRUE))
# FIXME: si pas de correspondance, renvoie tous les H2S
xrGetMesures(xr,
			 polluants=list(pattern='05', exact=TRUE),
			 campagnes=list(pattern='EUROC_1980', exact=TRUE))

xrGetMesures(xr, polluants='PM', reseaux='VH')
xrGetMesures(xr, polluants='PM10', reseaux='VHBX')
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 reseaux=list(pattern='VHBX', exact=TRUE))
# FIXME: si pas de correspondance renvoie tous les PM10
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 reseaux=list(pattern='VHX', exact=TRUE))
# FIXME: si pas de correspondance renvoie tous les VHBX
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 reseaux=list(pattern='VHBX', exact=TRUE))

xrGetMesures(xr, polluants='PM', stations='PRESID')
xrGetMesures(xr, polluants='PM10', stations='PRESID')
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 stations=list(pattern='PRESID', exact=TRUE))
# FIXME: si pas de correspondance renvoie tous les PM10
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 stations=list(pattern='PREID', exact=TRUE))
# FIXME: si pas de correspondance renvoie tous les PRESID
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 stations=list(pattern='PRESID', exact=TRUE))

# polluants -------------------------------------------------------------------
xrGetPolluants(xr)
xrGetPolluants(xr, c('24', 'PM2.5'))
xrGetPolluants(xr, c('24', 'PM2.5'), exact=TRUE)
xrGetPolluants(xr, c('24', 'PM2.5'), exact=TRUE, search.fields='CCHIM')

# reseaux ---------------------------------------------------------------------
xrGetReseaux(xr, 'RALQ1')
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_RES')
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_COURT_RES')
xrGetReseaux(xr, 'RALQ1', exact=TRUE)
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_RES', exact=TRUE)
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_COURT_RES', exact=TRUE)
xrGetReseaux(xr, 'RALQ')
xrGetReseaux(xr, 'RALQ', search.fields='NOM_RES')
xrGetReseaux(xr, 'RALQ', exact=TRUE)
xrGetReseaux(xr, 'RALQ', search.fields='NOM_RES', exact=TRUE)
xrGetReseaux(xr, c('RALQ', 'VH'))

# campagnes -------------------------------------------------------------------
xrGetCampagnes(xr, 'PANTA_2011')
xrGetCampagnes(xr, 'PANTA_2011')
xrGetCampagnes(xr, 'PANTA_2011', search.fields='LIBELLE')
xrGetCampagnes(xr, 'PANTA_2011', search.fields='NOM_COURT_CM')
xrGetCampagnes(xr, 'PANTA_2011', exact=TRUE)
xrGetCampagnes(xr, 'PANTA_2011', search.fields='LIBELLE', exact=TRUE)
xrGetCampagnes(xr, 'PANTA_2011', search.fields='NOM_COURT_CM', exact=TRUE)
xrGetCampagnes(xr, 'Pantaléon')
xrGetCampagnes(xr, 'Pantaléon', search.fields='LIBELLE')
xrGetCampagnes(xr, 'Pantaléon', exact=TRUE)
xrGetCampagnes(xr, 'Pantaléon', search.fields='LIBELLE', exact=TRUE)
xrGetCampagnes(xr, c('PANT', 'EUROC'))


#==============================================================================
# DONNEES MANUELLES TODO:
#==============================================================================
# pas de tests pour l'instant : utilise l'ancien moteur
# xrGetManualData
# xrGetMethodesPrelevement
# xrGetSitesPrelevement


