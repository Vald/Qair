library (Qair)
options(Xair.host='xair.atmo-na.org', Xair.version=2, Xair.port=8443, 
		Xair.dsn='N09', Xair.uid='vlad', Xair.pwd='vlad', Xair.drv='oracle',
		Xair.silent=TRUE, width=100)

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
xrGetContinuousData(xr, mes, '2017-06-07 08:00:00', '2020-07-01', period='y')[mes]
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
xrGetContinuousData(xr, mes[3], d, f, validated=TRUE, period='qh', what=c('value', 'state'))
xrGetContinuousData(xr, mes[3], d, f, validated=FALSE, period='qh', what=c('value', 'state'))

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
fields <- c('IDENTIFIANT','NOM_COURT_SIT','NSIT','ISIT',
					 'D_CREATION','D_ARRET',
					 'NSIT_PUBLIC','ISIT_LONG',
					 'LATI', 'LONGI', 'ALTI',
					 'AXE', 'CODE_POSTAL', 
					 'LAMBERTX', 'LAMBERTY',
					 'DENSITE','NBRE_SOURCES',
					 'NB_VEHICULE','RAYON_ACTION',
					 'DER_LECT_QH',
					 'CLASSE_SITE','SITE_TYPE',
					 'TYPE_LIEU_ECHAN','TYPE_VOIE',
					 'typologie',
					 'TYPE_SECTEUR','ZONE_ACTIVITE'
					 )
xrGetStations(xr, 'D87LIM_PRESID')[fields]
xrGetStations(xr, 'D87LIM_PRESID', search.fields='IDENTIFIANT')[fields]
xrGetStations(xr, 'D87LIM_PRESID', search.fields='NOM_COURT_SIT')[fields]
xrGetStations(xr, 'PRESID', search.fields='NOM_COURT_SIT')[fields]
xrGetStations(xr, '44803', search.fields='NSIT')[fields]
xrGetStations(xr, '35003', search.fields='NSIT_PUBLIC')[fields]
xrGetStations(xr, 'D87LIM_PRESID', exact=TRUE)[fields]
xrGetStations(xr, 'D87LIM_PRESID', search.fields='IDENTIFIANT', exact=TRUE)[fields]
xrGetStations(xr, 'D87LIM_PRESID', search.fields='NOM_COURT_SIT', exact=TRUE)[fields]
xrGetStations(xr, '7LIM_PRE')[fields]
xrGetStations(xr, '7LIM_PRE', search.fields='IDENTIFIANT')[fields]
xrGetStations(xr, '7LIM_PRE', exact=TRUE)[fields]
xrGetStations(xr, '7LIM_PRE', search.fields='IDENTIFIANT', exact=TRUE)[fields]
xrGetStations(xr, 'D87LIM_C')[fields]
xrGetStations(xr, c('D87LIM_C', 'D17LAR_C'))[fields]

xrGetStations(xr, campagnes='EUROC_20')[fields]
xrGetStations(xr, campagnes='EUROC_2020')[fields]
xrGetStations(xr, campagnes=list(pattern='EUROC_2020', exact=TRUE))[fields]
xrGetStations(xr, campagnes=list(pattern='EUROC_20', exact=TRUE))[fields]

xrGetStations(xr, reseaux='RALQ1')[fields]
xrGetStations(xr, reseaux=list(pattern='RALQ1', exact=TRUE))[fields]
xrGetStations(xr, reseaux=list(pattern='RALQ', exact=TRUE))[fields]

xrGetStations(xr, mesures='O3_PRE')[fields]
xrGetStations(xr, mesures=list(pattern='O3_PRE', exact=TRUE))[fields]
xrGetStations(xr, mesures=list(pattern='O3_PR', exact=TRUE))[fields]

# mesures ---------------------------------------------------------------------
fields <- c('IDENTIFIANT','NOM_COURT_MES','NOM_MES','TYPE_MESURE','TYPE_ACQ',
					 'D_CREATION', 'D_ARRET','DERNIER_QH', 'D_VALIDATION',
					 'D_VALIDATION_ENV','D_ADVAL',
					 'NSIT','NOM_COURT_SIT',
					 'UNITE','NOPOL')
xrGetMesures(xr, 'PM10_MARMANDE')[fields]
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='IDENTIFIANT')[fields]
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='NOM_COURT_MES')[fields]
xrGetMesures(xr, ':01908', search.fields='NOM_COURT_MES')[fields]
xrGetMesures(xr, 'PM10_MARMANDE', exact=TRUE)[fields]
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='IDENTIFIANT', exact=TRUE)[fields]
xrGetMesures(xr, 'PM10_MARMANDE', search.fields='NOM_COURT_MES', exact=TRUE)[fields]
xrGetMesures(xr, 'PM10_MARM')[fields]
xrGetMesures(xr, 'PM10_MARM', search.fields='IDENTIFIANT')[fields]
xrGetMesures(xr, 'PM10_MARM', exact=TRUE)[fields]
xrGetMesures(xr, 'PM10_MARM', search.fields='IDENTIFIANT', exact=TRUE)[fields]
xrGetMesures(xr, 'PM10_MAR')[fields]
xrGetMesures(xr, c('PM10_MAR', 'PM10_VER'))[fields]

xrGetMesures(xr, campagnes='EUROC_20')[fields]
xrGetMesures(xr, campagnes='EUROC_2020')[fields]
xrGetMesures(xr, campagnes=list(pattern='EUROC_2020', exact=TRUE))[fields]
xrGetMesures(xr, campagnes=list(pattern='EUROC_20', exact=TRUE))[fields]

xrGetMesures(xr, reseaux='RALQ1')[fields]
xrGetMesures(xr, reseaux=list(pattern='RALQ1', exact=TRUE))[fields]
xrGetMesures(xr, reseaux=list(pattern='RALQ', exact=TRUE))[fields]

xrGetMesures(xr, stations='D87LIM_PRESID')[fields]
xrGetMesures(xr, stations=list(pattern='D87LIM_PRESID', exact=TRUE))[fields]
xrGetMesures(xr, stations=list(pattern='D87LIM_PRESI', exact=TRUE))[fields]

xrGetMesures(xr, polluants='H2S')[fields]
xrGetMesures(xr, polluants=list(pattern='H2S', exact=TRUE))[fields]

xrGetMesures(xr, polluants='H2S', campagnes='EUROC_20')[fields]
xrGetMesures(xr,
			 polluants=list(pattern='05', exact=TRUE),
			 campagnes=list(pattern='EUROC_2019', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 campagnes=list(pattern='EUROC_2019', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='05', exact=TRUE),
			 campagnes=list(pattern='EUROC_1980', exact=TRUE))[fields]

xrGetMesures(xr, polluants='PM', reseaux='VH')[fields]
xrGetMesures(xr, polluants='PM10', reseaux='VHBX')[fields]
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 reseaux=list(pattern='VHBX', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 reseaux=list(pattern='VHX', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 reseaux=list(pattern='VHBX', exact=TRUE))[fields]

xrGetMesures(xr, polluants='PM', stations='PRESID')[fields]
xrGetMesures(xr, polluants='PM10', stations='PRESID')[fields]
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 stations=list(pattern='PRESID', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='24', exact=TRUE),
			 stations=list(pattern='PREID', exact=TRUE))[fields]
xrGetMesures(xr,
			 polluants=list(pattern='29', exact=TRUE),
			 stations=list(pattern='PRESID', exact=TRUE))[fields]

# polluants -------------------------------------------------------------------
fields <- c('NOPOL', 'CCHIM', 'NCON')
xrGetPolluants(xr)[fields]
xrGetPolluants(xr, c('24', 'PM2.5'))[fields]
xrGetPolluants(xr, c('24', 'PM2.5'), exact=TRUE)[fields]
xrGetPolluants(xr, c('24', 'PM2.5'), exact=TRUE, search.fields='CCHIM')[fields]

# reseaux ---------------------------------------------------------------------
fields <- c('NOM_COURT_RES', 'NOM_RES', 'FLAG_RESEAURES')
xrGetReseaux(xr, 'RALQ1')[fields]
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_RES')[fields]
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_COURT_RES')[fields]
xrGetReseaux(xr, 'RALQ1', exact=TRUE)[fields]
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_RES', exact=TRUE)[fields]
xrGetReseaux(xr, 'RALQ1', search.fields='NOM_COURT_RES', exact=TRUE)[fields]
xrGetReseaux(xr, 'RALQ')[fields]
xrGetReseaux(xr, 'RALQ', search.fields='NOM_RES')[fields]
xrGetReseaux(xr, 'RALQ', exact=TRUE)[fields]
xrGetReseaux(xr, 'RALQ', search.fields='NOM_RES', exact=TRUE)[fields]
xrGetReseaux(xr, c('RALQ', 'VH'))[fields]

# campagnes -------------------------------------------------------------------
fields <- c('NOM_COURT_CM','LIBELLE','COMMENTAIRE','DATEDEB','DATEFIN')
xrGetCampagnes(xr, 'PANTA_2011')[fields]
xrGetCampagnes(xr, 'PANTA_2011')[fields]
xrGetCampagnes(xr, 'PANTA_2011', search.fields='LIBELLE')[fields]
xrGetCampagnes(xr, 'PANTA_2011', search.fields='NOM_COURT_CM')[fields]
xrGetCampagnes(xr, 'PANTA_2011', exact=TRUE)[fields]
xrGetCampagnes(xr, 'PANTA_2011', search.fields='LIBELLE', exact=TRUE)[fields]
xrGetCampagnes(xr, 'PANTA_2011', search.fields='NOM_COURT_CM', exact=TRUE)[fields]
xrGetCampagnes(xr, 'Pantaléon')[fields]
xrGetCampagnes(xr, 'Pantaléon', search.fields='LIBELLE')[fields]
xrGetCampagnes(xr, 'Pantaléon', exact=TRUE)[fields]
xrGetCampagnes(xr, 'Pantaléon', search.fields='LIBELLE', exact=TRUE)[fields]
xrGetCampagnes(xr, c('PANT', 'EUROC'))[fields]


#==============================================================================
# DONNEES MANUELLES TODO:
#==============================================================================
# pas de tests pour l'instant : utilise l'ancien moteur
# xrGetManualData
# xrGetMethodesPrelevement
# xrGetSitesPrelevement


