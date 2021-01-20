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

# valid.states TODO:

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

# validOnly TODO:

# test sur les fonctions de recherches seules et dans xrGetContinuousData
#========================================================================

# stations
xrGetStations

# mesures
xrGetMesures

# 
xrGetPolluants
xrGetReseaux
xrGetCampagnes

# polluants et mesures
# reseaux et polluants
# campagnes et stations


#==============================================================================
# DONNEES MANUELLES TODO:
#==============================================================================
# pas de tests pour l'instant : utilise l'ancien moteur
# xrGetManualData
# xrGetMethodesPrelevement
# xrGetSitesPrelevement


