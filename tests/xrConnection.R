library (Qair)
options(Xair.host='xair.atmo-na.org', Xair.version=2, Xair.port=8443)

# test de la connexion  à XR
#============================

xr <- xrConnect()

# test des différentes fonctions xrGet*
#======================================

summary(xrGetPolluants(xr))
xrGetPolluants(xr, 'N2')
xrGetPolluants(xr, 'und', 'NCON')

xrGetMethodesPrelevement(xr)

xrGetCampagnes(xr, 'LARO', end='2011-01-01')

# xrGetReseaux(xr, 'R_SO2')

colonnes <- c(63, 64, 70, 92)
xrGetStations(xr, 'Chas')[-colonnes]
xrGetStations(xr, campagnes='ANG12')[-colonnes]
xrGetStations(xr, campagnes='ANG12', mesures='24')[-colonnes]

summary(xrGetSitesPrelevement(xr, 'TLAROC_L'))
xrGetSitesPrelevement(xr, campagnes='TLAROC2009')

#==========================================
# tests sur la fonction xrGetContinuousData

xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01')
xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01', tz='UTC')
xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01', tz='CET')
xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01', cursor=1)
xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01', tz='UTC', cursor=1)
xrGetContinuousData(xr, 'N2_VER', '2012-09-30', '2012-10-01', tz='CET', cursor=1)

#======================================
# tests sur la fonction xrGetManualData

xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='UTC')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='CET')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', cursor=0.5)
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='UTC', cursor=0.5)
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='CET', cursor=0.5)

