library (Qair)
options(Xair.host = '172.16.16.16',
	Xair.dsn = 'N09',
	Xair.uid = 'vlad',
	Xair.pwd = 'vlad',
	Xair.drv='oracle')

# test de la connexion  à XR
#============================

xr <- xrConnect()

# test des différentes fonctions xrGet*
#======================================

f <- c('NOPOL', 'CCHIM', 'NCON')
p <- xrGetPolluants(xr)[f]
p[order(p[['NOPOL']]),]
p <- xrGetPolluants(xr, 'N2')[f]
p[order(p[['NOPOL']]),]
p <- xrGetPolluants(xr, 'und', 'NCON')[f]
p[order(p[['NOPOL']]),]

mp <- xrGetMethodesPrelevement(xr)
mp[order(mp[['CODE_METH_P']]),]

xrGetCampagnes(xr, 'LARO', end='2011-01-01')

f <- c('NOM_COURT_RES', 'NOM_RES', 'FLAG_RESEAURES')
r <- xrGetReseaux(xr, 'SO2')[f]
r[order(r[['NOM_COURT_RES']]),]

xrGetStations(xr, 'Chas')
s <- xrGetStations(xr, reseaux='PALR')
s[order(s[['NSIT']]),]
s <- xrGetStations(xr, reseaux='PALR', mesures='24')
s[order(s[['NSIT']]),]

summary(xrGetSitesPrelevement(xr, 'TLAROC_L'))
xrGetSitesPrelevement(xr, campagnes='TLAROC2009')

#==========================================
# tests sur la fonction xrGetContinuousData

xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01')
xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01', tz='UTC')
xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01', tz='CET')
xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01', cursor=1)
xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01', tz='UTC', cursor=1)
xrGetContinuousData(xr, 'NO2_VERDUN', '2012-09-30', '2012-10-01', tz='CET', cursor=1)

#======================================
# tests sur la fonction xrGetManualData

xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='UTC')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='CET')
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', cursor=0.5)
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='UTC', cursor=0.5)
xrGetManualData(xr, '2011-01-01', '2012-01-01', 'PNIORT_001', 'C6H6', tz='CET', cursor=0.5)

