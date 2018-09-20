#' Fonction qui se connecte à ArcGIS online, sur les flux ASQAAs
#'
#' Cette fonction permet de faire une requête sur le flux et donc de n'en 
#' rappatrier qu'une partie (au lieu du flux complet). Sur des gros flux,
#' cela permet un gain de temps significatif quand seules quelques valeurs sont
#' recherchées.
#'
#' @param aasqa identifiant 'codé' de l'AASQA correspondant à ses URLs ArcGIS.
#'  Le code en question est la suite de caractères sans signification évidente
#'  qui apparaît systématiquement dans les URLs des flux WFS. Par exemple
#'  'AQwB7zFo5jNpvM0X' dans :
#'  "https://dservices1.arcgis.com/AQwB7zFo5jNpvM0X/arcgis/services/ind_nouvelle_aquitaine/WFSSe..."
#' @param flux le nom du flux à interroger.
#'  Il ne s'agit pas exactement du nom du flux mais de celui de la FeatureLayer
#'  qui peut être trouvée sur la page suivante dans l'encadré "URL de requête".
#'  Il s'agit de la partie entre "services/" et "/FeatureServer" (donc
#'  'ind_nouvelle_aquitaine' dans l'exemple).
#'  https://data-atmo-na.opendata.arcgis.com/datasets/ind-nouvelle-aquitaine/geoservice.
#'  Si les FeatureLayer et les flux WFS ont été nommés de la même façon, il 
#'  s'agit tout simplement du nom du flux.
#' @param where une requête à faire sur la table du flux. Ça fonctionne comme
#'  une requête SQL.
#' @param outFields Vecteurs de chaînes de caractères indiquant les champs à
#'  récupérer. La valeur par défaut '*' signifie 'tous'.
#' @param getGeometry Si TRUE, un Spatial*DataFrame est retourné, sinon une data.frame.
#' @examples
#' ingeom <- agolGetData(aasqa='AQwB7zFo5jNpvM0X', flux='ind_nouvelle_aquitaine',
#'                        where="date%20IN%20('2018-09-17','2018-09-18','2018-09-19')")
#' indice <- agolGetData(aasqa='AQwB7zFo5jNpvM0X', flux='ind_nouvelle_aquitaine',
#'                        outFields=c('lib_zone', 'date', 'valeur'),
#'                        where="date%20IN%20('2018-09-17','2018-09-18','2018-09-19')",
#'                        getGeometry=FALSE)
agolGetData <- function(aasqa, flux, where='1=1', outFields='*', getGeometry=TRUE) {
    loadNamespace('rgdal')
    loadNamespace('RCurl')
    loadNamespace('utils')

    if(getGeometry) {
        returnGeometry <- 'true'
        f <- 'geojson'
    } else {
        loadNamespace('rjson')
        returnGeometry <- 'false'
        f <- 'json'
    }

    url <- "https://services1.arcgis.com/%s/arcgis/rest/services/%s/FeatureServer/0/query?outFields=%s&where=%s&returnGeometry=%s&f=%s"
    url <- sprintf(url, aasqa, flux, paste(outFields, collapse=","),
                   where, returnGeometry, f)
    url <- URLencode(url)

    message(url)

    if(getGeometry) data <- rgdal::readOGR(url, stringsAsFactors=FALSE) else {

        data <- RCurl::getURL  (url)
        data <- rjson::fromJSON(data)

        ndata<- sapply(data$fields, '[[', 'name')

        data <- lapply(ndata, function(n)
                       sapply(data$features, function(x) x$attributes[[n]]))
        data <- as.data.frame(data)
        names(data) <- ndata
    }

    return(data)
}

#' Fonction pour lister les champs d'un flux
#'
#' @param flux le nom du flux à interroger ('ind_nouvelle_aquitaine' par exemple).
#'  Contrairement à celui utilisé pour la fonction agolGetData, il s'agit effectivement
#'  du nom du flux tel qu'il apparaît dans les URLs des flux WFS. Par exemple
#'  'ind_nouvelle_aquitaine' dans :
#'  "https://dservices1.arcgis.com/AQwB7zFo5jNpvM0X/arcgis/services/ind_nouvelle_aquitaine/WFSSe..."
#' @inheritParams agolGetData
#' @examples
#' agolListFields(aasqa='AQwB7zFo5jNpvM0X', flux='ind_nouvelle_aquitaine')
agolListFields <- function(aasqa, flux) {
    loadNamespace('RCurl')
    loadNamespace('XML')
    loadNamespace('utils')

    url <- 'https://dservices1.arcgis.com/%s/arcgis/services/%s/WFSServer?service=wfs&request=describefeaturetype'
    url <- sprintf(url, aasqa, flux)
    url <- URLencode(url)

    message(url)

    infos  <- XML::xmlTreeParse(RCurl::getURL(url))
    fields <- infos[[1]]$children[[1]][[3]][[1]][[1]][[1]]
    fields <- XML::xmlSApply(fields, XML::xmlGetAttr, 'name')
    names(fields) <- NULL

    return(fields)
}

