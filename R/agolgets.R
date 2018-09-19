#' Fonction qui se connecte à ArcGIS online, sur les flux ASQAAs
#'
#' Cette fonction permet de faire une requête sur le flux et donc de n'en 
#' rappatrier qu'une partie (au lieu du flux complet). Sur des gros flux,
#' cela permet un gain de temps significatif quand seules quelques valeurs sont
#' recherchées.
#'
#' @param aasqa identifiant 'codé' de l'AASQA correspondant à ses urls ArcGIS
#' @param flux le nom du flux à interroger ('ind_nouvelle_aquitaine' par exemple)
#' @param where une requête à faire sur la table du flux. Attention les espaces
#'  (' ') doivent être remplacés par la chaîne de caractères '%20', sinon ça
#'  fonctionne comme une requête SQL.
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

    if(getGeometry) {
        returnGeometry <- 'true'
        f <- 'geojson'
    } else {
        returnGeometry <- 'false'
        f <- 'json'
    }

    url <- "https://services1.arcgis.com/%s/arcgis/rest/services/%s/FeatureServer/0/query?outFields=%s&where=%s&returnGeometry=%s&f=%s"
    url <- sprintf(url, aasqa, flux, paste(outFields, collapse=","),
                   where, returnGeometry, f)

    message(url)

    if(getGeometry) data <- readOGR(url, stringsAsFactors=FALSE) else {

        data <- getURL  (url)
        data <- fromJSON(data)

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
#' @inheritParams agolGetData
#' @examples
#' agolListFields(aasqa='AQwB7zFo5jNpvM0X', flux='ind_nouvelle_aquitaine')
agolListFields <- function(aasqa, flux) {
    url <- 'https://dservices1.arcgis.com/%s/arcgis/services/%s/WFSServer?service=wfs&request=describefeaturetype'
    url <- sprintf(url, aasqa, flux)

    message(url)

    infos  <- xmlTreeParse(getURL(url))
    fields <- infos[[1]]$children[[1]][[3]][[1]][[1]][[1]]
    fields <- xmlSApply(fields, xmlGetAttr, 'name')
    names(fields) <- NULL

    return(fields)
}

