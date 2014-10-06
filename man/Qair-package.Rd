\name{Qair-package}
\alias{Qair-package}
\alias{Qair}
\docType{package}
\title{	Interface avec une base de donnees XR, outils de traitement
	des donnees de vent et de calculs reglementaires (niveaux
	Europe et France).
}
\description{
	Le paquet \sQuote{Qair} propose toute une série de fonction
	destinées à faciliter l'interface entre R et les base de données
	\sQuote{XR} qui représente l'un des principaux systèmes de stockage
	des données de qualité de l'air en France. Ces fonctions sont
	repérables par leur nommage en \sQuote{xrGet...} (pour la majorité).
	Les données manuelles aussi bien que continues sont récupérables
	via cette interface. Le format de données utilisé est le format
	\code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
	défini dans le paquet \code{\link[timetools]{timetools}}.
	
	\sQuote{Qair} propose également une série de fonctions destinées
	à traiter les données de vents que ce soit pour réaliser des roses
	de vents ou bien des roses de concentrations (cf \code{\link{rosevent}}
	et \code{\link{rosepol}}).

	\sQuote{Qair} définit également de manière formalisée un grand nombre
	des critères réglementaires applicables dans le domaine de la qualité
	de l'air (Europe et France). Il propose également une série de 
	fonctions destinées à vérifier que des données (issues de XR par 
	exemple) respectent ces critères réglementaires
	(cf \code{\link{validation.reglementaire}} et \code{\link{seuils}}).

	\sQuote{Qair} intègre également d'autres outils plus ou moins spécifique
	comme les plans de sondage.

}
\details{
\tabular{ll}{
Package: \tab Qair\cr
Type: \tab Package\cr
Version: \tab 2.8.0\cr
Date: \tab 2014-10-06\cr
License: \tab GPL\cr
}
}
\author{
Responsable : Vladislav Navel <vladislav.navel@atmopc.org>
}
\keyword{ package }
