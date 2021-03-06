% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrGetQuery.R
\name{xrGetQuery}
\alias{xrGetQuery}
\title{Recuperation du resultat d'une requete dans une base XR.}
\usage{
xrGetQuery(conn, query)
}
\arguments{
\item{conn}{une connexion à une base XR (cf \code{\link{xrConnect}})}

\item{query}{chaîne de caractères contenant la reqûete}
}
\value{
une data.frame avec le résultat de la requête ou 
	une erreur si la requête n'a pas abouti.
}
\description{
Recuperation du resultat d'une requete dans une base XR.
}
\section{Details}{
 Cette fonction permet d'envoyer une requête à une connexion
sur une base XR et d'en récupérer le résultat.
Sous windows, le pilote utilisé est ODBC (nécessite d'avoir
installer le paquet \code{\link[RODBC]{RODBC}}), sous linux JDBC
(nécessite d'avoir installer le paquet \code{\link[RJDBC]{JDBC}}).
}
\seealso{
\code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}}, \code{\link[RJDBC]{JDBC}}
}

