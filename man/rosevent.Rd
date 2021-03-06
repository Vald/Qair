% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosevent.R
\name{rosevent}
\alias{rosevent}
\title{Calcul et affichage de roses des vents}
\usage{
rosevent(dv, fv, breaks = 0:17 * 22.5 + 22.5/2, vlims, col, border,
  density = -1, rlim = NULL, centre = list(x = 0, y = 0), pas.rose = 1,
  facteur = 1, add = FALSE, ...)
}
\arguments{
\item{dv}{vecteur \sQuote{numeric} contenant les directions du vent en degrés
(0 -> Nord, 90 -> Ouest, etc.).}

\item{fv}{vecteur \sQuote{numeric} contenant la vitesse du vent en m/s.
Les valeurs de \sQuote{values} pour lesquelles \sQuote{fv} est 0 ou NA
ne sont pas prises en compte.
(Attention : la prise en compte de ce paramètre risque d'évoluer à l'avenir).}

\item{breaks}{vecteur \sQuote{numeric} indiquant les limites entre chaque secteur
    de vents (en degrés). Si une valeur indiquée est en-dehors de [0-360], 
elle est prise en compte, mais modulo 360.

Les valeurs par défaut correspondent aux 16 points cardinaux (N, NNE, NE, 
ENE, E, ESE, SE, SSE, S, etc.).}

\item{vlims}{vecteurs contenant les limites des différentes classes de vitesses 
de vent. Voir la section \sQuote{Details} pour la prise en compte des vents
calmes.}

\item{col}{couleurs de remplissage de la rose. Une valeur est attendue
pour chaque classe de vitesses.}

\item{border}{couleurs de la bordure de la rose. Une couleur peut être
donnée pour chaque classe de vitesses (mais pas nécessairement).}

\item{density}{the density of shading lines, in lines per inch.  The default
    value of ‘NULL’ means that no shading lines are drawn.  A
    zero value of ‘density’ means no shading nor filling whereas
negative values (and ‘NA’) suppress shading (and so allow
    color filling). Une valeur par classe de vitesses, ou pas.}

\item{rlim}{limites du graphiques. contient 2 valeurs, la première est la valeur
correspondant la concentration au centre de la rose ; la seconde à celle 
au bord extérieur de la rose.}

\item{centre}{lorsque la rose est ajoutée à un graphique, donne les coordonnées
dans le graphique pré-existant du centre de la rose. Liste avec deux élements
nommés \code{x} et \code{y}.}

\item{pas.rose}{valeur indiquant la précision de la rose (1 -> un point tous les
degrés, 0.5 -> 2 points par degré, etc).}

\item{facteur}{valeur numérique indiquant le grossissement à appliquer à la rose.
Utile uniquement lorsque la rose est ajoutée à un graphique pré-existant pour 
    régler la taille de la rose.}

\item{add}{logical indiquant si la rose doit être ajoutée à un graphique pré-existant.}

\item{...}{arguments supplémentaires transmis à la fonction \code{\link{plot.rose}}.}

\item{\dots}{arguments supplémentaires pour la fonction \code{\link{plot.rosevent}}.}
}
\value{
une liste invisible de listes structurées de manière identique à ce qui
	est obtenu en utilisant la fonction \code{\link{compute.rose}} ainsi
	qu'un élément supplémentaire : \code{vlim}.
	Chaque élément de la liste contient les éléments suivants :
\item{breaks}{un vecteur \sQuote{numeric} contenant les limites des secteurs de vents.
	Voir la section\sQuote{Details} pour plus de précisions.}
\item{values}{Vecteur \sQuote{numeric} de longueur égale au nombre de secteurs de vent
	 définis.}
\item{opened}{Un facteur indiquant si les secteurs de vent sont ouverts à gauche ou à droite
	(pour l'instant, seuls les secteurs ouverts à gauche sont pris en compte).}
\item{vlim}{Valeur numérique correspondant à la vitesse de vent à laquelle correspond
	l'élément en question (\code{0} correspond aux vents nuls et calmes).}
}
\description{
Cette fonction est un raccourci pour utiliser simultanément les fonctions
\code{\link{compute.rosevent}} puis \code{\link{plot.rosevent}}.
}
\seealso{
\code{\link{compute.rosevent}}, \code{\link{plot.rosevent}}
}

