% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rosepol.R
\name{plot.rose}
\alias{plot.rose}
\title{Trace des roses de concentrations (moyennes, maximales, etc.)}
\usage{
\method{plot}{rose}(x, y, col, border, density = -1, rlim = NULL,
  at.rho = NULL, unite = "", labels.rho = NULL, col.rho, lwd.rho = 1,
  lty.rho = 2, expr.rho = FALSE, cex.rho = 1, theta.rho = sample(360,
  1), at.theta = 0:7 * 45, labels.theta = c("N", "NE", "E", "SE", "S", "SO",
  "O", "NO"), nb.theta = 360, col.theta, lwd.theta = 1, lty.theta = 2,
  centre = list(x = 0, y = 0), pas.rose = 1, facteur = 1, add = FALSE,
  ...)
}
\arguments{
\item{x}{liste contenant la structure \sQuote{rose} à représenter. Cette
structure doit être similaire à ce qui est retourné par la fonction
\code{\link{compute.rose}}.}

\item{y}{voir la section \sQuote{Details}.}

\item{col}{couleur de remplissage de la rose.}

\item{border}{couleur de la bordure de la rose.}

\item{density}{the density of shading lines, in lines per inch.  The default
    value of ‘NULL’ means that no shading lines are drawn.  A
    zero value of ‘density’ means no shading nor filling whereas
negative values (and ‘NA’) suppress shading (and so allow
    color filling).}

\item{rlim}{limites du graphiques. contient 2 valeurs, la première est la valeur
correspondant la concentration au centre de la rose ; la seconde à celle 
au bord extérieur de la rose.}

\item{at.rho}{valeurs de rayon auxquelles doivent être affichés des divisions
pour l'échelle des concentrations.}

\item{unite}{chaîne de caractères qui sera ajouté à chaque étiquette de l'échelle
des concentrations.}

\item{labels.rho}{chaînes de caractères ou expressions (cf \sQuote{Details})
indiquant les étiquettes de l'échelle de concentrations.}

\item{col.rho}{couleur à appliquer à l'échelle des concentrations (cf \sQuote{Details}).}

\item{lwd.rho}{épaisseur de l'échelle des concentrations (cf \sQuote{Details}).}

\item{lty.rho}{type de l'échelle des concentrations (pointillés, continu) (cf \sQuote{Details}).}

\item{expr.rho}{logical indiquant si labels.rho contient des chaînes de caractères
ou des expressions (cf \sQuote{Details}).}

\item{cex.rho}{character expansion pour la graduation}

\item{theta.rho}{angle en degré où affiché les graduations}

\item{at.theta}{indique pour quels degrés les étiquettes pour l'échelle
des directions de vent doivent être affichées.}

\item{labels.theta}{chaînes de caractères contenant les étiquettes pour l'échelle
des directions de vent.}

\item{nb.theta}{indique (en degré) la précision des cercles de l'échelle des concentrations.
Un pas de 1 signifie qu'un point sera tracé tous les degrés.}

\item{col.theta}{couleur à appliquer à l'échelle des directions de vents (cf \sQuote{Details}).}

\item{lwd.theta}{épaisseur de l'échelle des directions de vent (cf \sQuote{Details}).}

\item{lty.theta}{type de l'échelle des concentrations (pointillés, continu) (cf \sQuote{Details}).}

\item{centre}{lorsque la rose est ajoutée à un graphique, donne les coordonnées
dans le graphique pré-existant du centre de la rose. Liste avec deux élements
nommés \code{x} et \code{y}.}

\item{pas.rose}{valeur indiquant la précision de la rose (1 -> un point tous les
degrés, 0.5 -> 2 points par degré, etc).}

\item{facteur}{valeur numérique indiquant le grossissement à appliquer à la rose.
Utile uniquement lorsque la rose est ajoutée à un graphique pré-existant pour 
    régler la taille de la rose.}

\item{add}{logical indiquant si la rose doit être ajoutée à un graphique pré-existant.}

\item{\dots}{pour éventuellement d'autres méthodes.}
}
\description{
Cette fonction ne réalise aucun calcul à proprement parler, 
elle sert uniquement à représenter graphiquement des roses 
telles que calculées par la fonction \code{\link{compute.rose}}.
}
\details{
Lorsque la rose doit être d'abord calculée, il est possible d'utiliser
d'abord la fonction \code{\link{compute.rose}} ou directement
\code{\link{rosepol}}.

Cette fonction a été réalisée de manière à pouvoir tracer une rose
sur un graphique \sQuote{vierge} ou bien à l'ajouter sur un graphique
déjà existant. Pour ajouter la rose à un graphique existant, il suffit
de mettre l'argument \code{add} à \code{TRUE}.

Afin de contrôler au mieux l'affichage de la rose, les paramètres graphiques
peuvent être regroupés en 4 catégories. Ces catégories sont les paramètres liés :\cr
\describe{
\item{à la rose :}{x, y, col, border, density ;}
\item{à l'échelle des concentrations :}{tous les paramètres en *.rho et nb.theta ;}
\item{à l'échelle des directions de vent :}{tous les paramètres en *.theta sauf nb.theta ;}
\item{au graphique en général :}{rlim, centre, pas.rose, facteur, add.}
}

\code{x} est une liste de même format que la valeur retournée par \code{\link{compute.rose}}.
Éventuellement, l'élément \code{values} de \code{x} peut être une liste (de taille identique
au nombre de secteurs) de \code{\link[base]{data.frame}}. Dans ce cas, \code{y} indique
le nom ou la place de l'élément à prendre dans chaque élément de cette liste.

Les autres éléments relatifs à la rose (\code{col, border, density}) sont identiques
à ceux de la fonction \code{\link[graphics]{polygon}}.

Pour les paramètres de l'échelle des concentrations, \code{at.rho, 
labels.rho, lwd.rho et lty.rho} s'utilisent de manière identique 
à ceux de la fonction \code{\link[graphics]{par}}. \code{expr.rho}
permet indique si les étiquettes (\code{labels.rho}) sont des expressions.
En effet, il est possible de définir ces étiquettes avec des expressions
sur le modèle de la fonction \code{\link[grDevices]{plotmath}}. Ça permet
entre autre d'afficher des microg/m3 de manière plus officielle (avec le
véritable caractère grec et en mettant des exposants). \code{expr.rho}
doit alors juste être mis à \code{TRUE}.

Pour les paramètres de l'échelle des vents, \code{at.theta, 
labels.theta, col.theta, lwd.theta et lty.theta} s'utilisent de manière identique 
à ceux de la fonction \code{\link[graphics]{par}}.

Pour les autres paramètres, voir la description détaillée.
}
\seealso{
\code{\link{compute.rose}}, \code{\link{rosepol}}, \code{\link{plot.rosevent}}, \code{\link{rosevent}}
}

