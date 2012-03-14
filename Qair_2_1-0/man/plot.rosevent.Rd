\name{plot.rosevent}
\alias{plot.rosevent}
\title{Trace des roses des vents}
\usage{
  plot.rosevent(x, y, col, border, density = -1,
    rlim = NULL, centre = list(x = 0, y = 0), pas.rose = 1,
    facteur = 1, add = FALSE, ...)
}
\arguments{
  \item{x}{liste contenant la structure \sQuote{rose} à
  représenter. Cette structure doit être similaire à ce qui
  est retourné par la fonction
  \code{\link{compute.rosevent}}.}

  \item{y}{argument conservé uniquement pour que la
  fonction reste cohérente avec la fonction générique
  \sQuote{plot}.}

  \item{col}{couleurs de remplissage de la rose. Une valeur
  est attendue pour chaque classe de vitesses.}

  \item{border}{couleurs de la bordure de la rose. Une
  couleur peut être donnée pour chaque classe de vitesses
  (mais pas nécessairement).}

  \item{density}{the density of shading lines, in lines per
  inch.  The default value of ‘NULL’ means that no shading
  lines are drawn.  A zero value of ‘density’ means no
  shading nor filling whereas negative values (and ‘NA’)
  suppress shading (and so allow color filling). Une valeur
  par classe de vitesses, ou pas.}

  \item{rlim}{limites du graphiques. contient 2 valeurs, la
  première est la valeur correspondant la concentration au
  centre de la rose ; la seconde à celle au bord extérieur
  de la rose.}

  \item{centre}{lorsque la rose est ajoutée à un graphique,
  donne les coordonnées dans le graphique pré-existant du
  centre de la rose. Liste avec deux élements nommés
  \code{x} et \code{y}.}

  \item{pas.rose}{valeur indiquant la précision de la rose
  (1 -> un point tous les degrés, 0.5 -> 2 points par
  degré, etc).}

  \item{facteur}{valeur numérique indiquant le
  grossissement à appliquer à la rose.  Utile uniquement
  lorsque la rose est ajoutée à un graphique pré-existant
  pour régler la taille de la rose.}

  \item{add}{logical indiquant si la rose doit être ajoutée
  à un graphique pré-existant.}

  \item{\dots}{arguments supplémentaires transmis à la
  fonction \code{\link{plot.rose}}.}
}
\description{
  Cette fonction ne réalise aucun calcul à proprement
  parler, elle sert uniquement à représenter graphiquement
  des roses telles que calculées par la fonction
  \code{\link{compute.rosevent}}.
}
\details{
  Lorsque la rose doit être d'abord calculée, il est
  possible d'utiliser d'abord la fonction
  \code{\link{compute.rosevent}} ou directement
  \code{\link{rosevent}}.

  Cette fonction a été réalisée de manière à pouvoir tracer
  une rose sur un graphique \sQuote{vierge} ou bien à
  l'ajouter sur un graphique déjà existant. Pour ajouter la
  rose à un graphique existant, il suffit de mettre
  l'argument \code{add} à \code{TRUE}.

  L'insertion de la rose sur la graphique peut être ajustée
  à l'aide des paramètres \code{centre} et \code{facteur}.
}
\seealso{
  \code{\link{compute.rose}}, \code{\link{rosepol}},
  \code{\link{plot.rosevent}}, \code{\link{rosevent}}
}

