\name{compute.lim}
\alias{compute.lim}
\title{Calcule un interval de representation.}
\usage{
  compute.lim(x, na.rm = FALSE)
}
\arguments{
  \item{x}{vecteur de 'numeric' pour lequel on souhaite
  déterminer les limites grpahiques}

  \item{na.rm}{boolean les valeurs NA doivent être elles
  considérées ?}
}
\value{
  vecteur de 2 'numeric's
}
\description{
  À partir d'un vecteur de valeurs numérique, la fonction
  renvoie un vecteur de 2 valeurs pouvant servir de limites
  pour un graphique (xlim, ylim, rlim, etc.)
}

