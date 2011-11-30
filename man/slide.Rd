\name{slide}
\alias{slide}
\title{Aggrégation 'glissante' de données.}
\usage{
  slide(x, fenetre, pas, pc = 0.75,
    FUN = c("mean", "min", "max", "var"))
}
\arguments{
  \item{x}{un vecteur de numeric sur lequel doit être
  appliqué la fonction}

  \item{fenetre}{taille de la fenetre d'application de la
  fonction}

  \item{pas}{pas auquel doit être appliqué la fonction}

  \item{pc}{pourcentage minimal de données dispo dans une
  fenêtre pour appliquer la fonction. Renvoie NA si ce
  minimum n'est pas atteint.}

  \item{FUN}{fonction à appliquer. Seules 'mean', 'min',
  'max' et 'var' sont autorisées.}
}
\value{
  un vecteur dont la longueur est déterminée par la
  longueur du vecteur initial et la pas d'application.
}
\description{
  Cette fonction permet de calculer des moyennes, des
  variances, un maximum ou un minimum en faisant 'glisser'
  une fenêtre sur un jeu de données.
}
\details{
  Avec un pas de 1, le vecteur de retour sera de la même
  longueur que le vecteur initial.

  symboliquement chaque élément du résultat peut s'écrire
  sous la forme (avec i allant de \code{1} à
  \code{ceiling(length(x)/pas)} ) :

  \code{y[i] = FUN(x[index[index > 0]])}

  avec \code{index = pas . i + 1 - (fenetre:1)}

  L'intérêt principal de cette fonction est d'optimiser le
  calcul glissant qui, réalisé sous forme classique 'R' est
  extrêmement lent (dans cette fonction le calcul est
  directement réalisé en 'C').
}
\examples{
slide (1:10, 2, 1, 0)
slide (1:10, 2, 4, 0)
}

