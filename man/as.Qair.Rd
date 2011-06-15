\name{as.Qair}
\alias{as.Qair}
\alias{Qair}
\alias{as.Qair.data.frame}
\title{ Transforme des objets de R en objets de type 'Qair' }
\description{
  Description du format 'Qair' ainsi que de l'utilisation de la fonction 'as.Qair'.
}
\usage{
as.Qair (x, dt = NULL, debut = NULL, ... )
\method{as.Qair}{data.frame} (x, dt = NULL, debut = NULL,
  stations = NULL, mesures = NULL, unites = NULL,
  longitudes = NULL, latitudes = NULL,
  lambertx = NULL, lamberty = NULL, ... )
}
\arguments{
  \item{x}{ une data.frame contenant les donnees a transformer au format Qair. Peut ou non contenir une colonne 'date'. Si cette
  colonne n'existe pas, les arguments dt et debut sont obligatoires.  }
  \item{dt}{ Le type temporel des donnees a convertir. Peut prendre les valeurs 'qh', 'heure', 'jour', 'hebdo', 'mois', 'an', 
  'pjour', 'phebdo', 'pmois', 'pannuel'. S'il est NULL, la classe de l'objet final sera uniquement c("Qair", "data.frame") (voir section Qair).  }
  \item{debut}{ Pour un dt c('qh', 'heure', 'jour', 'hebdo', 'mois', 'an'), 'debut' est une chaine de caracteres au format 'yy-mm-jj hh:mm:ss'.
  Dans les autres cas, debut est un nombre.  }
  \item{stations}{ vecteur de chaines de caracteres decrivant les stations auxquelles sont faites les mesures. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si stations est NULL, l'attribut
  correspondant est un vecteur de NA.  }
  \item{mesures}{ vecteur de chaines de caracteres decrivant les mesures contenues dans les colonnes de 'x'. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si mesures est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{unites}{ vecteur de chaines de caracteres decrivant les unites des mesures contenues dans 'x'. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si unites est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{longitudes}{ vecteur de chaines de caracteres indiquant la longitude des mesures. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si longitudes est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{latitudes}{ vecteur de chaines de caracteres indiquant la latitude des mesures. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si longitudes est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{lambertx}{ vecteur de chaines de caracteres indiquant la cordonnee 'x' des mesures en lambert II etendu. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si lambertx est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{lamberty}{ vecteur de chaines de caracteres indiquant la cordonnee 'y' des mesures en lambert II etendu. Doit avoir la meme 
  longueur que le nombre de colonnes de 'x', hors la colonne 'date'. Si ce n'est pas le cas ou si lamberty est NULL, l'attribut
  correspondant est un vecteur de NA. }
  \item{\dots}{ pour l'utilisation des methodes. }
}
\value{
  La fonction 'as.Qair' renvoie une structure 'Qair'.
  Si la colonne 'date' existe initialement dans 'x' et que 'dt' n'est pas precise, la classe de la structure est c("Qair", "data.frame").
  Si la colonne 'date' n'est pas initialement dans 'x', 'dt' doit etre precise. La classe de la structure est alors
  c(dt, "Qair", "data.frame").
  
  La structure renvoyee possede les 'attributes' stations, mesures, unites, longitudes, latitudes, lambertx, lamberty. Ces attributs sont
  eventuellement a NA s'ils ne sont pas specifies.
}
\section{Qair}{  
  Une structure 'Qair' est une data.frame avec un type particuler et pour lequel un certain nombre de fonctions son disponibles.
  Cette structure est definie en vue de manipuler des donnees temporelles regulieres.
  La data.frame d'une structure Qair doit obligatoirement avoir pour premiere colonne une colonne intitulee 'date'. Cette colonne sert
  a indiquer les dates auxquelles correspondent chaque ligne de la data.frame. Cette colonne est ordonnee et l'espacement temporel entre 2
  dates doit etre regulier. Ainsi, l'ecart entre deux dates de lignes adjacentes est toujours le meme dans une meme structure.
  
  Cet ecart peut-etre quart-horaire (qh), horaire (heure), journalier (jour), hebdomadaire (hebdo), mensuel (mois), annuel (an).
  Il peut egalement s'agir de profils journalier (pjour), hebodamaires (phebdo), mensuel (pmois) ou annuel (pannuel).
  Le type d'intervalle temporel d'une structure 'Qair' est identifie via la classe de la structure qui est c(dt, "Qair", "data.frame"), ou dt
  correspond aux intervalles pre-cites.
  Des fonctions types ont ete definies pour les structures 'Qair' permettant une representation graphique directe :
  \code{\link{plot.Qair}}, \code{\link{rosevent}}, \code{\link{rosepol}}, \code{\link{calcRose}}, \code{\link{is.na.Qair}}.
  Chacune de ces fonctions s'adapte au type temporel de la structure.
  
  Il existe egalement des fonctions permettant de faciliter la manipulation des structures 'Qair' (affectation, ajout de donnees,
  concatenation, acces aux 'attributes') :
  \code{\link{[.Qair}}, \code{\link{merge.Qair}}, \code{\link{mesures}}.
  
  Des fonctions de conversion d'un type temporel a l'autre sont egalement disponible : \code{\link{toAn}}.
  Les conversions sont possibles uniquement vers agregation temporelle plus importante. Ainsi, il est possible d'agreger des donnees
  quart-horaires en donnees annuelles, mais l'inverse n'est pas possible.
  
  Il est possible que la classe d'une structure 'Qair' soit uniquement c('Qair', 'data.frame') : le type temporel des donnees n'etant alors
  pas connu. Dans ce cas, la plupart des fonctions de manipulations de la structure fonctionnent. En revanche, les fonctions de
  conversion ne sont pas applicables.
}

\author{ Vladislav Navel }

\examples{
# exemple d'une structure 'Qair'
data(larochelle)
head(larochelle)
class(larochelle)
names(attributes(larochelle))
length(larochelle)
sapply(attributes(larochelle), length)
mesures(larochelle)

# transformation de 'larochelle' en data.frame pour la retransformer en 'Qair'
larochelle <- as.data.frame(larochelle)
class(larochelle)
larochelle <- as.Qair(larochelle, dt='qh')
class(larochelle)
names(attributes(larochelle))
length(larochelle)
sapply(attributes(larochelle), length)
# les attributs initiaux sont rattaches a la data.frame mais non rentres en argument.
# Ils ne sont donc pas pris en compte et la valeur NA est affectee a tous.
mesures(larochelle)
}
\keyword{ classes }
\keyword{ data }
\keyword{ manip }
