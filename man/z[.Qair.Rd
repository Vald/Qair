\name{[.Qair}
\alias{[.Qair}
\alias{[<-.Qair}
\alias{$<-.Qair}
\alias{[.rose}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract or replace part of 'Qair', extract part of 'rose' }
\description{
  Permet d'acceder a des sous-groupes d'un objet 'Qair' ou 'rose, et de les remplacer dans le cas des objets 'Qair'.
}
\usage{
\method{[}{Qair}(x, i, j, drop)
\method{[}{Qair}(x, i, j) <- value
\method{$}{Qair}(x, name) <- value

\method{[}{rose}(x, i, j, drop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ objet de classe 'Qair' ou de classe 'rose' }
  \item{i}{ voir details }
  \item{j}{ voir details }
  \item{drop}{ Uniquement pour la compatibilite des methodes, ne pas utiliser. }
  \item{name}{ voir details }
  \item{value}{ voir details }
}
\details{
  Pour Les objets de classe 'Qair', les methodes d'extractions fontcionnent comme pour les data.frame. (cf. \code{\link{[.data.frame}})  
  Pour les objets de classe 'rose', la methode '[' permet d'acceder aux elements de objets 'Qair' qu'elle copntient. Appliquer '[' a un  
  rose revient a l'appliquer a chacun des secteurs qui la compose.  

  Une posssibilite d'extraction suppplementaire a ete ajoutee : elle permet de faire une selection par date. Ainsi il est possible d'indiquer  
  une date de  debut en donnant la valeur a 'i' et une date de  fin en donnant la valeur a j.  
  Les dates sont indiquees sous le format suivant "y-m-d" s'il la date est en jour, c("y-m-d", "h:m:s") pour prendre en compte les heures.  

  La fonction d'affection '<-' basee sur une selection par '[', fonctionne comme s'il n'y avait pas de colonne "date" dans la structure Qair.  
  La raison en est que cette colonne ne doit pas etre modifiee, et ne peut donc pas l'etre.  

  POUR LA METHODE '$<-', 'value' peut etre un vecteur de valeurs a ajouter dans la colonne specifiee par 'name'. Si 'name' n'est pas un nom de  
  colonne deja existant, une colonne sera creee. Si 'value' n'a pas la meme longueur que le nombre de lignes de x, les elements de 'value'  
  sont recycles.
  toujours pour la METHODE '$<-', value peut etre une list de la forme suivante :  
  list(newVar=..., unite=..., mesure=..., station=...)  
  Les noms des elements de la liste sont obligatoires, 'newVar' prend un role identique a celui de 'value' presente precedemment. Les trois  
  autres elements de la liste sont optionnels et modifieront les attributs en consequence. (si une colonne est ajoutee, la valeur par defaut  
  des trois attributs est NA.)  

  POUR LA METHODE '[<-', 'value' peut etre un vecteur une matrice ou une data.frame. Cette methode n'est utilisable QUE pour faire des  
  remplacements de valeurs. (aucune creation de colonne n'est possible via cette syntaxe.)  

}
\value{
  Un objet dans la meme classe que celui a qui est applique la selection.
  La colonne 'date' fait systematiquement partie de l'element extrait.
}
\seealso{ \code{\link{merge.Qair}}, \code{\link{[.data.frame}} }
\examples{
  data(larochelle)
  lundi <- larochelle
  lundi[weekdays(lundi)!="Mon"] <- NA
  plot(lundi)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
