\name{seuils}
\alias{seuils}
\title{Seuils reglementaires pour la qualite de l'air}
\usage{
  seuils(type = c("objectif de qualité", "seuil d'information et de recommandation", "seuil d'alerte", "valeur limite", "niveau critique", "valeur cible", "seuil d'évaluation inférieur", "seuil d'évaluation supérieur"),
    protection = c("la santé humaine", "la végétation", "la mise en oeuvre progressive de mesures d'urgence"),
    polluant = c("03", "12", "24", "39", "19", "AA", "01", "08", "04", "V4", "80", "AH", "82", "AJ", "87", "AC", "P6"),
    cchim = c("NO2", "NOx", "PM10", "PM2.5", "Plomb", "SO2", "O3", "CO", "C6H6", "Arsenic", "Cadmium", "Nickel", "B(a)P"),
    sites = typologies)
}
\arguments{
  \item{type}{type du seuil recherché : "objectif de
  qualité", "seuil d'information et de recommandation",
  "seuil d'alerte", "valeur limite", "niveau critique",
  "valeur cible", "seuil d'évaluation inférieur" ou "seuil
  d'évaluation supérieur".}

  \item{protection}{domaine pour lequel est défini le seuil
  : "la santé humaine", "la végétation" ou "la mise en
  oeuvre progressive de mesures d'urgence".}

  \item{polluant}{code ISO du polluant auquel s'applique le
  seuil (tel que défini dans les bases XR ou Pol'air).
  ("03", "12", "24", "39", "19", "AA", "01", "08", "04",
  "V4", "80", "AH", "82", "AJ", "87", "AC", "P6")}

  \item{cchim}{code plus \sQuote{compréhensible} pour
  chaque polluant.  ("NO2", "NOx", "PM10", "PM2.5",
  "Plomb", "SO2", "O3", "CO", "C6H6", "Arsenic", "Cadmium",
  "Nickel", "B(a)P")}

  \item{sites}{typologie des sites auxquels le seuil est
  applicable (cf \code{\link{typologies}}).}
}
\value{
  l'objet retourné est une liste constituée de tous les
  seuils qui correspondent aux paramètres entrés. Un seuil
  est en réalité une liste (la classe \sQuote{seuil} sert
  principalement à permettre un affichage plus lisible de
  chaque seuil) contenant les éléments suivants :
  \item{polluant}{code ISO du polluant} \item{cchim}{code
  chimique du polluant} \item{type}{type de seuil (OQ, VL,
  etc.)} \item{protection}{domaine ciblé par le seuil}
  \item{sites}{typologie des sites où le seuil est
  applicable} \item{base.calcul}{Pour certains calcul, les
  données doivent avant tout être préparées d'une certaine
  manière. Il peut s'agir de les mettre au format horaire
  (ce qui est le cas le plus courant). Si l'opération à
  réaliser est une simple conversion temporelle,
  \code{base.calcul} peut être une chaîne de caractères
  utilisable comme argument de \code{\link{new_period}}
  ('hour' par exemple) ou directement un objet
  \code{\link{period}}.  Si l'opération de préparation est
  plus complexe, \sQuote{base.calcul} peut être directement
  une fonction à appliquer aux données. NULL si aucun
  préparation n'est nécessaire.} \item{rep.b.calcul}{Dans
  le cas où \code{base.calcul} est un \code{\link{period}}
  (ou une chaîne de caractères), \sQuote{rep.b.calcul} est
  la représentativité minimale en-dessous de laquelle la
  conversion temporelle donnera NA.}
  \item{base.comparaison}{Dans certains cas, les mesures ne
  sont par directement comparées au seuil : elles doivent
  être préalablement transformées. Dans le cas où la
  transformation est uniquement temporelle (concentration
  moyenne annuelle par exemple), \code{base.comparaison}
  est une chaîne de caractères utilisable comme argument de
  \code{\link{new_period}} ('hour' par exemple) ou
  directement un objet \code{\link{period}}.  Pour des
  transformations plus complexes (calcul d'AOT40),
  \code{base.comparaison} peut être directement une
  fonction. NULL si aucune transformation n'est requise.}.
  \item{rep.b.comparaison}{Dans le cas où
  \code{base.comparaison} correspond à une transformation
  temporelle, \code{base.rep.comparaison} est la
  représentativité minimale en-dessous de laquelle la
  conversion temporelle donnera NA.}
  \item{precision}{Entier indiquant la précision à laquelle
  la comparaison doit être effectuée.} \item{seuil}{Valeur
  numérique à laquelle sont comparées les mesures ou les
  mesures transformées pour déterminer s'il y a
  dépassement.} \item{unite}{Chaîne de caractères
  symbolisant dans quelle unité doivent être exprimées les
  concentrations du polluant concerné par le seuil.}
  \item{reference}{Chaîne de caractères précisant de quel
  texte réglementaire est issu le seuil.}
  \item{comparaison}{Soit NULL si les données préparées
  avec \code{base.comparaison} ont juste à être comparées
  au seuil ; un objet \code{\link{period}} (ou chaîne de
  caractères correspondante) si, une fois comparées, les
  données préparées doivent être agrégées sur cette période
  (nombre limite de dépassements journaliers sur une année,
  etc.) ; directement une fonction dans les cas plus
  complexes (notamment en cas d'existance de marges de
  dépassement).} \item{rep.comparaison}{Dans le cas où
  \code{comparaison} correspond à une transformation
  temporelle, \code{rep.comparaison} est la
  représentativité minimale en-dessous de laquelle la
  conversion temporelle donnera NA.} \item{nb.max}{Dans les
  cas où le seuil est un nombre à ne pas dépasser sur une
  période, le nombre en question.}
  \item{applicable}{\code{\link{POSIXct}} donnant la date
  d'application du seuil, ou NULL.}
  \item{description}{L'affichage d'un seuil est normalement
  déduit automatiquement de ses caractéristiques
  (\code{\link{format.seuil}}). Cependant dans certains
  cas, cette déduction n'est pas possible. Il est alors
  possible de préciser la chaîne représentation le seuil
  ici.}

  Le détail fourni ci-dessus doit permettre de définir
  d'autres seuils utilisable avec
  \code{\link{validation.reglementaire}} sans pour autant
  que ces seuils soient \sQuote{réglementaires}.
}
\description{
  Cette fonction permet d'accéder à l'ensemble des seuils
  réglementaires applicables en France pour la surveillance
  de la qualité de l'air.
}
\details{
  Les seuils concernés sont les objectifs de qualité, les
  valeurs limtes, les valeurs cibles et niveaux critiques
  définis pour la protection de la santé humaine et de la
  végétation. Les seuils d'évaluation supérieur et
  inférieur sont également définis, ainsi que les seuils
  d'information et de recommandation et les seuils
  d'alertes.

  L'ensemble des polluants concernés par ces seuils sont
  pris en compte : NO2, NOx, PM10, PM2.5, plomb, SO2, O3,
  CO, C6H6, arsenic, cadmium, nickel et benzo(a)pyrène.

  Les modalités d'application des d'alerte dépendant
  d'arrêtés régionaux, ils sont donnés ici à titre
  indicatif.

  Pour récupérer un ou plusieurs seuil(s), il suffit de
  préciser les paramètres souhaités. La structure d'un
  \sQuote{seuil} n'est pas très lisible en soit car elle
  permet de réaliser la comparaison d'une mesure au seuil à
  l'aide des fonctions \code{\link{preparation.base}},
  \code{\link{comparaison}}
  \code{\link{validation.reglementaire}} (pour plus de
  détails, se reporter aux-dites fonctions).
}
\references{
  Décret 2010-1250 du 21 octobre 2010, Directive 2008/50/CE
  du 21 mai 2008, Directive 2004/107/CE du 15 décembre 2004
}
\seealso{
  \code{\link{typologies}}
  \code{\link{validation.reglementaire}},
  \code{\link{comparaison}},
  \code{\link{preparation.base}},
  \code{\link{format.seuil}}
}

