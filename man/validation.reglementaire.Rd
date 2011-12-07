\name{validation.reglementaire}
\alias{validation.reglementaire}
\title{Applique strictement des seuils reglementaires a des valeurs donnees}
\usage{
  validation.reglementaire(x, seuil,
    etapes = c("preparation.calcul", "preparation.comparaison", "comparaison"),
    resultat = c("detail", "comparaison"),
    check.720 = TRUE, ...)
}
\arguments{
  \item{etapes}{indique si les données doivent d'abord être
  préparées avant d'appliquer le seuil, et quelles
  préparations doivent être réalisées (\sQuote{calcul}
  et/ou \sQuote{comparaion}).  Voir la section
  \sQuote{Details} pour plus d'infos.  Si la comparaison
  doit être réalisée, cette info doit également être
  indiquée.}

  \item{resultat}{dans le cas où la comparaison est
  demandée, indique si le résultat doit contenir le détail
  des calculs, juste le bilan, ou les deux (voir
  \sQuote{Value}).}

  \item{\dots}{tout argument pouvant être transmis aux
  autres fonctions \sQuote{use.marges} par exemple (cf
  \code{\link{comparaison}} et
  \code{\link{preparation.base}}).

  Il est également possible de mettre en argument l'un des
  attributs du seuil (cf \code{\link{seuils}}) pour
  remplacer sa valeur par défaut (par exemple, dans le
  calcul de l'AOT, il est possible de remplacer le seuil de
  6000 par 8000 et ajoutant \code{seuil=8000} en
  argument).}

  \item{x}{jeu de données
  (\code{\link[timetools]{TimeIntervalDataFrame-class}})
  contenant les données à mettre en forme}

  \item{seuil}{seuil (cf \code{\link{seuis}}) pour lequel
  les données doivent être mise en forme}

  \item{check.720}{booléen indiquant si, lorsque cela est
  pertinent, la règle des \sQuote{720 heures consécutives}
  doit être vérifiée ou non (cette règle apparaît dans un
  guide \sQuote{ADEME} mais dans aucun texte
  réglementaire).}
}
\value{
  un objet de classe
  \code{\link[timetools:TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
  qui contient soit les données préparées (en fonction de
  ce qui a été demandé) soit le résultat de la comparaison.
  Si le détail est demandé la valeur (si le seuil consiste
  simplement en une valeur à ne pas dépasser) ou le nombre
  de dépassement du seuil (s'il le seuil ne doit pas être
  dépassé plus d'un nombre de fois donné) est retourné.
  TRUE ou FALSE si le détail n'est pas demandé. Renvoi les
  deux si c'est demandé.
}
\description{
  À partir d'un jeu de donnée et d'un seuil réglementaire
  (cf code{\link{seuils}}, cette fonction permet de tester
  si les données respectent le seuil. Les différents
  arguments de la fonction permettent de contrôler les
  différentes étapes.
}
\details{
  La fonction \code{validation.reglementaire} permet de
  vérifier qu'un jeu de données respecte un critère
  réglementaire donné. Cette vérification est faite par un
  appel successif aux fonctions \code{preparation.base}
  (avec l'argument \code{base} = \code{'calcul'} puis
  \code{'comparaison'}) et \code{comparaison}. Ainsi cette
  fonction permet de préparer et d'assurer qu'un je de
  données est correctement formaté avant de le comparer à
  un critère réglementaire.

  Si l'utilisateur sait que ses données sont correctes, ou
  si elles sont incorrectes (pas la bonne couverture
  temporelle par exemple), mais qu'il souhaite quand même
  comparer ses données au seuil, il peut désactiver les
  étapes de préparation en précisant l'argument :\cr
  \code{etapes='comparaison'}\cr ou n'importe quelle
  combinaison des 3 valeurs possibles.

  Note: quel que soit dans lequel sont indiqués les étapes
  souhaitées, ces dernières sont toujours effectuées dans
  l'ordre suivante : \enumerate{ \item preparation.calcul ;
  \item preparation.comparaison ; \item
  preparation.comparaison ; }

  Pour une utilisation plus avancée de la fonction, se
  reporter aux différentes sections suivantes.
}
\section{Introduction}{
  La comparaison de données à des seuils réglementaire doit
  être appliquée de manière rigoureuse afin d'assurer la
  fiabilité des résultats. D'autre part, afin de facilité
  le maintien des calculs qui réalisent la comparaison, il
  est intéressant d'uniformiser au maximum la procédure de
  calcul. Ainsi, en cas de modification du mécanisme, tous
  les calculs peuvent bénificier de la modification.

  Pour l'utilisateur final, le fait que la procédure est ou
  non uniformisée est normalement transparent. Les détails
  donnés ici s'adresse donc principalement à ceux qui
  souhaitent aller plus loin dans la compréhension de
  l'implémentation des calculs.
}

\section{Processus de comparaison}{
  Pour comprendre comment a été structuré la procédure de
  calcul il est nécessaire de \sQuote{modéliser} cette
  procédure, en quoi (quelles étapes) consiste exactement
  \sQuote{comparer des données à un seuil réglementaire}.

  Tout d'abord \sQuote{comparer à un seuil} n'est pas la
  terminologie la plus appropriée : il s'agit en réalité de
  savoir si un niveau de pollution respecte ou non certains
  critères. Chaque critère doit alors répondre à un certain
  nombre de questions pour pouvoir être défini : \itemize{
  \item Quel \sQuote{niveau} doit être comparé. Une
  concentration, moyenne annuelle, des concentrations
  horaires, journalières, quelques choses de plus complexe
  (AOT40, valeur journalière maximale des moyennes 8
  heures, etc). \item Quelle valeur ne doit pas être
  dépassée ? \item Combien de fois cette valeur ne
  doit-elle pas être dépassée ? \item Sur combien de temps
  ne doit-elle pas être dépassée ?  (Nombre de moyennes
  journalières ne devant pas dépasser un seuil sur 1 an,
  moyenne horaire à ne pas dépasser sur 3 heures
  consécutives, etc.). \item Si la valeur à comparer est
  une valeur agrégée (moyenne annuelle, journlaière), à
  partir de quelles données de base doit-elle être calculée
  ? (ici, soit la donnée de base est horaire, soit elle
  n'est pas définie) }

  Au travers de ces questions, il ressort que jusqu'à trois
  étapes peuvent être nécessaires dans la vérification du
  respect de critères réglementaires : \itemize{ \item
  préparation/vérification de la donnée de base pour le
  calcul ( les données sont-elles bien horaires, sinon,
  peuvent-elles être transformées en horaire ?) ; \item
  préparation/vérification de la donnée de base pour la
  comparaison (une fois en horaire, la donnée doit-elle
  être convertie en tri-horaire, journalière, autre ? ) ;
  \item vérification du respect du critère réglementaire. }
  Ces trois étapes sont détaillées par la suite.
}

\section{Préparation de la donnée de base}{
  Comme indiqué précédemment, la préparation de la donnée
  de base est requise uniquement lorsque la donnée à
  comparer doit être calculée sur une base spécifique (base
  horaire par exemple le plus souvent).

  Dans ce cas, les informations nécessaires sont :
  \itemize{ \item la base temporelle (horaire le plus
  souvent) ou une méthode permettant d'obtenir cette base ;
  \item le pourcentage de représentativité minimal requis
  pour qu'une données soit calculée dans le cas d'une base
  temporelle. } Dans un seuil (cf \code{\link{seuils}}),
  ces informations sont stockées dans \code{base.calcul} et
  \code{rep.b.calcul}. Dans le cas où il ne s'agit pas
  d'une base temporelle mais d'une méthode d'obtention,
  \code{base.calcul} est alors une fonction prenant comme
  argument \code{x} (les données à transformer) et
  \code{seuil} (le seuil en question) et éventuellement
  d'autres arguments récupérables au moyen des \code{\dots}
  de la fonction \code{validation.reglementaire}.

  Si cette étape n'est pas requise, les deux éléments
  indiqués ne sont pas renseignés pour le seuil.

  La donnée obtenue peut alors être soumise aux
  fonctionnalités de préparation de la comparaison.

  Cette étape est réalisée au moyen de la fonction
  \code{\link{preparation.base}} avec l'argument \code{base
  = 'calcul'}.
}

\section{Préparation de la donnée de comparaison}{
  La préparation de la donnée avant comparaison est une
  opération plus souvent requise et parfois plus complexe
  que la préparation de la donnée de base de calcul.

  Son fonctionnement est cependant identique : \itemize{
  \item soit la donnée doit être agrégée sur une base
  temporelle \item spécifique avant d'être comparée
  (moyenne annuelle, moyenne journalière, etc), auquel cas
  \code{base.comparaison} fournit cette base (\code{'day'},
  \code{year}) et \code{rep.b.comparaison} indique la
  représentativité minimale pour préparer la donnée ; \item
  soit la donnée doit être agrégée de façon plus complexe (
  AOT40, AOT40 en moyenne sur 5 ans, valeur maximale
  journalière de la valeur moyenne sur 8 heures, etc),
  auquel cas \code{base.comparaison} est une fonction qui
  permet de réaliser cette agrégation avec comme argument
  \code{x} (les données à transformer) et \code{seuil} (le
  seuil en question) et éventuellement d'autres arguments
  récupérables au moyen des \code{\dots} de la fonction
  \code{validation.reglementaire}. }

  Quelque soit le cas, la donnée obtenue doit pouvoir être
  directement comparée au seuil (à l'exception de quelques
  rares cas : cf section \sQuote{Vérification du respect du
  critère}).

  Cette étape est réalisée au moyen de la fonction
  \code{\link{preparation.base}} avec l'argument \code{base
  = 'comparaison'}.
}

\section{Vérification du respect du critère}{
  Une fois que les données sont prêtes, normalement il n'y
  a plus qu'à comparer avec le seuil du critère. Cependant,
  lorsqu'un seuil est \sQuote{à ne pas dépasser plus de x
  fois sur une période}, l'information permettant de savoir
  si le critère est respecté est le nombre de dépassement
  du seuil. Dans ce cas, le seuil possède un élément nommé
  \code{comparaison} qui indique de quelle période il
  s'agit (\code{'year'} le plus souvent, mais peut être
  \code{new_period(hour=3)} pour des seuils d'alerte).

  Dans ce cas, le seuil possède deux autres éléments. Le
  premier, \code{rep.comparaison}, indique la
  représentativité minimale de la période qui doit être
  couverte pour que la comparaison soit validée. Le second
  \code{nb.max} indique le nombre de dépassements maximal
  autorisés.

  Dans le cas où il existe des marges de dépassement,
  autrement dit : quand le seuil varie en fonction du
  temps, \code{comparaison} est une fonction qui réalise la
  comparaison. Cette fonction possède au minimum les
  arguments suivants : \describe{ \item{x}{données à
  comparer au seuil} \item{seuil}{le seuil lui-même}
  \item{detail}{booléen indiquant si la fonction doit
  retournée le détail ou juste si le critère est respecté
  (prend la valeur indiquée à l'appel de la fonction
  \code{\link{comparaison}})} \item{use.marges}{booléen
  indiquant si le calcul doit être réalisé avec la valeur
  applicable augmentée de la marge de dépassement}
  \item{get.marges}{dans le cas d'un appel direct à la
  fonction, retourne une data.frame contenant l'évolution
  de la marge. L'utilisation se fait de la sorte :\cr
  \code{seuils()[[61]]$comparaison(get.marges=TRUE)}. }
  \item{\dots}{Éventuellement d'autres arguments} }

  Si le détail est demandé la valeur (si le seuil consiste
  simplement en une valeur à ne pas dépasser) ou le nombre
  de dépassement du seuil (s'il le seuil ne doit pas être
  dépassé plus d'un nombre de fois donné) est retourné.
  TRUE ou FALSE si le détail n'est pas demandé.

  Cette étape est réalisée par un appel à la fonction
  \code{\link{comparaison}}. La valeur de \code{detail}
  prise dans la fonction est celle donnée en argument à
  l'appel de \code{validation.reglementaire}.
}

\section{Autres éléments définissant un critère}{
  En plus des éléments présentés ci-dessus, un seuil est
  défini par d'autres \sQuote{méta}-données. Ces dernières
  sont précisées dans la définition de la fonction
  \code{\link{seuils}}. Une liste \sQuote{castée} en tant
  que \code{'seuil'} qui respecte les définitions
  précédentes ainsi que celles données dans la
  documentation d'un seuil peut parfaitement être utilisée
  en argument de la fonction
  \code{validation.reglementaire}. Par exemple, dans le cas
  où il est nécessaire de calculer la moyenne annuelle en
  ozone et de vérifier que cette moyenne ne dépasse pas 50
  microgrammes par mètre cube (quelle drôle d'idée ...),
  c'ets possible : voir \sQuote{examples}.
}

\section{Fonctionnalités avancées}{
  Pour finir, l'argument \code{\dots} permet de
  personnaliser à l'envie le fonctionnement de la fonction
  \code{validation.reglementaire}. En effet, en ajoutant un
  argument dont le nom correspond à l'un des éléments du
  seuil, la valeur de cet élément va être \sQuote{écrasée}
  au moment du calcul.

  Ainsi, pour les seuils à respecter en moyenne annuelle,
  le taux de représentativité exigé réglementairement est
  90%. La valeur de l'élément \code{rep.comparaison} est
  alors 0.9. Lors d'un suivi \sQuote{à la volée} des
  données de mesures il est possible de trouver ce 90% trop
  contraignant (lors d'une campagne mobile de 2 mois, on
  souahite pouvoir s'informer du respect ou non du critère,
  sans pourtant qu'il soit possible de respecter les 90%).

  Il suffit alors de préciser l'argument suivant
  \code{rep.comparaison=0}, et la comparaison sera
  effectuée (n'importe quelle mesure représentant
  normalement plus de 0% d'une année). voir la section
  \sQuote{examples}.

  De cette manière, n'importe quel paramètre du seuil peut
  être modifié. Cette fonctionalité est toutefois à
  utiliser avec précaution : le remplacement du calcul de
  la valeur maximale journalière de la moyenne glissante
  sur 8 heures par un AOT pourrait avoir des conséquences
  inattendues (ou pas).
}
\examples{
# création d'un jeu de données 'bidon'
#-------------------------------------
donnees <- RegularTimeIntervalDataFrame('2010-01-01', '2010-03-01', 'hour', 'UTC')
donnees$yo <- sample (100, nrow(donnees), TRUE)
head (donnees)
summary (donnees)

# définition d'un nouveau seuil
#------------------------------
mon.seuil <- list(base.comparaison='year', rep.comparaison=0.75,
	seuil=50, polluant='08', cchim='O3', type='personnalisé',
	description="seuil personnalisé pour l'ozone",
	sites=typologies)
	# typologies est un vecteur qui contient toutes les
# typologies définies

# c'est bien une liste !!
mon.seuil
class(mon.seuil) <- 'seuil'
# ça devient un seuil :)
mon.seuil

# redéfinition 'locale' d'un paramètre
#-------------------------------------
# sélection d'un seuil pour tester
seuil <- seuils(cchim='CO', type='valeur limite')[[1]]
seuil

validation.reglementaire(donnees, seuil, resultat='comparaison')
validation.reglementaire(donnees, seuil, resultat='comparaison', rep.comparaison=0)
}
\seealso{
  \code{\link[timetools]{TimeIntervalDataFrame-class}},
  \code{\link{comparaison}}, \code{\link{seuils}},
  \code{\link{preparation.base}}
}

