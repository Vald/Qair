\name{seuils.Qair}
\alias{seuils.Qair}
\docType{data}
\title{ seuils de calculs reglementaires}
\description{
  seuils.Qair est une data.frame contenant les definitions des parametres des calculs de valeurs reglementaires
}
\usage{data(seuils.Qair)}
\format{
  A data frame with 49 observations on the following 17 variables.
  \describe{
    \item{\code{id_poll}}{contient l'identifiant correspondant au polluant pour lequel le calcul est defini. }
    \item{\code{polluant}}{nom du polluant en toutes lettres.}
    \item{\code{precision}}{precision imposee au calcul. (utilise dans la fonction round.a) }
    \item{\code{donnees_base}}{heure ou jour (pour horaire ou journalier) }
    \item{\code{periode}}{indique les mois a prendre en compte pour le calcul d el'indicateur reglementaire. }
    \item{\code{agregation}}{periode sur laquelle doit etre faite l'agregation. ('an', 'jour' ou '8heures') }
    \item{\code{glissement}}{le calcul doit-il etre glissant ?}
    \item{\code{operation}}{operation a realiser lors de l'agregation ('moyenne', 'max', 'AOT40', NA, 'Cxx' -> voir details)}
    \item{\code{seuil}}{valeur du seuil a comparer au resultat du calcul.}
    \item{\code{remanence}}{si le seuil doit etre depasse pendant plusieurs unite de base consecutive (par ex. pendant 3 heures). }
    \item{\code{nb_dep}}{le resultat du calcul doit-il etre le nombre de depassements du seuil (voir details) valeurs prise 0 ou 1. }
    \item{\code{nb_jour_dep}}{le resultat du calcul doit-il etre le nombre de jours de depassement du seuil (voir details) valeurs prise 0 ou 1.}
    \item{\code{annee}}{annee a partir de laquelle le seuil est applicable. }
    \item{\code{diffusion}}{le calcul doit-il etre realise. }
    \item{\code{typeCalcul}}{nature reglementaire du calcul ('valeur limite', objectif de qualite', 'seuil d'alerte', etc.)}
    \item{\code{enteteLigne1}}{entete de ligne dans le tableau donnant la nature reglementaire du calcul. }
    \item{\code{enteteLigne2}}{entete de ligne dans le tableau donnant le detail du calcul realise. }
  }
}
\details{
  la definition des calculs reglementaires correspond a celle issue des reglementations francaise et europeenne
  applicables au moment de la compilation du package.

  pour la definition des percentiles, si la valeur a calculee est le percentile 99.2, doit ertre notee 'C99.2'.

  Si nb_dep est a 1, le nombre de depassements sera calcule ; si nb_jour_dep est a 1, le nombre de jours de depassement sera calcule.
  Ces deux valeurs ne peuvent etre simultanement mises a 1.
}
\source{
  DIRECTIVE 2008/50/CE DU PARLEMENT EUROPÉEN ET DU CONSEIL du 21 mai 2008 concernant la qualité de l'air ambiant et un air pur pour l'Europe

  ( http://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2008:152:0001:0044:FR:PDF )

  Décret no 2008-1152 du 7 novembre 2008 relatif à la qualité de l'air

  ( http://www.legifrance.gouv.fr/affichTexte.do?cidTexte=JORFTEXT000019735657 )

  Décret no 2007-1479 du 12 octobre 2007 relatif à la qualité de l'air et modifiant le code de l'environnement (partie réglementaire)

  ( http://www.legifrance.gouv.fr/affichTexte.do?cidTexte=JORFTEXT000000619033&dateTexte= )

  Décret n° 2003-1085 du 12 novembre 2003 portant transposition de la directive 2002/3/CE du Parlement européen et du Conseil 
  du 12 février 2002 et modifiant le décret n° 98-360 du 6 mai 1998 relatif à la surveillance de la qualité de l'air et de ses effets sur la santé 
  et sur l'environnement, aux objectifs de qualité de l'air, aux seuils d'alerte et aux valeurs limites

  ( http://www.legifrance.gouv.fr/affichTexte.do?cidTexte=JORFTEXT000000430152&dateTexte= )

  Décret n°2002-213 du 15 février 2002 portant transposition des directives 1999/30/CE du Conseil du 22 avril 1999 et 2000/69/CE du 
  Parlement européen et du Conseil du 16 novembre 2000 et modifiant le décret n° 98-360 du 6 mai 1998 relatif à la surveillance de la qualité 
  de l'air et de ses effets sur la santé et sur l'environnement, aux objectifs de qualité de l'air, aux seuils d'alerte et aux valeurs limites.

  ( http://www.legifrance.gouv.fr/affichTexte.do?cidTexte=JORFTEXT000000775221&dateTexte= )

}
\examples{
data(seuils.Qair)
edit(seuils.Qair)
}
\keyword{datasets}
