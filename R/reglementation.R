representatif <- function (x, pc.min=0.75, na.consecutif.max=720) {
	return (mean (!is.na(x))>=pc.min & !any (slide (is.na(x), na.consecutif.max+1, 1, 1) == 1, na.rm=TRUE) )
}

setMethod ('aot40', 'TimeIntervalDataFrame',
	   function (object, mois, heures=10:21, tz='CET', seuil=80, ...) {
		   if (period(object) != new_period(hour=1) )
			   stop('object doit etre periodique, de période égale à 1 heure.')
		   
		   the.calcul <- function(x, seuil)
			   if (mean(!is.na(x)) >= 0.9) {
				   sum(ifelse(!is.na(x) & x > 80, x - 80, 0), na.rm = TRUE)/mean(!is.na(x))
			   } else { NA }

#                            ifelse (mean(!is.na(x)) >= 0.9,
#                                    sum(x[x>seuil]-seuil, na.rm=TRUE)/mean(!is.na(x)),
#                                    NA)

		   if (tz!=timezone (object) )
			   timezone (object) <- tz
		   object <- object[month(end(object)) %in% mois & hour(end(object)) %in% heures,]

		   start <- unique (floor_date (start (object), 'day'))
		   end <- unique (ceiling_date (end (object), 'day'))
		   to <- new ('TimeIntervalDataFrame', start=start, end=end, timezone=tz, data=data.frame (bidon=1:length(start) ) )

		   object <- project (from=object, to=to, FUN=the.calcul, seuil=seuil,
				      split.from=FALSE, merge.from=TRUE, min.coverage=0)
		   timezone (object) <- tz
		   object$bidon <- NULL

		   return (object)
	   } )

#seuils <-
#data.frame (polluant,		# code ISO base XR (NOPOL et surtout pas CCHIM)
#	    type,		# valeur cible, valeur limite, objectif qualité, niveau critique,
#	    			#	seuil d'alerte, seuil d'information et de recommandation
#	    			#	seuil d'évaluation min/max, descriptif
#	    cible_protection,	# santé humaine, végétation, forêt
#	    typologies,		# comme dans le fichier sauf o3 sites industriels (pour nous) -> moyenne annuelle
#	    periode_seuil,	# periode sur laquelle doit être appliquée le seuil
#	    periode_base, 	# periode des données de base (heure, jour, annee, ...) (peut-être NA -> cas des métaux louds)
#	    aggregation,	# fonction qui transforme les données de base pour les 
#				# comparer au seuil. NULL si pas periode_base NULL.
#	    seuil,		# le seuil à proprement à parler
#	    unite,		# unite dans laquelle est exprimée le seuil
#	    debut_application,	# la date à partir de laquelle le seuil est applicable
#	    fin_application,	# la date à partir de laquelle le seuil n'est plus applicable
#	    marges,		# fonction qui calcule le seuil pour une période donnée
#	    			# lorsqu'il y des marges de dépassement à appliquer (seuil 
#	    			# à appliquer progressivement
#	    description		# fonction retournant un texte décrivant le seuil # si différent de default
#	    ref.france		# texte d'où est issu le seuil
#	    )
# a ajouter dans le bilan annuel, mais pas là :
#	la moyenne annuelle ozone
#	moyenne médiane perc 98 et max (base jour) pour les PM2.5

typologies <- c('industriel', 'trafic', 'urbain', 'périurbain', 'rural régional', 'rural national')




comparer.seuils <- function (x, seuils, periode, verif.representativite=TRUE, agrege.first=TRUE) {
	# x peut être un vecteur, ou une data.frame (dans ce cas on suppose que x est regulier)
	#	vect.periode précise alors la periode de ce vecteur permettant de tenter d'appliquer les seuils
	#	tous les seuils fournis sont alors appliqués (brutalement) Si tri il faut, avant il doit être fait
	# x peut être un TimeIntervalDataFrame


}


