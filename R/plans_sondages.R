#-----------------------------
# METHODE DES PLANS DE SONDAGE
#-----------------------------

# echantillon est data.frame avec des colonnes "dated", "datef", "pol1", "pol2", etc. et une colonne "grappe", et une "strate"
# auxiliaire est une data.frame avec les colonnes "date" et "strate", et si on realise un redressement , "pol1", "pol2", etc. 

#' Calcul de moyenne par application des methodes issues des plans de sondage
#'
#' Cette fonction permet de réaliser les estimations statistiques développées
#' dans le cadre d'un groupe de travail national piloté par l'ADEME et décrite
#' dans un guide méthodologique (cf. \sQuote{references}).
#'
#' Seules les méthodes de reconstitution de données issues de la théorie des plans 
#' de sondage sont mis-à-disposition au travers de cette fonction. Les autres
#' méthodes (élaboration d'un plan de sondage, reconstitution par regression linéaire, etc.)
#' sont accessibles à travers l'outils développé et mis à disposition par le LCSQA
#' sur son site Internet (cf. \sQuote{references}).
#'
#' @section Note:
#' Dans le cas où le paramètre \sQuote{auxiliaire} ne contient aucune variable
#' autre que celle identifiée par \sQuote{strate}, aucun redressement n'est effectué.
#'
#' Si plusieurs variables d'intérêt et/ou auxiliaires sont fournies, les estimations sont
#' calculées pour toutes les combinaisons (intérêt , auxiliaire) possible.
#'
#' @param echantillon \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' contenant les variables d'intérets ainsi qu'une colonne dont le nom est identique 
#' à la valeur de \sQuote{grappe} et une autre identique à la valeur de \sQuote{strate}.
#' @param auxiliaire \code{\link[=TimeIntervalDataFrame-class]{TimeIntervalDataFrame}}
#' contenant les variables  auxiliaires si un redressement est à réaliser. Une colonne doit avoir
#' un nom identique la valeur de \sQuote{strate}. Les intervalles de cet objet
#' définissent la période d'estimation de la moyenne.
#' @param redressement chaîne de caractères indiquant le ou les redressements à effectuer 
#' (c('diff', 'quot', 'reg')).
#' NULL si aucun redressement ne doit être calculé.
#' @param iso vecteur de valeurs numériques indiquant dans quelle \sQuote{grappe} iso
#' chaque valeur horaire de la periode d'estimation se trouve. Dans le cas où ce vecteur
#' n'est pas nul, la méthode ISO est appliquée (cf guide LCSQA pour de plus amples détails).
#' Les données doivent alors être impérativement horaires. NULL si la méthode ISO ne doit
#' pas être appliquée.
#' @param grappe chaîne de caractères indiquant le nom de la colonne contenant la 
#' répartition des grappes.
#' @param strate le nom des colonnes contenant la répartition des strates. Doit être
#' identique pour \sQuote{echantillon} et \sQuote{auxiliaire}.
#'
#' @return une liste contenant pour chaque couple
#' (\sQuote{interet}, \sQuote{auxiliare}) une \code{\link[base]{data.frame}}
#' contenant les estimations et les variances d'estimations.
#'
#' @references guide ADEME pour l'élaboration d'un plan
#' d'échantillonnage temporel et la reconstitution de données :
#' \url{http://www2.ademe.fr/servlet/getDoc?cid=96&m=3&id=63725&p1=02&p2=01&ref=17597}
#'
#' Pour les utilisateurs dotés d'un compte sur le site du LCSQA (\url{http://www.lcsqa.org/}), le guide 
#' est également disponible avec ses annexes à cette adresse :
#' \url{http://www.lcsqa.org/echantillonnage-reconstitution-donnees/documentation/guide-pratique-elaboration-plans-echantillonnag}.
estimation_sondage <- function(echantillon, auxiliaire, redressement=NULL, iso=NULL, grappe='grappe', strate='strate')
{
	interets <- setdiff(names(echantillon), c(grappe, strate))
	auxiliaires <- setdiff(names(auxiliaire), strate)

	resultats <- list()
	for (int in interets)
		for (aux in auxiliaires)
			resultats[[paste(int, aux, sep='_')]] <- es.sub (echantillon[c(int, grappe, strate)],
									 auxiliaire[c(aux, strate)],
									 redressement, iso, grappe, strate)
	return (resultats)
}

es.sub <- function (echantillon, auxiliaire, redressement, iso, grappe, strate)
{
	#-- ça on s'en fout, c'est à voir si on fait le test ou pas
	#         if(length(echantillon)==2) return(NULL)
	#-- ça, c'est gérer directement dans les TimeIntervalDataFrame et les noms 'grappe' et 'strate'
	#         echantillon <- echantillon[c("dated", "datef", "grappe", "strate", names(echantillon)[!(names(echantillon) %in% c("dated", "datef", "grappe", "strate"))])]
	#         auxiliaire <- auxiliaire[c("dated", "datef", "strate", names(auxiliaire)[!(names(auxiliaire) %in% c("dated", "datef", "strate"))])]

	# constantes
	#-----------
	#         duree <- as.numeric(mean(auxiliaire$datef-auxiliaire$dated))
	duree <- mean (when(auxiliaire))
	redressement <- if(any(redressement %in% c("diff", "quot", "reg")))
		redressement[which(redressement %in% c("diff", "quot", "reg"))] else
		NULL
	# nom des variables dont on cherche a estimer la moyenne
	aux <- setdiff (names(auxiliaire), strate)[1]
	interet <- setdiff (names(echantillon), c(grappe, strate, names(aux)))[1]

	# mise en forme
	#--------------
	#         echValide <- echantillon[!is.na(echantillon$grappe) & !is.na(echantillon[[interet]]), 1:5]
	echValide <- echantillon[!is.na(echantillon[[grappe]]) & !is.na(echantillon[[interet]]), c(grappe, strate, interet[1])]

	if(!is.null(redressement))
	{
		grappe_d <- as.POSIXct(tapply(start (echValide), echValide[[grappe]], min), origin=origin)
		grappe_f <- as.POSIXct(tapply(end (echValide), echValide[[grappe]], max), origin=origin)
		compa_date <- outer(grappe_d, start(auxiliaire), "<=") & outer(grappe_f, end(auxiliaire), ">=")

		if(!is.null(iso))
		{
			gr_nom <- as.numeric(sapply(strsplit(names(grappe_d), " "), "[[", 1))

			laquelle_grappe <- lapply(lapply(iso+sum(!is.na(unique(iso)))*(auxiliaire[[strate]]-1), "==", gr_nom), which)
			laquelle_grappe <- ifelse(lapply(laquelle_grappe, any), laquelle_grappe, NA)
			laquelle_grappe <- sapply(mapply("==", ifelse(apply(compa_date, 2, any), apply(compa_date, 2, which), NA), laquelle_grappe), which)
			laquelle_grappe <- ifelse(lapply(laquelle_grappe, any), laquelle_grappe, length(grappe_f)+1)

			indices <- unlist(mapply("[", apply(compa_date, 2, which), laquelle_grappe))

			echValide[[grappe]] <- sapply(lapply(echValide$grappe, "==", gr_nom), which)
		} else {
			gr_nom <- as.numeric(sapply(strsplit(names(grappe_d), " "), "[[", 1))
			indices <- unlist(ifelse(apply(compa_date, 2, any), apply(compa_date, 2, which), NA))
			indices <- gr_nom[indices]
		}

		auxValide <- auxiliaire[!is.na(indices), c(strate, aux)]
		auxValide[[grappe]] <- indices[!is.na(indices)]
		auxValide <- auxValide[c(strate, grappe, aux)]
	}

	# determination des caracteristiques utiles de chaque grappe
	#-----------------------------------------------------------
	echValide$poids <- as.numeric (when (echValide))
	grappe_ech <- split(data.frame (echValide), echValide$grappe)
	grappe_ech <- data.frame(t(rbind(poids = sapply(lapply(grappe_ech, "[[", "poids"), sum),
					 sapply(lapply(grappe_ech, "[", c(grappe, strate, interet)), colMeans))))
	grappe_ech$poids <- unlist(mapply("/",
					  split(grappe_ech$poids, grappe_ech[[strate]]),
					  as.list(tapply(grappe_ech$poids, grappe_ech[[strate]], sum)), SIMPLIFY=F))
	names(grappe_ech)[4] <- "moy"

	if(!is.null(redressement))
	{
		auxValide$poids <- as.numeric(when (auxValide))
		grappe_aux <- split (data.frame (auxValide[!is.na(auxValide[[aux]]), ]),
				     auxValide[[grappe]][!is.na(auxValide[[aux]])])
		grappe_aux <- data.frame(t(rbind(poids = sapply(lapply(grappe_aux, "[[", "poids"), sum),
						 sapply(lapply(grappe_aux, "[", c(grappe, strate, aux)), colMeans))))
		grappe_aux$poids <- unlist(mapply("/",
						  split(grappe_aux$poids, grappe_aux[[strate]]),
						  as.list(tapply(grappe_aux$poids, grappe_aux[[strate]], sum)), SIMPLIFY=F))
		names(grappe_aux)[4] <- "moy"
	}

	# determination des caracteristiques utiles de chaque strate
	#-----------------------------------------------------------
	strates <- data.frame(poids=tapply(auxiliaire[[strate]], auxiliaire[[strate]], length)/nrow(auxiliaire))
	strates$m_ech <- tapply(grappe_ech$poids*grappe_ech$moy, grappe_ech[[strate]], sum)

	# CAS D'UNE POPULATION FINIE
	grappes <- split(grappe_ech, grappe_ech[[strate]])
	nbGrappeEch <- sapply(grappes, nrow)
	nbGrappePop <- nbGrappeEch*
			tapply(as.numeric(when(auxiliaire)), auxiliaire[[strate]], sum)/
			tapply(as.numeric(when(echValide)), echValide[[strate]], sum)
	varGrappe <- sapply(lapply(mapply("-", mapply("*", lapply(grappes, "[[", "moy"), lapply(grappes, "[[", "poids"), SIMPLIFY=F), as.list(strates$m_ech / nbGrappeEch), SIMPLIFY=F), "^", 2), sum)

	strates$v_ech <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe

	if(!is.null(redressement))
	{
		if(nrow(grappe_ech) != nrow(grappe_aux))
		{
			print("le nombre de grappes dans la variable auxiliaire est insuffisant, le(s) redressement(s) n'est(e sont) pas calcule(s)")
			strates$cov <- NA
		} else {
			strates$m_aux <- tapply(grappe_aux$poids*grappe_aux$moy, grappe_aux[[strate]], sum)
			strates$m_reelle <- tapply(auxiliaire[[aux]], auxiliaire[[strate]], mean, na.rm=T)

			# CAS D'UNE POPULATION FINIE
			grappes <- split(grappe_aux, grappe_aux[[strate]])
			varGrappe <- sapply(lapply(mapply("-", mapply("*", lapply(grappes, "[[", "moy"), lapply(grappes, "[[", "poids"), SIMPLIFY=F), as.list(strates$m_aux / nbGrappeEch), SIMPLIFY=F), "^", 2), sum)

			strates$v_aux <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe

			varGrappe <- sapply(mapply("*", mapply("-", mapply("*", lapply(split(grappe_ech, grappe_ech[[strate]]), "[[", "moy"), lapply(split(grappe_ech, grappe_ech[[strate]]), "[[", "poids"), SIMPLIFY=F), as.list(strates$m_ech/nbGrappeEch), SIMPLIFY=F), mapply("-", mapply("*", lapply(split(grappe_aux, grappe_aux[[strate]]), "[[", "moy"), lapply(split(grappe_aux, grappe_aux[[strate]]), "[[", "poids"), SIMPLIFY=F), as.list(strates$m_aux/nbGrappeEch), SIMPLIFY=F), SIMPLIFY=F),sum, na.rm=T)
			strates$cov <- nbGrappeEch/nbGrappePop * (nbGrappePop - nbGrappeEch)/(nbGrappeEch - 1) * varGrappe
		}
	}



	# estimation des caracteristiques sur la population
	#--------------------------------------------------
	pop <- data.frame(moy=sum(strates$poids*strates$m_ech), var=sum(strates$poids^2*strates$v_ech))
	if("diff" %in% redressement)
	{
		if(all(!is.na(strates$cov)))
		{
			pop$m_diff <- sum(strates$poids*(strates$m_ech-strates$m_aux+strates$m_reelle))
			pop$v_diff <- sum(strates$poids^2*(strates$v_ech+strates$v_aux-2*strates$cov))
		} else {
			pop$m_diff <- NA
			pop$v_diff <- NA
		}
	}
	if("quot" %in% redressement) 
	{
		if(all(!is.na(strates$cov))) 
		{
			r <- pop$moy/sum(strates$poids*strates$m_aux)
			pop$m_quot <- sum(strates$poids*(strates$m_ech/strates$m_aux*strates$m_reelle))
			pop$v_quot <- sum(strates$poids^2*(strates$v_ech+r^2*strates$v_aux-2*r*strates$cov))
		} else {
			pop$m_quot <- NA
			pop$v_quot <- NA
		}
	}
	if("reg" %in% redressement)
	{
		if(all(!is.na(strates$cov)))
		{
			b <- strates$cov/strates$v_aux
			pop$m_reg <- sum(strates$poids*(strates$m_ech+b*(strates$m_reelle-strates$m_aux)))
			roCarre <- strates$cov^2/(strates$v_ech*strates$v_aux)
			pop$v_reg <- sum(strates$poids^2*strates$v_ech*(1-roCarre))
		} else {
			pop$m_reg <- NA
			pop$v_reg <- NA
		}
	}

	return (pop)
}

#pop$moy + 1.96*c(-1, 1)*sqrt(pop$var)
#pop$m_diff + 1.96*c(-1, 1)*sqrt(pop$v_diff)
#pop$m_quot + 1.96*c(-1, 1)*sqrt(pop$v_quot)
#pop$m_reg + 1.96*c(-1, 1)*sqrt(pop$v_reg)



