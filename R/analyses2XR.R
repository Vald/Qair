#' Creation d'un fichier d'import de données manuelles pour XR
#'
#' Cette fonction lit un fichier xlsx contenant des colonnees spécifiques et
#' créé un fichier importable dans XR. Le fichier d'origine doit contenir 
#' les colonnes suivantes :
#' NSIT (numéro du site dans XR), NUM_ECH (identifiant de l'échantillon à
#' renseigner selon les règles de l'AASQA), BT (le prélèvement est-il un
#' blanc ? 2 si oui, 0 si non), CODE_PREL (code de la méthode de prélèvement
#' dans XR), DEBUT (date de début au format date dans le fichier excel - si 
#' chaîne de caractères, ça ne fonctionnera pas), HDEBUT (heure de début, au
#' format heure), FIN et HFIN (date et heure de fin, aux formats appropriés), 
#' ANALYSE (date d'analyse au format date). Les colonnes HDEBUT et HFIN ne sont
#' pas obligatoires.
#'
#' En plus de ces colonnes, une colonne supplémentaire doit être ajoutée pour 
#' chaque polluant ayant fait l'objet d'une analyse. Le nom des colonnes doit 
#' être constitué de la chaîne "ISO_" suivi du code ISO du polluant en question.
#' Par exemple ISO_01 pour le SO2.
#'
#' @param fichier nom du fichier d'entrée
#' @param nreseau numero du réseau (tel qu'il doit être intégré dans XR).
#' @param labo Nom du laboratoire ayant réalisé les analyses
#'  ('Micropolluants Technologie', 'IANESCO CHIMIE', ...).
#' @param fichier_export nom du fichier de sortie (par défaut =
#'  fichier+'export.csv')
#' @param sheet numéro ou titre de l'onglet à lire
#' @param startRow numéro de ligne contenant les titres de colonne (et donc
#'  première ligne à lire dans l'onglet)
#' @param infLQ caractères utilisés dans le fichier d'entrée pour indiquer
#'  que la concentration est inférieure à la limite de quantification.
#' @param unites chaîne de caractères à utiliser pour les unités du polluant.
analyses2XR <- function(fichier, nreseau, labo, fichier_export=NULL, sheet=1, startRow=1,
					infLQ=c('\\*', '< '), unites='microg/m3') {

	osaf <- getOption('stringsAsFactors[')
	options(stringsAsFactors=FALSE)

	# fonction interne --------------------------------------------------------

	wt <- function(x, file, append=TRUE) utils::write.table(
		x = x, file = file, col.names = FALSE, row.names = FALSE,
		append = append, quote = FALSE, fileEncoding = "UTF8", eol = '\r\n')

	# fichier de sortie -------------------------------------------------------

	if(is.null(fichier_export))
		fichier_export <- paste0(sub('.xlsx', '', fichier), '_export.csv')

	# lecture des données -----------------------------------------------------

	data <- openxlsx::read.xlsx(fichier, sheet, startRow)

	# mise en forme et écriture -----------------------------------------------

	# juste pour préciser la première ligne du fichier
	fl <- TRUE
	for (nprel in 1:nrow(data)) {

		# lecture de la ligne du prelevement

		prel <- data[nprel,]
		if(!'BT' %in% names(prel)) prel[['BT']] <- 0

		# suppression des colonnes inutiles

		prel <- prel[,names(prel) != paste0('X', 1:ncol(prel))]

		# mise des concentrations

		id_mols <- grep('^ISO_', names(prel), value=TRUE)
		concs   <- data.frame(NOPOL=sub('ISO_', '', id_mols),
							  conc =unlist(prel[id_mols]))

		concs[['etat']] <- ifelse(
			grepl('\\*', concs[['conc']]) | grepl('< ', concs[['conc']]),
			'L',
			'A')
		concs[['conc']] <- sub('\\*', '', concs[['conc']])
		concs[['conc']] <- sub('< *', '', concs[['conc']])
		concs[['conc']] <- sub(',', '.', concs[['conc']])
		concs[['conc']] <- as.numeric(concs[['conc']])
		if(any(is.na(concs[['conc']]))) {
			warning('LE PRELEVEMENT ', prel[['NUM_ECH']],
					" contient des valeurs invalides. Il n'est pas pris en compte")
			next
		}

		if (prel[['BT']] == 2)
			concs[['etat']] <- ifelse(concs[['etat']] == 'L', 'l','I')

		concs[['unite']]       <- unites
		concs[['commentaire']] <- ''
		#concs[['commentaire']] <- if(prel[['CATEGORIE']] == 2) prel[['UNITE']] else ''

		message('prélèvement ', nprel, ' ...')

		# première ligne

		wt(sprintf("SITE_PRELEVEMENT;%s;%s;;;0.0;0.0;;0;0;;0;;;",
				   nreseau, prel[['NSIT']]),
		   fichier_export, !fl)

		# deuxième ligne
		if(!'FIN' %in% names(prel)) prel[['FIN']] <- prel[['DEBUT']]
		if('HDEBUT' %in% names(prel)) prel[['DEBUT']] <-
			prel[['DEBUT']] + ifelse(is.na(prel[['HDEBUT']]), 0, prel[['HDEBUT']])
		if('HFIN' %in% names(prel))
			prel[['FIN']] <- prel[['FIN']] + ifelse(is.na(prel[['HFIN']]), 0, prel[['HFIN']])

		prel[['DEBUT']]   <- openxlsx::convertToDateTime(prel[['DEBUT']])
		prel[['FIN']]     <- openxlsx::convertToDateTime(prel[['FIN']])
		prel[['ANALYSE']] <- openxlsx::convertToDateTime(prel[['ANALYSE']])

		wt(sprintf("PRELEVEMENT;%s;%s;%s;;%s;%s;%s;%i;%i;%s",
			nreseau,								# numéro de réseau
			prel[['NSIT']],							# Numéro de site de mesure 
			prel[['CODE_PREL']],					# Code Méthode
			# Code méthode d'extraction 
			strftime(prel[['DEBUT']], '%d/%m/%y %H:%M'),# Date de début 
			strftime(prel[['FIN']], '%d/%m/%y %H:%M'),# Date de fin 
			prel[['NUM_ECH']],						# Référence
			prel[['BT']],							# Catégorie
			nrow(concs),							# Nombre d'analyses
			if(prel[['BT']] == 2)'(Blanc terrain exprimé en masse)' else ''),
		fichier_export)

		# ligne pour chaque analyse

		wt(sprintf("%s;%s;%s;%s;%6.3f;%s;%s;%s;%s;%s;%s;%s;%s",
			"",									# Nom court de la mesure 
			concs[['NOPOL']],					# Identifiant du paramètre
			"",									# Code méthode
			"",									# Libellé du paramètre 
			concs[['conc']],					# Valeur
			concs[['etat']],					# Code qualité 
			"",									# Unité
			strftime(prel[['ANALYSE']], '%d/%m/%y %H:%M'),# Date d'analyse 
			labo,								# Libellé du laboratoire 
			"",									# Modèle de l'équipement 
			"",									# Numéro de série de l'équipement 
			"",									# Méthode de mesure 
			concs[['unite']]),					# commentaire
		fichier_export)

		message('\t\t\t\t... ok')

		# pour continuer à remplir le fichier
		fl <- FALSE
	}
	options(stringsAsFactors=osaf)
}

