'calcul_seuils' <-
function(date, x, id_poll, seuils, pourDiffusion=FALSE){
	if(missing(seuils)){
		if(!exists('seuils.Qair'))data(seuils.Qair)
		seuils <- seuils.Qair
		}
	seuils <- seuils[seuils$id_poll == id_poll & ifelse(rep(pourDiffusion, nrow(seuils)), seuils$diffusion==1, TRUE),]
	resultat <- NULL
	for(i in 1:nrow(seuils))
		resultat[i] <- calcul_seuil(date, x, seuils[i,])
	resultat
	}
