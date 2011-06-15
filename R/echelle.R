`echelle` <-
function(donnees, mini=0, maxi=ceiling(max(donnees, na.rm=TRUE)), nclass=101, inverted=FALSE, col = arcenciel(nclass, inverted)){
	donnees <- ifelse(donnees < mini, mini, ifelse(donnees > maxi, maxi, donnees))
	col[round((donnees-mini) * (nclass-1)/(maxi-mini))+1]}

