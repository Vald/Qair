% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reglementation.R
\name{aot}
\alias{aot}
\title{Calcule un aot}
\usage{
aot(x, seuil, rep.min)
}
\arguments{
\item{x}{vecteur sur lequel le calcul doit être appliqué.}

\item{seuil}{valeur du seuil de l'AOT.}

\item{rep.min}{représentativité minimale pour réaliser le calcul.
Valeur entre [0-1].}
}
\description{
Fonction permettant de calculer des AOT (Accumulated Ozone exposure
over a Threshold of 40 Parts Per Billion).
}
\details{
Cette fonction réalise le calcul 'brutal' d'un AOT : le vecteur qui
lui est donné en argument doit déjà avoir été traité pour ne contenir
que les valeurs aux dates et heures souhaitées. Elle calcule uniquement
la somme des dépassements du seuil indiqué, sous réserve que le nombre
de données valides dans le vecteur soit supérieur au pourcentage
indiqué par rep.min.
}

