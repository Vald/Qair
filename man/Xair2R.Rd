\name{Xair2R}
\alias{Xair2R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extraire des donnees d'une base Xair. }
\description{
  Dans le domaine de la qualite de l'air, les donnees sont souvent stockees dans une base Xair. Cette fonction permet d'y acceder simplement.
}
\usage{
Xair2R (
  polluants, dated, datef, dt=c("qh", "heure", "jour", "mois", "an"),
  merging, codeV = c("A", "R", "O", "W", "P"),
  dsn=NULL, uid=NULL, pwd=NULL,
  brute=FALSE, reseaux=NULL, stations=NULL,
  host=NULL, campagnes=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polluants}{ Noms des polluants a aller chercher dans la base. }
  \item{dated}{ date de debut au format "y-m-d" }
  \item{datef}{ date de fin au format "y-m-d" }
  \item{dt}{ Base temporelle des donnees a extraire. }
  \item{merging}{ Un 'Qair' deja existant auquel ajouter les nouvelles donnees }
  \item{codeV}{ Vecteur de 'character' indiquant les codes de validation de Xair pour lesquels les donnees doivent etre rapatriees. }
  \item{dsn}{ Nom de la base de donnees Xair telle que definie dans le pilote ODBC. Si NULL, la fonction demandera a chaque premiere utilisation
  une valeur, puis la conservera dans options('Xair.dsn'). }
  \item{uid}{ Identifiant a utiliser la connection a Xair. Si NULL, la fonction demandera a chaque premiere utilisation une valeur, 
  puis la conservera dans options('Xair.uid'). }
  \item{pwd}{ Mot de passe pour la connection a Xair. Si NULL, la fonction demandera a chaque premiere utilisation une valeur, 
  puis la conservera dans options('Xair.pwd'). }
  \item{brute}{ 'logical' indiquant si les donnees a charger son les donnees brutes ou les donnees validees. (n'est utilise que 
  pour les donnees qh). }
  \item{reseaux}{ il est possible de charger toutes les mesures definies pour un reseau (au sens Xair) en specifiant le nom (ou les noms)  
  des reseaux souhaites. Les mesures de ces reseaux sont alors ajoutes aux polluants definis dans 'polluants'. Il est egalement possible  
  de ne definir aucun polluant.  }
  \item{stations}{ fonctionnent comme l'argument reseaux. il permet de rapatrier directement toutes les donnees d'une station.}
  \item{host}{ Hote de la base de données Xair (uniquement pour systeme unix utilisant RJDBC).}
  \item{campagnes}{ fonctionnent comme l'argument reseaux. il permet de rapatrier directement toutes les donnees d'une campagne.}
  \item{keep.state}{Booléen indiquant s'il faut rapatrier l'état des mesures en même
	temps que les mesures elle-mêmes.}
  \item{XR6}{Booléne indiquant si la vesrion d'XR utilisée est la v6. Par défaut 
	cette valeur est à TRUE (indiquant que c'est la version 6 qui est utilisée.)
	Cet argument permet de prendre en compte les changements intervenus dans la 
	gestion de FMUL dans la base XR.}
}
\value{
  Renvoie un objet de type 'Qair'.
}
\examples{
#  # ne fonctionne que si une 'data source name' appelee BaseN correspond
#  # une base Xair, et si des mesures dont le nom court de mesure est l'un
#  # de c("O3_VAU", "O3_VER", "O3_AYT"), sont definies dans la base. 
#  donnees <- Xair2R (
#    c("O3_VER", "N2_VER", "NO_VER", "O3_VAU", "O3_AYT", "DV_ATM", "VV_ATM"),
#    "07-01-01", "07-12-31", "qh")  
#  head(donnees) 
#  mesures(donnees) 
#  unites(donnees) 
#  plot(donnees) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
