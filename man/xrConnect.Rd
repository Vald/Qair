\name{xrConnect}
\alias{Xair.dsn}
\alias{Xair.host,}
\alias{Xair.pwd,}
\alias{Xair.uid,}
\alias{options,}
\alias{xrConnect}
\title{Initialisation d'une connexion a une base XR.}
\usage{
  xrConnect(dsn = NULL, uid = NULL, pwd = NULL,
    host = NULL, ojdbc = NULL, drv.type = NULL)
}
\arguments{
  \item{dsn}{Nom de la base de données (cf le pilote
  concerné, JDBC, ODBC ou Oracle).  optionnel : sera
  demandé si nécessaire.}

  \item{uid}{Identifiant utilisé pour la connexion.
  optionnel : sera demandé si nécessaire.}

  \item{pwd}{Mot de passe pour initialiser la connexion.
  optionnel : sera demandé si nécessaire.}

  \item{host}{Adresse de l'hôte hébergeant la base de
  données. Uniquement pour les systèmes type unix,
  utilisant le pilote JDBC.  optionnel : sera demandé si
  nécessaire.}

  \item{ojdbc}{Emplacement de la librairie contenant la
  définition des classes nécessaires à RJDBC. Uniquement
  dans le cas où jdbc est choisi comme driver (par défaut
  pour les systèmes type 'unix')}

  \item{drv.type}{chaine de caractere indiquant le driver
  de base de données à utiliser.  peut prendre les valeurs
  'jdbc' ou 'odbc'. Par défault, la valeur utilisée est
  'jdbc' pour les systèmes type 'unix' et 'odbc' pour
  windows.}
}
\value{
  Une connexion à la base XR (le type exact dépend du
  pilote utilisé).
}
\description{
  Initialisation d'une connexion a une base XR.
}
\section{Details}{
  Cette fonction permet d'initialiser une connexion avec
  une base XR. Sous windows, le pilote utilisé par défaut
  est ODBC (nécessite d'avoir installer le paquet
  \code{\link[RODBC]{RODBC}}), sous linux JDBC (nécessite
  d'avoir installer le paquet
  \code{\link[RJDBC:JDBC]{RJDBC}}). Il est également
  possible d'utiliser le paquet
  \code{\link[ROracle:Oracle]{ROracle}}).

  Une fois définis, les arguments sont définis pour la
  session entière. Il est possible de définir ces
  'arguments' en utilisant la fonction
  \code{\link[base]{options}} avec les mêmes arguments,
  mais précédés de 'Xair.' (cf la section exemple).

  Les arguments non-spécifiés à l'appel de la fonction et
  qui n'apparaissent pas dans les options seront
  interactivement demandés à l'utilisateur.
}
\examples{
\dontrun{
# dans le cas d'un système windows
# cette ligne permet de définir l'identifiant et la base de données
options (Xair.uid='vlad', Xair.dsn='BaseN')
# la ligne suivante ne demandera que le mot de passe.
xrConnect()
}
}
\seealso{
  \code{\link{xrConnect}}, \code{\link[RODBC]{RODBC}},
  \code{\link[RJDBC]{JDBC}}, \code{\link[base]{options}},
  \code{\link[ROracle]{Oracle}}
}

