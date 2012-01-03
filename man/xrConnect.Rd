\name{xrConnect}
\alias{options,}
\alias{Xair.dsn}
\alias{Xair.host,}
\alias{Xair.pwd,}
\alias{Xair.uid,}
\alias{xrConnect}
\title{Initialisation d'une connection a une base XR.}
\usage{
  xrConnect(dsn = NULL, uid = NULL, pwd = NULL,
    host = NULL, ojdbc = NULL)
}
\arguments{
  \item{dsn}{Nom de la base de données (cf le pilote
  concerné, JDBC ou ODBC).  optionnel : sera demandé si
  nécessaire.}

  \item{uid}{Identifiant utilisé pour la connection.
  optionnel : sera demandé si nécessaire.}

  \item{pwd}{Mot de passe pour initialiser la connection.
  optionnel : sera demandé si nécessaire.}

  \item{host}{Adresse de l'hôte hébergeant la base de
  données. Uniquement pour les systèmes type unix,
  utilisant le pilote JDBC.  optionnel : sera demandé si
  nécessaire.}

  \item{ojdbc}{Emplacement de la librairie contenant la
  définition des classes nécessaires à RJDBC. Uniquement
  pour les systèmes type unix, utilisant le pilote JDBC.}
}
\value{
  Une connection à la base XR (le type exact dépend du
  pilote utilisé).
}
\description{
  Initialisation d'une connection a une base XR.
}
\section{Details}{
  Cette fonction permet d'initialiser une connection avec
  une base XR. Sous windows, le pilote utilisé est ODBC
  (nécessite d'avoir installer le paquet
  \code{\link[RODBC]{RODBC}}), sous linux JDBC (nécessite
  d'avoir installer le paquet \code{\link[RJDBC]{JDBC}}).

  Une fois définis, les arguments sont définis pour la
  session entière. Il est possible de définir ces
  'argument' en utilisant la fonction
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
  \code{\link[RJDBC]{JDBC}}, \code{\link[base]{options}}
}

