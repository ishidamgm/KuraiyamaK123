% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{TreeHeight}
\alias{TreeHeight}
\title{calculate tree height from species and dbh using with function (Kato)}
\usage{
TreeHeight(conif_sp, sp, dbh)
}
\arguments{
\item{conif_sp}{character vector of conifer specie list}

\item{sp}{character vector of  species to calculate tree height}

\item{dbh}{vector of dbh (cm)}
}
\value{
vector of tree height
}
\description{
calculate tree height from species and dbh using with function (Kato)
}
\examples{
TreeHeight(c("スギ"),c("スギ","ヒノキ","ブナ"),c(50,30,80))

}
