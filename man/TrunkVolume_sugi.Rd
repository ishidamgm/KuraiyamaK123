% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{TrunkVolume_sugi}
\alias{TrunkVolume_sugi}
\title{Japan Rinyacho Zaisekishiki Sugi (Cryptomeria japonica)}
\usage{
TrunkVolume_sugi(DBH, H)
}
\arguments{
\item{DBH}{numeric vector of DBH}

\item{H}{numeric vector of tree heights}
}
\value{
numeric vector of tree trunk volumes
}
\description{
Japan Rinyacho Zaisekishiki Sugi (Cryptomeria japonica)
}
\examples{

DBH<-10:80
H <- DBH^0.8
TrunkVolume_sugi(DBH,H)

}
