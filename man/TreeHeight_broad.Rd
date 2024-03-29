% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{TreeHeight_broad}
\alias{TreeHeight_broad}
\title{broadleaved tree height estimate from DBH (diameter at brest height)}
\usage{
TreeHeight_broad(dbh)
}
\arguments{
\item{dbh}{}
}
\value{
tree height
}
\description{
broadleaved tree height estimate from DBH (diameter at brest height)
}
\examples{
dbh <- 1:100
h_broad <- TreeHeight_broad(dbh)
h_conif <- TreeHeight_conif(dbh)

plot(dbh,h_conif,type="l",xlab="DBH (cm)",ylab="Tree Height (m)")
lines(dbh,h_broad,lty=2,col="green")
legend(0,30,
c("Coniferous tree species","Broadleaved tree species"),
col=c("black","green"),lty=c(1,2))
}
