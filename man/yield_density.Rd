% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{yield_density}
\alias{yield_density}
\title{YN plot}
\usage{
yield_density(v, a)
}
\arguments{
\item{v}{volume of trees}

\item{a}{area of plot}
}
\value{
data frame of yn (number of trees ordered with size , yield of cumulative volume)
}
\description{
YN plot
}
\examples{
#### simple example

v=1:100  ; a=10000
plot(yield_density(v,a))

#### example of Kuraiyama K123

pn <- 2 # plot number
d<-subset(k123_field,plot==pn)
a<-k123_site$area[pn]
v<-TrunkVolume(d$sp,d$dbh,d$h)
NY <- yield_density(v,a)
plot(NY,
 xlab="N",
 ylab="Cummulative trunk volume(m^3/ha)",
 main=paste("Site :",pn))

}
