% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculation.R
\name{dataframe2list}
\alias{dataframe2list}
\title{return list from dara frame}
\usage{
dataframe2list(d3, index = "L2", col = c("X", "Y"))
}
\arguments{
\item{d3}{data frame}

\item{index}{characters, name of index  in data frame column}

\item{col}{vector of characters, data frame column names to include}
}
\value{
list
}
\description{
return list from dara frame
}
\examples{
dir.<-"../K123_2022/crown_polygon" # dir(dir.)
k1crown<-read_sf(dir.,"polygon_k1_2022")
plot(k1crown)
plot(k1crown \%>\% select(樹種))
k1crown$geometry[[1]][[1]]
d<-k1crown
st_dimension(d)
d2<-d[!is.na(st_dimension(d)),]
d3<- as.data.frame(st_coordinates(d2))
L<-dataframe2list(d3,index="L2",col=c("X","Y"))
x12 <- range(d3$X) ; y12 <- range(d3$Y)
plot(0,type="n",xlim=x12,ylim=y12)
sapply(L,polygon)



}
