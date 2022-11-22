#kuraiyamaK123Rdata.R
library(shapefiles)
wd<-("C:/Users/ayamo/岐阜大学/山地研ゼミ　2020 - General/功刀さん/K123_2022")
wd<-("C:/Users/ayamo/岐阜大学/山地研ゼミ　2020 - General/功刀さん/K123_2022")
wd<-"../K123_2022"
setwd(wd)
dir()

dir(pattern = ".*.shp")

#' read shapefile points
#'
#' @param filename
#'
#' @return data frame of shapefile points
#' @export
#'
#' @examples
#'k1<- ReadShapefile_Points("k1_2022")
#'k2<-　ReadShapefile_Points("k2_2022_2")
#'k3<-  ReadShapefile_Points("k3_2022")
#'k123_points<-list(k1=k1,k2=k2,K3=k3)
#'save(k123_points,file="k123_points.RData")
ReadShapefile_Points <-function(filename="k2_2022_2"){

  d<-shapefiles::read.shapefile(filename)　
  shp<-d$shp$shp;
  dbf<-d$dbf$dbf;	names(dbf)
  d<-data.frame(dbf,shp)
  d$sp<-iconv(d$sp, from = "shift-jis", to = "utf8")
  return(d)
}

edit(d)



