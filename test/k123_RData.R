# k123_RData.R
#C:\Users\ishid\Dropbox\00D\00\kuraiyama\K123_2022\
# KuraiyamaK123_Qgis.qgz

# rm(list=ls())

getwd()
setwd("../K123_2022")
library(sf)
 dir()



# k123_RData ####

#' make k123_RData
#'
#' @param d  simple future (sf) object forest stund
#' @param lbl column name of tree labels
#' @param sp  column name of tree species
#' @param dbh column name of diameter at breast height
#' @param h   column name of tree height
#' @param v   column name of vital index
#'
#' @return   simple future simplified
#' @export
#'
#' @examples
#' #setwd("../K123_2022")
#'
#' dir.<-"./tree-point"
#' dir(dir.)
#' k1<-st_read(dir.,"k1_2022")
#' k2<-st_read(dir.,"k2_2022")
#' k3<-st_read(dir.,"k3_2022")
#' names(k1) ; names(k2) ; names(k3) ;
#' k123 <- list(k1=K123_RData(k1),k2=K123_RData(k2),k3=K123_RData(k3))
#' plot(k123$k1)
#' plot(k123$k2)
#' plot(k123$k3)
#' #save(k123,file="K123.RData")
K123_RData <- function(d,lbl="label",sp="sp",dbh="dbh",h="h",v="vital"){
  return(d[,c(lbl,sp,dbh,h,v)])
}


#' make  k123plot.RData
#'
#' @param k1crown
#' @param k2crown
#' @param k3crown
#'
#' @return
#' @export
#'
#' @examples
#' dir.<-"./crown_polygon" # dir(dir.)
#' k1crown<-st_read(dir.,"polygon_k1_2022")
#' k2crown<-st_read(dir.,"polygon_k2_2022")
#' k3crown<-st_read(dir.,"polygon_k3_2022")
#' K123plot<- K123plot_RData(k1crown,k2crown,k3crown)
#' #k123plot <- list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
#' par(mfrow=c(1,3));sapply(k123plot,plot)
#' sapply(k123plot,st_area)
#'  #save(k123plot,file="K123plot.RData")
K123plot_RData <- function(k1crown,k2crown,k3crown){
  k123plot <- list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
  return(k123plot)
}


# make  k123ttop.RData ####

dir.<-"./ttop/" # dir(dir.)
k1ttop<-read.csv(paste0(dir.,"k1_ttop.csv"))
k2ttop<-read.csv(paste0(dir.,"k2_ttop.csv"))
k3ttop<-read.csv(paste0(dir.,"k3_ttop.csv"))


k123ttop_RData <- function(ttop.df=k1ttop){
  sfc <- st_sfc(st_multipoint(as.matrix(ttop.df[,c("tx","ty")])))
  tt<-st_sf(data.frame(ttop.df,geom=sfc))
}

k1

tt<-st_sfc(,crs=st_crs(k1))
plot(tt)
,k1ttop$ty)
st_point(c(0,1))

st_geometry(k1)

st_crs(k1)

# make  k123clown.RData ####
dir.<-"./clown_plot_outline_polygon" # dir(dir.)
k1plot<-st_read(dir.,"k1_crown_plot_outlinepolygon")
k2plot<-st_read(dir.,"k2_crown_plot_outlinepolygon")
k3plot<-st_read(dir.,"k3_crown_plot_outlinepolygon")




K123plot_RData <- function(d,lbl="label",sp="sp",dbh="dbh",h="h",v="vital"){
  return(d[,c(lbl,sp,dbh,h,v)])
}



####
names(k1)
names(k2)
names(k3)
#k123<-list(k1=data.frame(label=k1$label,sp=k))
k123<-c()
# k1 ####
d<-k1
(d.<-data.frame(lbl=d$label,sp=d$sp,dbh=d$dbh,h=d$h,st_coordinates(d)))
k123<-c(list(k1=d.))


###
d<-k1crown
plot(d)
as_Spatial(d[1:10,])
st_dimension(d)
st_area(d)
st_drivers(d)
st_as_text(d)
st_bbox(d)
plot(d)

get_key_pos(d)

plot(d %>%  select("樹種"))

plot(k1[,"sp"],add=T)
plot(d %>%  select("樹種"),add=T)
plot(d[c("樹種","ID")])

st_dimension(d)
d2<-d[c(-40,-57),]
plot(d2[c("樹種")],reset=F,main="Kuraiyama_K1")
plot(k1plot, col=NA,border=2,lwd=5,add = TRUE)
plot(k1, col="red",add = TRUE)
#plot(st_centroid(st_geometry(d2)), add = TRUE)
points(k1ttop[,c("tx","ty")], pch=3,col="blue")
legend(4900,-1755,c("tttop"),pch=3,col="blue")


