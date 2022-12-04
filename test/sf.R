#  sf.R
#　shapefileswで林冠ポリゴンが読み込めない。2022/11/30
#　おそらく種名等に2バイト文字含まれているため?
#　sfのst_readでは読めるが，s3クラスのリターン
#　現時点でマルチポリゴンの座標取得ができない
# > st_coordinates(k2crown)
# Error in (function (..., deparse.level = 1)  :
#            number of columns of matrices must match (see arg 79)
# ポイントや1ポリゴンのshpでは座標取得が可能　
# 関数自作　sf_polygon_xy

# k1の844削除?　empty

# rm(list=ls())
library(tidyverse)
library(sf)

getwd()
#setwd("../K123_2022")
dir()
dir.<-"./tree-point" # dir(dir.)
k1<-st_read(dir.,"k1_2022")
k2<-st_read(dir.,"k2_2022")
k3<-st_read(dir.,"k3_2022")
K1 <- data.frame(st_coordinates(k1))
K2 <- data.frame(st_coordinates(k2))
K3 <- data.frame(st_coordinates(k3))

dir.<-"./crown_polygon" # dir(dir.)
k1crown<-st_read(dir.,"polygon_k1_2022") # k1crown<-read_sf(dir.,"polygon_k1_2022")
k2crown<-st_read(dir.,"polygon_k2_2022")
k3crown<-st_read(dir.,"polygon_k3_2022")
dir.<-"./clown_plot_outline_polygon" # dir(dir.)
k1plot<-st_read(dir.,"k1_crown_plot_outlinepolygon2")
k2plot<-st_read(dir.,"k2_crown_plot_outlinepolygon")
k3plot<-st_read(dir.,"k3_crown_plot_outlinepolygon")

dir. <-"./ttop/" # dir(dir.)
k1ttop <- read.csv(paste0(dir.,"k1_ttop.csv"))
k2ttop <- read.csv(paste0(dir.,"k2_ttop.csv"))
k3ttop <- read.csv(paste0(dir.,"k3_ttop.csv"))

# k123_P L plot, ttop###
names(k1)

xy<-data.frame(st_coordinates(k1plot))
edit(xy)
xy<-subset(xy,L1==1)
names(xy)
plot(xy[,c("X","Y")],type="b")
text(xy[,1],xy[,2],1:nrow(xy),col="red",cex=0.5)
edit(xy)
tapply(d3,d3$L2,list)

#' return list from dara frame
#'
#' @param d3  data frame
#' @param index characters, name of index  in data frame column
#' @param col vector of characters, data frame column names to include
#'
#' @return list
#' @export
#'
#' @examples
#' dir.<-"../K123_2022/crown_polygon" # dir(dir.)
#' k1crown<-read_sf(dir.,"polygon_k1_2022")
#' plot(k1crown)
#' plot(k1crown %>% select(樹種))
#' k1crown$geometry[[1]][[1]]
#' d<-k1crown
#' st_dimension(d)
#' d2<-d[!is.na(st_dimension(d)),]
#' d3<- as.data.frame(st_coordinates(d2))
#'L<-dataframe2list(d3,index="L2",col=c("X","Y"))
#' x12 <- range(d3$X) ; y12 <- range(d3$Y)
#' plot(0,type="n",xlim=x12,ylim=y12)
#' sapply(L,polygon)
#' points(K1[,c("X","Y")],col="blue")
#' k1plot_xy<-st_coordinates(k1)
#'
#' points(k1ttop[,c("tx","ty")],col="red")
#' k1plot_xy<-st_coordinates(k1plot)
#' polygon(k1plot_xy,col=NA,border="red",lwd=3)
#'
#'
dataframe2list <- function(d3,index="L2",col=c("X","Y")){
  L<-c()
  unq<-unique(d3[,index])
  for (i in unq){
    j<-d3[,index]==i
    L<-c(L,list(d3[j,col]))
  }
  names(L)<-index
  return(L)
}

#' return list of polygons of sf class (st_read), can`t extract by st_coordinate
#'
#' @param sf.
#'
#' @return list of polygons
#' @export
#'
#' @examples
#' rng<-st_bbox(k1)
#' plot(0,type="n",xlim=c(rng[1],rng[3]),ylim=c(rng[2],rng[4]))
#' PL<-sf_polygon_xy(k1)
#' sapply(PL,polygon)
sf_polygon_xy <- function(sf.=k1crown){
  pxy<-c()
  for (i in 71:143){#nrow(sf.)
    pxy<-c(pxy,list(sf.$geometry[[i]][[1]]))
  }
  return(pxy)
}






