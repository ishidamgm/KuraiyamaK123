# k123_RData_kunugi_2.R
# C:/Users/ayamo/岐阜大学/山地研ゼミ　2020 - General/功刀さん/KuraiyamaK123-main/test/k123_RData_kunugi.R
# k123_kunugi_LAPTOP-3SBNJGPV.qgs

# rm(list=ls())

getwd()
setwd("../K123_2022")
library(sf)
dir()



##1  k123_RData ####
#' return list of  simplified sf object from shape file of every trees points data
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
setwd("../K123_2022")
dir.<-"./"#tree-point"
dir(dir.)
k1<-st_read("./","k1_2022")
plot(k1)
k1$label

k2<-st_read(dir.,"k2_2022_2")
k3<-st_read(dir.,"k3_2022")
names(k1) ; names(k2) ; names(k3) ;

k123_RData <- function(d,lbl="label",sp="sp",dbh="dbh",h="h",v="vital"){
  return(d[,c(lbl,sp,dbh,h,v)])
}


k123 <- list(k1=k123_RData(k1),k2=k123_RData(k2),k3=k123_RData(k3))
plot(k123$k1)
plot(k123$k2)
plot(k123$k3)

#save(k123,file="K123.RData")


##2  return list of  simplified sf object from shape file of plot outline polygon data
#'
#' @param k1crown
#' @param k2crown
#' @param k3crown
#'
#' @return
#' @export
#'
#' @examples
dir.<-"./clown_plot_outline_polygon" # dir(dir.)
k1plot<-st_read(dir.,"k1_crown_plot_outlinepolygon")
k2plot<-st_read(dir.,"k2_crown_plot_outlinepolygon")
k3plot<-st_read(dir.,"k3_crown_plot_outlinepolygon")
k123plot　<-　list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
k123plot　<-　list(k1,k2,k3)　 #追加
par(mfrow=c(1,3));sapply(k123plot,plot) 　#par(mfrow=グラフグラフィックス環境を変更(複数グラフの描画)
sapply(k123plot,st_area)

#save(k123plot,file="K123plot.RData")

k123plot_RData <- function(k1crown,k2crown,k3crown){
  k123plot <- list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
  return(k123plot)
}



##3   k123ttop_RData ####　
#' return sf object from data frame including x,y, coordinates
#'
#' @param t. data frame including x,y, coordinates
#' @param xc column name of x
#' @param yc column name of y
#' @param crs crs code
#'
#' @return sf object
#' @export
#'
#' @examples
#'
#csvバージョン shpはうまくできず
dir.<-"./clown_plot_outline_polygon/"    # dir(dir.)
k1ttop<-read.csv(paste0(dir.,"k1_ttop_small.csv"))
k2ttop<-read.csv(paste0(dir.,"k2_ttop_small.csv"))
k3ttop<-read.csv(paste0(dir.,"k3_ttop_small.csv"))

names(k1ttop)[c(3,6,7)]<-c("th","tx","ty")
names(k2ttop)[c(3,6,7)]<-c("th","tx","ty")
names(k3ttop)[c(3,6,7)]<-c("th","tx","ty")

datafarame2sf <- function(t.=k2ttop,xc="tx",yc="ty",crs=6675){
  nrows<-nrow(t.)
  geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
  df <- st_sf(t., geometry = geometry,crs=crs)
  for(i in 1:nrows)st_geometry(df)[i]<-st_point(c(t.[i,xc],t.[i,yc]))
  return(df)
}

k123ttop<-list(
  k1=datafarame2sf(k1ttop),
  k2=datafarame2sf(k2ttop),
  k3=datafarame2sf(k3ttop)
)

par(mfrow=c(1,3))
sapply(k123ttop,plot)
for (i in 1:3)plot(k123ttop[[i]]["th"],reset=F)

# save(k123ttop,file="k123ttop.RData")


##4  k123clown.RData ####


  k123crown_RData <- function(d=k1crown,lbl="label",sp="sp",a="Area",h="Height",dbh="dbh"){
  d<-d[,c(lbl,sp,a,h,dbh)]
  names(d)[1:4]<-c("label","species","area","height","dbh")
  return(d)
  }
#'
#'
#' Title
#'
#' @param d
#' @param lbl
#' @param sp
#' @param dbh
#' @param h
#' @param v
#'
#' @return
#' @export
#'
#' @examples]
  getwd()
  setwd("../K123_2022")
  dir()
  dir.<-".//" # dir(dir.)
  k1crown <- st_read(dir.,"polygon_k1_2022")
  k2crown <- st_read(dir.,"polygon_k2_2022")
  k3crown <- st_read(dir.,"polygon_k3_2022")

  k123crown_RData <- function(d=k1crown,lbl="label",sp="sp",a="Area",h="Height",dbh="dbh"){
  d<-d[,c(lbl,sp,a,h,dbh)]
  names(d)[1:4]<-c("label","species","area","height","dbh")
  return(d)
  }

  k123crown <- list(
   k1=k123crown_RData(d=k1crown,lbl="lbl",sp="sp",a="a",h="h",dbh="dbh"),
   k2=k123crown_RData(d=k2crown,lbl="lbl",sp="X_sp_i",a="a",h="h",dbh="dbh"),
   k3=k123crown_RData(d=k3crown,lbl="lbl",sp="sp",a="a",h="h",dbh="dbh")
  )

  for(i in 1:3){
   plot(k123crown[[i]])
  }

 # save(k123crown,file="k123crown.Rdata")



##5  k123_RData_arrange ####

  tn<-sapply(k123,nrow)
  K <- rbind(k123[[1]],k123[[2]],k123[[3]])
  plt<-rep(1:3,tn[1:3])
  K<-cbind(plt,K)
  K<- subset(K,sp!="不明" & sp!="広葉樹"& sp!="針葉樹")
  colnames(K)
  (sp. <- table(K$sp))

  #write.csv(sp.,"spj_correct3.csv")
  (spj<-read.csv("./test/spj_correct2.csv",fileEncoding = "cp932"))

  apg<-read.csv("../k123_2022/種名APG.csv",fileEncoding = "cp932")
  #apg<-data.table::fread("~/Downloads/種名APG.csv",encoding = "cp932")
  names(apg)
  match(spj$spj2,apg$種名)

  i <- match(K$sp,spj$spj)
  K$sp<-spj$spj2[i]
  K123_species<-data.frame(table(K$sp))
  names(K123_species)[1]<-"sp"
  i<-match(K123_species$sp,apg$種名)
  ScienceName<-apg$学名[i]
  ID<-apg$ID[i] ; Family<-apg$科名[i] ; Genus<-apg$属名[i]

  K123_species<-data.frame(K123_species,ID, Family, Genus,ScienceName)


# write.csv(K123_species,"K123_species.csv")
# save(K123_species,file="K123_species.RData")
#edit(K123_species)
# save(K,file="K.RData")

  # k123<-list(k1=subset(K,plt==1)[,-1],
  #            k2=subset(K,plt==2)[,-1],
  #            k3=subset(K,plt==3)[,-1]
  # )
  # save(k123,file="./data/K123.RData")

  data.frame(k123[[2]])


##6  ##
mtime_stamp <- function(fn){
  mt <- file.info(fn)$mtime
  return(gsub("[^0-9]","",mt))
}

mtime_stamp(dir())



##終わり######################

#kunugi label照合の試行
library(tidyr)
library(dplyr)
m1<-intersect(k123$k1$label,k123crown$k1$label)
m2<-intersect(k123$k2$label,k123crown$k2$label)
m3<-intersect(k123$k3$label,k123crown$k3$label)

m1
m2
m3

##gsub＝文字の置換　使い方#########

w<-"abc"
gsub("a","A",w)

#head(データフレームの名前)でデータの中身を一部表示で確認できる


##　K123とK123crownのlabelの照合　##############

#iii=k1,k2,k3
 #k1# iii<-1
 #k2# iii<-2
 #k3# iii<-3

#lbl =ポリゴンのデータ #lbl_=毎木調査のデータ

dc<-data.frame(k123crown[[iii]])
lbl<-dc$label
lbl

d<-data.frame(k123[[iii]])
lbl_<-d$label
lbl_

#k1用　EGH等の文字を取り除く
lbl_<-gsub("[A-Z]","",lbl_)

###照合
i<-match(lbl,lbl_)
#iはポリゴンのlabelに一致する毎木調査データの行番号
d.<-data.frame(lbl,lbl_[i],i)
#order=順番を整えて表示
d.[order(d.[,1]),]



###dbhから樹高を推定 使い方######
#パッケージからkuraiyama123にチェック
#TreeHeight_conif(DBH)

