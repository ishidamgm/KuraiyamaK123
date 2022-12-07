# k123_RData.R
#C:\Users\ishid\Dropbox\00D\00\kuraiyama\K123_2022\
# KuraiyamaK123_Qgis.qgz

# rm(list=ls())

# getwd()
# setwd("../K123_2022")
# library(sf)
#  dir()



# k123_RData ####
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
#' #setwd("../K123_2022")
#'
#' dir.<-"./tree-point"
#' dir(dir.)
#' k1<-st_read(dir.,"k1_2022")
#' k2<-st_read(dir.,"k2_2022")
#' k3<-st_read(dir.,"k3_2022")
#' names(k1) ; names(k2) ; names(k3) ;
#' k123 <- list(k1=k123_RData(k1),k2=k123_RData(k2),k3=k123_RData(k3))
#' plot(k123$k1)
#' plot(k123$k2)
#' plot(k123$k3)
#' #save(k123,file="K123.RData")
k123_RData <- function(d,lbl="label",sp="sp",dbh="dbh",h="h",v="vital"){
  return(d[,c(lbl,sp,dbh,h,v)])
}


#' return list of  simplified sf object from shape file of plot outline polygon data
#'
#' @param k1crown
#' @param k2crown
#' @param k3crown
#'
#' @return
#' @export
#'
#' @examples
#' dir.<-"./clown_plot_outline_polygon" # dir(dir.)
#' k1plot<-st_read(dir.,"k1_crown_plot_outlinepolygon")
#' k2plot<-st_read(dir.,"k2_crown_plot_outlinepolygon")
#'k3plot<-st_read(dir.,"k3_crown_plot_outlinepolygon")
#' #k123plot <- list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
#' par(mfrow=c(1,3));sapply(k123plot,plot)
#' sapply(k123plot,st_area)
#'  #save(k123plot,file="K123plot.RData")
k123plot_RData <- function(k1crown,k2crown,k3crown){
  k123plot <- list(k1=st_geometry(k1plot),k2=st_geometry(k2plot),k3=st_geometry(k3plot))
  return(k123plot)
}

# k123ttop_RData ####
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
#' dir.<-"./ttop/" # dir(dir.)
#' k1ttop<-read.csv(paste0(dir.,"k1_ttop.csv"))
#' k2ttop<-read.csv(paste0(dir.,"k2_ttop.csv"))
#' k3ttop<-read.csv(paste0(dir.,"k3_ttop.csv"))
#'
#' k123ttop<-list(
#'   k1=datafarame2sf(k1ttop),
#'   k2=datafarame2sf(k2ttop),
#'   k3=datafarame2sf(k3ttop)
#' )
#' par(mfrow=c(1,3))
#' sapply(k123ttop,plot)
#' for (i in 1:3)plot(k123ttop[[i]]["th"],reset=F)
#' # save(k123ttop,file="k123ttop.RData")
datafarame2sf <- function(t.=k2ttop,xc="tx",yc="ty",crs=6675){
  nrows<-nrow(t.)
  geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection()))
  df <- st_sf(t., geometry = geometry,crs=crs)
  for(i in 1:nrows)st_geometry(df)[i]<-st_point(c(t.[i,xc],t.[i,yc]))
  return(df)
  }


# k123clown.RData ####


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
#' @examples
#'  dir.<-"./crown_polygon" #
#'  dir(dir.)
#' k1crown <- st_read(dir.,"polygon_k1_2022")
#' k2crown <- st_read(dir.,"polygon_k2_2022")
#' k3crown <- st_read(dir.,"polygon_k3_2022")
#' k123crown <- list(
#'    k1=k123crown_RData(d=k1crown,lbl="Label1",sp="樹種",a="Area",h="Height"),
#'    k2=k123crown_RData(k2crown,lbl="label.i",sp="樹種",a="Area",h="Height"),
#'    k3=k123crown_RData(k3crown,lbl="Label1",sp="樹種",a="Area",h="Height")
#'    )
#'
#' for(i in 1:3){
#' plot(K123crown[[i]])
#' }
#' # save(k123crown,file="k123crown.Rdata")



k123crown_RData <- function(d=k1crown,lbl="label",sp="sp",a="Area",h="Height"){
  d<-d[,c(lbl,sp,a,h)]
  names(d)[1:4]<-c("label","species","area","height")
  return(d)
}




# k123_RData_arrange ####

tn<-sapply(k123,nrow)
K <- rbind(k123[[1]],k123[[2]],k123[[3]])
plt<-rep(1:3,tn[1:3])
K<-cbind(plt,K)
K <- subset(K,dbh>=10 & sp!="不明")
colnames(K)
(sp. <- table(K$sp))
#write.csv(sp.,"spj_correct.csv")
(spj<-read.csv("spj_correct2.csv"))
apg<-read.csv("~/Downloads/種名APG.csv",fileEncoding = "cp932")
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
# save(K123_species,file="../data/K123_species.RData")
#edit(K123_species)

# save(K,file="../data/K.RData")


mtime_stamp(dir())


mtime_stamp <- function(fn){
  mt <- file.info(fn)$mtime
  return(gsub("[^0-9]","",mt))
}


plot(k123crown)


