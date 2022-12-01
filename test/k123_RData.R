# k123_RData.R
#C:\Users\ishid\Dropbox\00D\00\kuraiyama\K123_2022\
# KuraiyamaK123_Qgis.qgz

# rm(list=ls())

getwd()
#setwd("../K123_2022")
library(sf)
dir()
dir.<-"./tree-point" # dir(dir.)
k1<-st_read(dir.,"k1_2022")
k2<-st_read(dir.,"k2_2022")
k3<-st_read(dir.,"k3_2022")

dir.<-"./crown_polygon" # dir(dir.)
k1crown<-st_read(dir.,"polygon_k1_2022")
k2crown<-st_read(dir.,"polygon_k2_2022")
k3crown<-st_read(dir.,"polygon_k3_2022")

dir.<-"./clown_plot_outline_polygon" # dir(dir.)
k1plot<-st_read(dir.,"k1_crown_plot_outlinepolygon")
k2plot<-st_read(dir.,"k2_crown_plot_outlinepolygon")
k3plot<-st_read(dir.,"k3_crown_plot_outlinepolygon")

dir.<-"./ttop/" # dir(dir.)
k1ttop<-read.csv(paste0(dir.,"k1_ttop.csv"))
k2ttop<-read.csv(paste0(dir.,"k2_ttop.csv"))
k3ttop<-read.csv(paste0(dir.,"k3_ttop.csv"))

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





