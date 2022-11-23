# matsunami_RData.R

# 松波さんの最終バージョンから　####
#### k2_林分構造3.R
###  k2_林分構造2.Rの内容は網羅

# "C:/Users/ishid/Dropbox/00D/00/kuraiyama/k123/k2/matsunami/_matsunami_original/20220126/20220113/k2_林分構造5_utf8.R"

# rm(list=ls())

# setwd() ####
PD <- getwd()
setwd("C:/Users/ishid/Dropbox/00D/00/kuraiyama/k123/k2/matsunami/_matsunami_original/20220126/20220113")
dir()

# setClass() ####

setClass("ForestBiomassFieldDroneLiDar",
         slot=c(field="data.frame", 　　　#　毎木調査データ
                drone="data.frame",　　　　# ドローン調査データ
                drone2="data.frame",     #　　ドローン調査現地確認・修正
                lidar="data.frame",     #　　Lidar　ForestTools ttops
                area="numeric"          #    調査面積

         ))

FB <- new("ForestBiomassFieldDroneLiDar")



FB@field <- read.csv( "k2_maiboku.csv",fileEncoding="shift-jis")
FB@drone  <- read.csv( "k2_森林資源票ver4_matsunami_area3.csv",fileEncoding="shift-jis")
FB@drone2 <- read.csv("k2_label_dbh_T.csv",fileEncoding="shift-jis")
FB@lidar  <- read.csv( "K2_ttops_CrownArea2.csv",fileEncoding="shift-jis")

FB@area <- 8012.621
FB
slotNames(FB)

if(0){  # *.csv　を使わない場合
  # d <- read.csv( paste0(ddir,"k2_label_dbh_T.csv"),fileEncoding="shift-jis")
  # d3<-read.csv("k2_maiboku.csv",fileEncoding="shift-jis") #names(d3)
  # d4<-read.csv("k2_森林資源票ver4_matsunami_area3.csv",fileEncoding="shift-jis")
  # d2 <- read.csv("K2_ttops_CrownArea2.csv")
  load("matsunami_d_d2_d3_d4.RData")
  FB@field <- d3
  FB@drone  <- d4
  FB@drone2 <- d
  FB@lidar  <- d2
}


# save(FB,file="./data/matsunam_ForestBiomassFieldDroneLiDar_FB.RData")

