# demo_k123_2.R
# 2022/11/23 -> 2023/2/5

#load("./data/k123crown.RData")



#' demo plot k123
#'
#'
#' @return
#' @export
#'
#' @examples
#' demo_plot_K123(1)
#' demo_plot_K123(2)
#' demo_plot_K123(3)

demo_plot_K123 <- function(i){
  plot(k123crown[[i]]["height"],reset=F,main=names(k123)[i])
  plot(k123plot[[i]], col=NA,border=2,lwd=5,add = TRUE)
  plot(k123[[i]], col="red",add = TRUE)
  plot(k123ttop[[i]],pch=3,col="blue",add = TRUE)
}


# >>>>> ####
# load RData ####
# K123_species
# k123 (K123)
# k123_points
# k123_site
# k123crown
# k123plot (K123plot)
# k123ttop


#demo_k123 <- function(iii=1){

iii <- 2　　# k plot number　set　####

survey_area <- k123_site$area[iii]  ## fin_area_polygon.shp
d.<-k123crown[[iii]]
plot(d.)
d<-data.frame(d.)
names(d)
lbl <- d$label ; sp<-d$species ;ca <-d$area ;h <- d$height; dbh <- d$dbh
tail(d) ; h[366]<-15;h[367]<-10  #!!!! 適当なデータをとりあえずいれている　####
ba <- (dbh/200)^2*pi ## ba00<-ba
v<-0.5*ba*h

conif_sp<-c("アスナロ","スギ","サワラ")
conif<-is.element(sp,conif_sp)
sp[conif]
ca[conif]
####　基礎集計
table(sp)
(sp_ba <- aggregate(ba,by=list(sp),sum)) #data.frame(sp_ba,table(sp))

##図化 ####
par(mfrow=c(1,2),mar=c(4,4,4,4))
hist(dbh)
hist(h)

#### dbh-h　関係式
par(mfrow=c(1,2),mar=c(4,4,4,4))
i<-conif
plot(dbh[i],h[i],main="Conifer",xlab="胸高直径(cm)",ylab="樹高(m)")
plot(dbh[!i],h[!i],main="Broad leaved",xlab="胸高直径(cm)",ylab="樹高(m)")

#### LiDarとDroneによる算出樹高の比較
####　胸高断面積と樹冠面積　全体　針葉樹と広葉樹で違いは?　ca=f(dbh)
####　樹冠面積 の説明変数に　胸高直径と樹高を使うと　?　ca=f(dbh,h) 大きな改善認められず


#### 全種
plot(ca,ba,main="全種",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)")
#ylab="Crown Area (m^2)",xlab="Basal Area (m^2)"
ans<-lm(ba~ca+0)
summary(ans)			###
cor(ba,ca)	### 0.49
abline(ans,col="red",lty=2,lwd=2)
text(30,0.8,"y =   0.0044189 x ")
text(30,0.75,"Adjusted R^2:  0.6273 (p<0.001)",cex=0.7)



#' correlation analysis between basal area and crown area
#'
#' @param x numeric vector of crown area
#' @param y numeric vector of basal area
#' @param main
#' @param ylab
#' @param xlab
#' @param Fx a value of horizontal position of  text "y = a x"
#' @param Fy  a value of vertical position of  text "y = a x"
#' @param Rx a value of horizontal position of  text "Adj. R^2 (P)"
#' @param Ry a value of vertical position of  text "Adj. R^2 (P)"
#'
#' @return
#' @export
#'
#' @examples
#' windows()
#' par(mfrow=c(1,3))
#' #針葉樹
#' i<-conif
#' kaiki(x=ca[i],y=ba[i],main="針葉樹",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)",
#' Fx=20,Fy=0.3,Rx=20,Ry=0.25)
#' #広葉樹
#' i<-!conif
#' kaiki(x=ca[i],y=ba[i],main="広葉樹",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)",
#' Fx=40,Fy=0.65,Rx=50,Ry=0.6)
#' #全体
#' kaiki(x=ca,y=ba,main="全種",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)",
#' Fx=40,Fy=0.65,Rx=50,Ry=0.6)

kaiki<-function(x=ca[i],y=ba[i],
                main="針葉樹",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)",
                Fx=20,Fy=0.3,Rx=20,Ry=0.25){
  plot(ca[i],ba[i],main=main,ylab=ylab,xlab=xlab)
  ans<-lm(y~x+0)
  (sans<-summary(ans))
  coef<-round(ans$coefficients,5)
  P<- sans$coefficient[1,4]
  Pr<-ifelse(P>0.001,paste("P =",P),"P<0.001")
  r2<-round(sans$adj.r.squared,3)

  abline(ans,col="red",lty=2,lwd=2)
  text(Fx,Fy,paste("y =",coef,"x" ),col="blue")
  text(Rx,Ry, paste(" Adj. R^2:",r2,"(",Pr,")"),cex=0.8,col="blue")

}




####
par(mfrow=c(2,2))
### 針葉樹
i<-conif
dbh_conif_ <- ca2dbh_conif(ca[i])
hist(dbh_conif_,breaks = seq(0,120,10),xlab="DBH (cm)",main="Direct measurement (conifer)")
hist(dbh[i],breaks = seq(0,120,10),xlab="DBH (cm)",main="Estimated from drone ortho(conifer)")
### 広葉樹
dbh_broad  <- dbh[!i] # dbh_broad[dbh_broad==100 ] <-39.2 !!!!要修正
dbh_broad_ <- ca2dbh_broad(ca[!i])
#summary(dbh_broad_) # hist(dbh_broad_)
hist(dbh_broad_,breaks = seq(0,110,10),xlab="DBH (cm)",main="Direct measurement (broadleaved")
hist(dbh_broad ,breaks = seq(0,110,10),xlab="DBH (cm)",main="Estimated from drone ortho/nbroadleaved")

# 収量密度 ####
(a <- survey_area) #調査面積


# 収量密度図　yield_density diagram ####

#' calculate tree trunk volume from species,dbh, tree height using with function of Japanese governments
#'
#' @param sp  character vector of Japanese species name (Katakana)
#' @param dbh vector of dbh (cm)
#' @param h   vector of tree height (m)
#'
#' @return    vector of trunk volume
#' @export
#'
#' @examples
#' TrunkVolume(c("スギ","ヒノキ","ブナ"),c(50,30,80),c(20,15,25))
TrunkVolume <- function(sp,dbh,h){
  v=ifelse(sp=="スギ",TrunkVolume_sugi(dbh,h),
           ifelse(sp=="ヒノキ",TrunkVolume_hinoki(dbh,h),
                  ifelse(sp=="サワラ",TrunkVolume_sawara(dbh,h),
                         ifelse(sp=="アスナロ",TrunkVolume_hinoki(dbh,h),
                                TrunkVolume_broardleaved(dbh,h)))))
  return(v)
}


#' calculate tree height from species and dbh using with function (Kato)
#'
#' @param sp  character vector of Japanese species name (Katakana)
#' @param dbh vector of dbh (cm)
#'
#' @return    vector of tree height
#' @export
#'
#' @examples
#' TreeHeight(c("スギ","ヒノキ","ブナ"),c(50,30,80))
TreeHeight <- function(sp,dbh){
  h=ifelse(is.element(sp,conif_sp),TreeHeight_conif(dbh),
           TreeHeight_broad(dbh))
  return(h)
}


# yd_all 毎木 (全立木　胸高以上)  ####
plot(k123[[iii]])
d0<-data.frame(k123[[iii]])   #names(d0)
d0<-subset(d0,dbh>=10)
lbl0<- d0$label; sp0<-d0$sp ; dbh0<-d0$dbh ;h0<-d0$h
#dbh0[is.na(dbh0)]<-9 ;h0[is.na(h0)]<-0 #### !!! 暫定入力
ba0<-pi*(dbh0/200)^2
i<-h0==0 ; h0[i]<-TreeHeight(sp0[i],dbh0[i])
v0<-TrunkVolume(sp0,dbh0,h0)
(yd_all <- yield_density(v0,a)) #632 788.755639 795.80469	# 532 663.952532 719.24115

#d$sp0


# 樹冠に到達していたかどうかの情報 ####
lbl0
lbl_<-gsub("[A-Z]","",lbl0)

#ID_feye <- d$ID[match(lbl,lbl)]
#canopy<-!is.na(ID_feye)

canopy<-!is.na(match(lbl_,lbl))
which(is.na(canopy))

100*sum(ba0[canopy])/sum(ba0) 	### 林冠木の胸高断面積合計割合は　89.73665%
sum(v0[canopy])/sum(v0) 		#			　材積割合は　 93.28978%

# 直径分布図作 ####
par(mfrow=c(1,1))
cls<-seq(0,110,10)
freq<-rbind(table(cut(dbh0[canopy],cls)),table(cut(dbh0[!canopy],cls)))
#barplot(freq,name=paste0("-",10*(1:11)),xlab="DBH (cm)",ylab="N")
barplot(freq,name=paste0("-",10*(1:11)),xlab="胸高直径 (cm)",ylab="N")
#legend(5,150,c("canopy","understory"),pch=c("■","□"))
legend(8,50,c("林冠木","下層木"),pch=c("■","□"))


# yd_canopy 毎木 (林冠木) ####
yd_canopy <- yield_density(v0[canopy],a)	    #196 244.614091 670.97851

# drone ####
# ドローンで推定する場合　樹冠面積⇒胸高断面積⇒05*ba*h⇒材積
# ### 張さんオリジナル yd_feye_original
(yd_feye_original <- yield_density(v,a))	#267 333.224297 677.73153	#267 333.224297 753.79092
#
# ### 張さんオリジナル yd_feye_original 針広なし
# #d4<-read.csv("k2_森林資源票ver4_matsunami_area3.csv")
(yd_feye_original_ <- yield_density(v,a))	#267 333.224297 753.79092
#
#
# ドローン写真からの推定 yd_canopy_drone_針広なし ####
#ca<-ca[order(-ca)]
ba_drone_all<-ca2ba(ca)
dbh_drone_all<-ba2dbh(ba_drone_all)
v__ <- ba_drone_all*h*0.5
( yd_canopy_drone_針広なし <- yield_density(v__,a)) # 227 283.303054 622.58981

# ドローン写真からの推定(針葉樹・広葉樹分けて) ####
i<-conif
ba_drone_conif<-ca2ba_conif(ca[i])
ba_drone_broad<-ca2ba_broad(ca[!i])
ba_drone_cb <-c(ba_drone_conif,ba_drone_broad)
dbh_drone_cb <-ba2dbh(ba_drone_cb)

v_conif <- ba_drone_conif * h[i]  * 0.5
v_broad <- ba_drone_broad * h[!i] * 0.5
v_ <- c(v_conif,v_broad)
( yd_canopy_drone_針広別 <- yield_density(v_,a)) # 227 283.303054 766.02678

#### yd_ttops_針広なし
#d2 <- FB@lidar #read.csv("K2_ttops_CrownArea2.csv")

ttop<-k123ttop[[iii]]
plot(ttop)
names(ttop)
dt<-data.frame(ttop)[,c("treeID","th","winRadius","tx","ty","z")]

# ttop_CrownArea ####
load("./data/k123ttops_CrownArea.RData")
ca2<-ttops_CrownArea[[iii]]
i<-match(dt$treeID,ca2$id)
dt<-data.frame(dt,ca2[i,])
head(dt)
ca2<-dt$CrownArea
v_tt<-0.5*dt$th*ca2ba(ca2)
( yd_ttops_針広なし <- yield_density(v_tt,a)) # ttops 231 288.295178 570.79614


# 収量密度図 ####
windows()
par(mfrow=c(1,1))

plot(yd_all,type="l",lwd=2,xlab="順位 (N/ha)",ylab="積算材積 (m^3/ha)",main=paste("収量密度図 K",iii),ylim=c(0,800),lty=2)
lines(yd_canopy,col="black",lwd=5)			#### 毎木 (林冠木) yd_canopy
lines(yd_canopy_drone_針広なし,col="magenta",lty=1)		#### drone_針広なし
lines(yd_canopy_drone_針広別,col="magenta",lty=2)		#### drone_針広別
lines(yd_ttops_針広なし,col="blue",lty=2)	 	#### ttops (針葉樹・広葉樹分けないで　青線)

legend(300,430,
       c(	"現地毎木調査　(胸高以上)",
          "現地毎木調査 (林冠木)",
          "ドローン写真からの推定(針広区別なし)",
          "ドローン写真からの推定(針広区別あり)",
          "LiDAR DSMからの推定　(針広区別なし)"),


       col=c("black","black","magenta","magenta","blue"),
       lty=c(2,1,1,2,2),
       lwd=c(2,5,2,2,2),cex=0.7

)

# 収量密度図 その2 ####　
par(mfrow=c(1,1))
plot(yd_all,type="l",lwd=2,xlab="順位 (N/ha)",ylab="積算材積 (m^3/ha)",main="収量密度図",ylim=c(0,800),lty=2)
lines(yd_canopy,col="gray",lty=1,lwd=5)
lines(yd_all,col="black",lty=2,lwd=2)			#### 毎木 (林冠木) yd_canopy
lines(yd_feye_original,col="black",lty=5,lwd=2)		#### 張さんオリジナル yd_feye_original
lines(yd_feye_original_,col="black",lty=4,lwd=2)		#### 張さんオリジナル yd_feye_original 針広なし
lines(yd_ttops_針広なし,col="black",lty=1,lwd=3)	 	#### ttops (針葉樹・広葉樹分けないで　青線)

legend(200,300,
       c(	"現地毎木調査　(胸高以上)",
          "現地毎木調査 (林冠木)",
          "ドローン目視判別法(針広区別あり)",
          "ドローン目視判別法(針広区別なし)",
          "LiDAR自動計算法(針広区別なし)"),cex=0.8,
       col=c("black","gray","black","black","black"),
       lty=c(2,1,5,4,1),
       lwd=c(2,5,2,2,3)
)

# 収量密度図 その3 カラー ####　
par(mfrow=c(1,1))
plot(yd_all,type="l",lwd=2,xlab="順位 (N/ha)",ylab="積算材積 (m^3/ha)",main="収量密度図",ylim=c(0,800),lty=3)
lines(yd_canopy,col="gray",lty=1,lwd=5)
lines(yd_all,col="black",lty=3,lwd=2)			#### 毎木 (林冠木) yd_canopy
lines(yd_feye_original,col="magenta",lty=1,lwd=3)		#### 張さんオリジナル yd_feye_original
lines(yd_feye_original_,col="magenta",lty=5,lwd=2)		#### 張さんオリジナル yd_feye_original 針広なし
lines(yd_ttops_針広なし,col="blue",lty=5,lwd=2)	 	#### ttops (針葉樹・広葉樹分けないで　青線)

legend(200,300,
       c(	"現地毎木調査　(胸高以上)",
          "現地毎木調査 (林冠木)",
          "ドローン目視判別法(針広区別あり)",
          "ドローン目視判別法(針広区別なし)",
          "LiDAR自動計算法(針広区別なし)"),cex=0.79,
       col=c("black","gray","magenta","magenta","blue"),
       lty=c(3,1,1,2,2),
       lwd=c(2,5,3,2,2)
)







