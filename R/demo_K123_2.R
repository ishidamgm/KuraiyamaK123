# matsunami.R
# 2022/11/23
# k2_林分構造5_utf8.R
# load("./data/matsunami.RData")


# plot_test ####

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

#' demonstration of calculation  and plots of k2 (Matsunami 2022)
#'
#' @param FB
#'
#' @return
#' @export
#'
#' @examples
#'
#' demo_matsunami_k2(FB)
#'
#'
# >>>>> ####
demo_matsunami_k2 <- function(FB){


survey_area <- FB@area ## fin_area_polygon.shp
d<-FB@drone2
names(d)

table(d$ID)
sp0<-d$樹種　; dbh <- d$dbh ; ca0 <-d$樹冠面積... ; h0 <- d$樹高.m. ; lbl <- d$label.i
ca <- d$area
sp<-d$sp
x <- d$X ; y <-d$Y
ba <- (dbh/200)^2*pi ## ba00<-ba
v<-0.5*ba*h0

conif_sp<-c("アスナロ","スギ","サワラ")
conif<-is.element(sp,conif_sp)
sp[conif]

####　基礎集計

table(sp)
sp_ba <- aggregate(ba,by=list(sp),sum) #data.frame(sp_ba,table(sp))

##図化 ####
plot(x,y)
hist(dbh)
hist(h0)
plot(ca,ca0) ; abline(coef=c(0,1))

#### dbh-h　関係式
i<-conif
plot(dbh[i],h0[i])
plot(dbh[!i],h0[!i])

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




#### 針葉樹
i<-conif
par(mfrow=c(1,2))
plot(ca[i],ba[i],main="針葉樹",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)")
#ylab="Crown Area (m^2)",xlab="Basal Area (m^2)"
ans<-lm(ba[i]~ca[i]+0)
summary(ans)			###  Adjusted R-squared:  0.8618
abline(ans,col="red",lty=2,lwd=2)
text(30,0.8,"y =   0.0075718 x ")
text(30,0.75,"Adjusted R^2:  0.8618 (p<0.001)",cex=0.7)

#### 広葉樹
plot(ca[!i],ba[!i],main="広葉樹",ylab="胸高断面積(m^2)",xlab="樹冠面積(m^2)")
ans<-lm(ba[!i]~ca[!i]+0)
abline(ans,col="red",lty=2,lwd=2)
text(60,0.35,"y =   0.002090 x ")
text(60,0.30,"   Adjusted R^2:  0.8902 (p<0.001)",cex=0.8)

summary(ans)



###### ca=f(dbh,h) 大きな改善認められず
ans<-lm(ca[i]~ba[i]+h0[i])
summary(ans)			###  R-squared:  0.6518



####
par(mfrow=c(2,2))
### 針葉樹
dbh_conif_ <- ca2dbh_conif(ca[i])
hist(dbh_conif_,breaks = seq(10,110,10),xlab="DBH (cm)",main="Direct measurement/nconifer")
hist(dbh[i],breaks = seq(10,110,10),xlab="DBH (cm)",main="Estimated from drone ortho/nconifer")
### 広葉樹
dbh_broad  <- dbh[!i]
dbh_broad_ <- ca2dbh_broad(ca[!i])
hist(dbh_broad_,breaks = seq(10,80,10),xlab="DBH (cm)",main="Direct measurement/nbroadleaved")
hist(dbh_broad ,breaks = seq(10,80,10),xlab="DBH (cm)",main="Estimated from drone ortho/nbroadleaved")

# 収量密度 ####
a <- survey_area #調査面積  8012.621


# 収量密度図　yield_density diagram ####

#### 毎木 (全立木　胸高以上) yd_all
d3<-FB@field  #read.csv("k2_maiboku.csv",fileEncoding="shift-jis") #names(d3)
dbh3<-d3$dbh ; v3<-d3$v ;ba3 <- d3$ba
sum(v3)
(yd_all <- yield_density(v3,a)) #632 788.755639 795.80469	# 532 663.952532 719.24115

# 樹冠に到達していたかどうかの情報 ####
ID_feye <- d$ID[match(d3$label,lbl)]
canopy<-!is.na(ID_feye)
100*sum(ba3[canopy])/sum(ba3) 	### 林冠木の胸高断面積合計割合は　89.73665%
sum(v3[canopy])/sum(v3) 		#			　材積割合は　 93.28978%

# 直径分布図作 ####
par(mfrow=c(1,1))
cls<-seq(0,110,10)
freq<-rbind(table(cut(dbh3[canopy],cls)),table(cut(dbh3[!canopy],cls)))
#barplot(freq,name=paste0("-",10*(1:11)),xlab="DBH (cm)",ylab="N")
barplot(freq,name=paste0("-",10*(1:11)),xlab="胸高直径 (cm)",ylab="N")
#legend(5,150,c("canopy","understory"),pch=c("■","□"))
legend(5,150,c("林冠木","下層木"),pch=c("■","□"))


#### 毎木 (林冠木) yd_canopy
yd_canopy <- yield_density(v3[canopy],a)	    #196 244.614091 670.97851


### 張さんオリジナル yd_feye_original
d4<-FB@drone #read.csv("k2_森林資源票ver4_matsunami_area3.csv",fileEncoding="shift-jis")
(yd_feye_original <- yield_density(d4$volume2,a))	#267 333.224297 677.73153	#267 333.224297 753.79092

### 張さんオリジナル yd_feye_original 針広なし
#d4<-read.csv("k2_森林資源票ver4_matsunami_area3.csv")
(yd_feye_original_ <- yield_density(d4$v_,a))	#267 333.224297 753.79092


#### ドローン写真からの推定 yd_canopy_drone_針広なし
ca<-ca[order(-ca)]
ba_drone_all<-ca2ba(ca)
dbh_drone_all<-ba2dbh(ba_drone_all)
v__ <- ba_drone_all*h0*0.5
( yd_canopy_drone_針広なし <- yield_density(v__,a)) # 227 283.303054 622.58981

#### ドローン写真からの推定(針葉樹・広葉樹分けて)
i<-conif
ba_drone_conif<-ca2ba_conif(ca[i])
ba_drone_broad<-ca2ba_broad(ca[!i])
ba_drone_cb <-c(ba_drone_conif,ba_drone_broad)
dbh_drone_cb <-ba2dbh(ba_drone_cb)

v_conif <- ba_drone_conif * h0[i]  * 0.5
v_broad <- ba_drone_broad * h0[!i] * 0.5
v_ <- c(v_conif,v_broad)
( yd_canopy_drone_針広別 <- yield_density(v_,a)) # 227 283.303054 766.02678

#### yd_ttops_針広なし
d2 <- FB@lidar #read.csv("K2_ttops_CrownArea2.csv")

ca2<-d2$CrownArea
v_tt<-0.5*d2$th*ca2ba(ca2)
( yd_ttops_針広なし <- yield_density(v_tt,a)) # ttops 231 288.295178 570.79614

#### yd_ttops_針広別
j<-match(d2$Id_2,d$ID)
dd<-data.frame(d2,sp=d$sp[j])
#edit(dd)
sp_<-dd$sp
i<-is.element(sp_,conif_sp)

ca2<-dd$CrownArea

v_tt_conif<-0.5*ca2ba_conif(ca2[i])*d2$th[i]
v_tt_broad<-0.5*ca2ba_broad(ca2[!i])*d2$th[!i]
v_tt_na<-0.5*ca2ba_broad(ca2[is.na(i)])*d2$th[is.na(i)]
v_tt2<-na.omit(c(v_tt_conif,v_tt_broad,v_tt_na))
(yd_ttops_針広別 <- yield_density(v_tt2,a)) 	# 231 288.295178 776.29935

# 収量密度図 ####
windows()
par(mfrow=c(1,1))

plot(yd_all,type="l",lwd=2,xlab="順位 (N/ha)",ylab="積算材積 (m^3/ha)",main="収量密度図",ylim=c(0,800),lty=2)
lines(yd_canopy,col="black",lwd=5)			#### 毎木 (林冠木) yd_canopy
lines(yd_feye_original,col="magenta",lty=1)		#### 張さんオリジナル yd_feye_original
lines(yd_feye_original_,col="magenta",lty=2)		#### 張さんオリジナル yd_feye_original 針広なし
lines(yd_ttops_針広なし,col="blue",lty=2)	 	#### ttops (針葉樹・広葉樹分けないで　青線)

legend(150,300,
       c(	"現地毎木調査　(胸高以上)",
          "現地毎木調査 (林冠木)",
          "ドローン写真からの推定(針広区別なし)",
          "ドローン写真からの推定(針広区別あり)",
          "ドローン写真からの推定(針広区別あり　張さんオリジナル)",
          "LiDAR DSMからの推定　(針広区別なし)",
          "LiDAR DSMからの推定　(針広区別あり)"),

       col=c("black","black","red","red","magenta","blue","blue"),
       lty=c(2,2,2,2,2,2,2),
       lwd=c(2,2,2,2,2,2,2),cex=0.7

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



#ttops_vs_drone　### #
par(mfrow=c(1,2))
hist(h0);hist(d2$th)

tid<-d2$field_1
tlbl<-d2$Id_2


ttn<-data.frame(table(tlbl))
lbl<-d$ID
lbl
ttops_n<-rep(0,nrow(d))
ttops_n[match(ttn[,1],lbl)]<-ttn[,2]

ttops_n
ttops_n_conif <-ttops_n[i]
ttops_n_broad <-ttops_n[!i]
sapply(list(ttops_n,ttops_n_conif,ttops_n_broad ),summary)


par(mfrow=c(1,3))
sapply(list(ttops_n,ttops_n_conif,ttops_n_broad ),hist)

# 樹高推定 ####

ttops_h<-rep(0,nrow(d))

for (ii in 1:nrow(d)){
  x0<-x[ii];y0<-y[ii]
  j<-which.min((d2$tx-x0)^2+(d2$ty-y0)^2)
  ttops_h[ii]<-d2$th[j]
}

d<-data.frame(d,ttops_n,ttops_h)
##### write.csv(d,"k2_stand_ttops_drone.csv")
#   Tree Height estimated with LiDar and drone ####
hist(h0)
hist(ttops_h)
plot(h0,ttops_h,xlab="Tree Height estimated with LiDar(m)",ylab="Tree Height estimated with drone ortho (m)")
ans<-lm(ttops_h~h0+0)
summary(ans)			###
cor(ba,ca)	### 0.49
abline(ans,col="red",lty=2,lwd=2)
text(30,15,"y =   0.972134 x ")
text(30,13,"Adjusted R^2:   0.9927  (p<0.001)",cex=0.7)

} # <<<<< ####

