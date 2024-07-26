# calculation.R

#### 多角形の面積を求める関数　
#' area return a  area from polygon xy coordinates
#'
#' @param xy a matrix or data frame of xy coordinates
#'
#' @return a vector of polygon area
#' @export
#'
#' @examples
#' xy<-data.frame(x=c(0,1,2,1),y=c(1,2,1,0))
#' plot(xy,type="b") ; polygon(xy)
#' area(xy)
#'
area <- function(xy){
  x <- xy[,1];y <- xy[,2]
  x2 <- c(x[2:length(x)], x[1])
  y2 <- c(y[2:length(y)], y[1])

  abs(sum((x2-x)*(y+y2)/2))
}

#' correlation analysis between basal area and crown area
#'
#' @param x numeric vector of crown area
#' @param y numeric vector of basal area
#' @param main character for title name of plot
#' @param ylab character for y label
#' @param xlab character for x label
#' @param Fx a value of horizontal position of  text "y = a x"
#' @param Fy  a value of vertical position of  text "y = a x"
#' @param Rx a value of horizontal position of  text "Adj. R^2 (P)"
#' @param Ry a value of vertical position of  text "Adj. R^2 (P)"
#'
#' @return results of regression
#' @export
#'
#' @examples
#' d <- data.frame(k123_field)
#' conif <- d$conif==1 ; ca <- d$ca ; ba <- pi*(d$dbh/200)^2
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
　return(ans)
}

# lm_xy (rename from lm_vv)####
#' statistical information of regression analysis (lm y=ax)
#'
#' (rename from lm_vv)
#' regression of intercept = 0
#'
#' @param x vector of variable
#' @param y vector of variable
#'
#' @return data frame of statistical information
#' @export
#'
#' @examples
#' slotNames(TV2)
#' x<-TV2@conifer_vv$v.
#' y<-TV2@conifer_vv$v_
#' plot(x,y)
#' lm_xy(x,y)
#' TV2@conifer_reg
#'
lm_xy <-function(x,y){
  lm.<-summary(lm(y~x+0))
  lm.. <- data.frame(
    lm=as.character(lm.[[1]])[2],
    n=length(x),
    lm_a=lm.$coefficients[[1]],
    lm_b=lm.$coefficients[[4]],
    lm_adj.r.squared=lm.$adj.r.squared,
    lm_se=lm.$sigma
  )
  return(lm..)
}

# 樹冠面積　⇒　胸高直径  #####
#' estimates basal area from crown area  (all tree spcies)
#'
#' @param ca vector of crown area
#'
#' @return vector of basal area
#' @export
#'
#' @examples
#'
#'
#'  ca.<- 10:300
#'  plot(ca.,ca2ba(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Basal Area (m*m)")
#'
ca2ba       <- function(ca){
  return(0.0044189*ca)
}

#' estimates basal area from crown area  (broad leaved tree spcies)
#'
#' @param ca vector of crown area  (broad leaved tree spcies)
#'
#' @return vector of basal area  (broad leaved tree spcies)
#' @export
#'
#' @examples
#'  ca.<- 10:300
#'  plot(ca.,ca2ba_broad(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Basal Area (m*m)",
#'  main="Broadleaved")
#'
ca2ba_broad <- function(ca){
  return(0.002090*ca)
}

#' estimates basal area from crown area  (conifer)
#'
#' @param ca vector of crown area  (conifer)
#'
#' @return vector of basal area  (conifer)
#' @export
#'
#' @examples
#'  ca.<- 10:300
#'  plot(ca.,ca2ba_conif(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Basal Area (m*m)",
#'  main="Conifer")
ca2ba_conif <- function(ca){
  return(0.0075718*ca)
}


#' estimates DBH from crown area
#'
#' @param ca vector of crown area
#'
#' @return vector of DBH (diameter at breast height)
#' @export
#'
#' @examples
#'  ca.<- 10:100
#'  plot(ca.,ca2dbh(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Diameter at Breast Height (cm)")
#'
#'
#'
ca2dbh	　　　 <- function(ca){
  return(2*sqrt(0.0044189*ca/pi)*100)
}

#' estimates DBH from crown area (broadleaved)
#'
#' @param ca vector of crown area
#'
#' @return vector of DBH (diameter at breast height)
#' @export
#'
#' @examples
#'  ca.<- 10:100
#'  plot(ca.,ca2dbh(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Diameter at Breast Height (cm)",
#'  main="Broadleaved")
ca2dbh_broad <- function(ca){
  return(2*sqrt(0.002090*ca/pi)*100)
}

#' estimates DBH from crown area (conifer)
#'
#' @param ca
#'
#' @return DBH (conifer)
#' @export
#'
#' @examples
#'   ca.<- 10:100
#'  plot(ca.,ca2dbh(ca.),
#'  xlab="Crown Area (m*m)",
#'  ylab="Diameter at Breast Height (cm)",
#'  main="Conifer")
#'
ca2dbh_conif <- function(ca){
  return(2*sqrt(0.0075718*ca/pi)*100)
}

if(0){

  ca2dbh_broad(100)
  ca2dbh_conif(100)

}

#' Calculate DBH from Basal Area
#'
#' @param ba vector of basal area
#'
#' @return vector of DBH
#' @export
#'
#' @examples
#' ba2dbh(0.5)   # BA unit  m*m
#' ba. <- seq(0.1,1,0.01)
#' plot(ba.,ba2dbh(ba.),
#' xlab="Basal area (m*m)",
#' ylab="DBH (cm)")
#'
#'
ba2dbh<-function(ba){
  return(100*2*sqrt(ba/pi))
}

# 胸高直径―樹高　####
#' figure of ralatinshps between DBH (cm)and Tree Height (m) for k123
#'
#' @return only plot
#' @export
#'
#' @examples
#' P <- data.frame(k123_field)
#' DBHTreeHeightPlots123(P)
DBHTreeHeightPlots123<-function(P){
  par(mfrow=c(1,1))
  conif<-P$conif==1
  plot(P$dbh[!conif],P$h_drone[!conif],xlab="DBH (cm)",ylab="Tree Height (m)",xlim=c(0,110),ylim=c(0,50))
  points(P$dbh[conif],P$h_drone[conif],col="red",pch=2)
  legend(0,50,c("broad leaved","conifer"),col=c("black","red"),pch=c(1,2))
}

#' Relationship between DBH and tree height (Chapman-Richards)
#'
#' Tree height-diameter allometry of 75 major tree species in japan.
#' Kobayashi et al. 2021
#'
#' @param dbh vector of diameter at breast height (DBH : cm)
#' @param a   default a=22.5
#' @param b   default b=0.046
#' @param c   default c=1.52
#'
#' @return   vector of tree height (m)
#' @export
#'
#' @examples
#' dbh <- 1:100
#' plot(dbh,TreeHeight_CR(dbh,a=22.5,b=0.046,c=1.52),
#' xlab="DBH (cm)",ylab="Tree height (m) " ,
#' main="Relationship between DBH and tree height (Chapman-Richards model)",
#' sub="Kobayashi et al. 2021, Abies firma ")
#' P <- data.frame(k123_field)
#' DBHTreeHeightPlots123(P)
#'legend(82,15,c("all species","broad leaved","conifer"),
#'       col=c("blue","black","red"),lty=c(1,2,2),lwd=c(2,2,2))
#'
#'dbh. <- 1:100
#'lines(dbh.,TreeHeight_CR(dbh.,a=36.17,b=0.02888,c=0.997),col="blue",lwd=3) # all species
#'lines(dbh.,TreeHeight_CR(dbh.,a=33.37,b=0.03461,c=1.006751),col="black",lwd=2,lty=2) # broadleaved
#'lines(dbh.,TreeHeight_CR(dbh.,a =41.26,b =0.02189,c=1.007),col="red",lwd=2,lty=2) # conifere
#'
TreeHeight_CR <- function(dbh,a=22.5,b=0.046,c=1.52){
  return(1.3+a*((1-exp(-b*dbh)^c)))
}



#' conifer tree height estimate from DBH (diameter at brest height)
#'
#' @param dbh
#'
#' @return tree height
#' @export
#'
#' @examples
#' dbh <- 1:100
#' h_conif <- TreeHeight_conif(dbh)
#'
#' plot(dbh,h_conif,type="l",xlab="DBH (cm)",ylab="Tree Height (m)",
#' main="Conifer")
TreeHeight_conif<-function(dbh){
  return(1/(1/(0.4303*(dbh^1.2620))+1/39.4))
}


#' broadleaved tree height estimate from DBH (diameter at brest height)
#'
#' @param dbh
#'
#' @return tree height
#' @export
#'
#' @examples
#' dbh <- 1:100
#' h_broad <- TreeHeight_broad(dbh)
#' h_conif <- TreeHeight_conif(dbh)
#'
#' plot(dbh,h_conif,type="l",xlab="DBH (cm)",ylab="Tree Height (m)")
#' lines(dbh,h_broad,lty=2,col="green")
#' legend(0,30,
#' c("Coniferous tree species","Broadleaved tree species"),
#' col=c("black","green"),lty=c(1,2))
TreeHeight_broad<-function(dbh){
  return(1/(1/(1.4357*(dbh^0.9276))+1/39.9))
}


#' calculate tree height from species and dbh using with function (Kato)
#'
#' @param conif_sp  character vector of conifer specie list
#' @param sp  character vector of  species to calculate tree height
#' @param dbh vector of dbh (cm)
#'
#' @return    vector of tree height
#' @export
#'
#' @examples
#' TreeHeight(c("スギ"),c("スギ","ヒノキ","ブナ"),c(50,30,80))
#'
TreeHeight <- function(conif_sp,sp,dbh){
  h=ifelse(is.element(sp,conif_sp),TreeHeight_conif(dbh),
           TreeHeight_broad(dbh))
  return(h)
}

# 胸高直径-樹冠面積 ####


# estimation from crown area and tree height ####
#' Estimate DBH (diameter at breast height) from crown area and tree height
#'
#'  "dbh = a * ca ^b * h^c"
#'  DBH_cah_abc$all,$conifer,$broadleaved
#' @param ca vector of  crown area
#' @param h  vector of
#' @param abc  a vevtor of parameters abc ,default "all species"
#'
#' @return vector of DBH
#' @export
#'
#' @examples
#' p<-subset(k123_field,vital>0 & crown==1)
#' ca<-p$ca ; h<- p$h ; dbh <- p$dbh ; conif <- p$conif==1
#' DBH_cah_abc
#'
#' par(mfrow=c(1,3))
#' i<-conif
#' # all tree species ####
#' dbh. <-  DBH_cah(ca,h)
#' plot(dbh,dbh.,xlab="DBH (field)",ylab="DBH_ca_h",main="All species")
#' points(dbh[i],dbh.[i],col="red")
#' (lm.all <- summary(lm(dbh~dbh.-1)))
#' lines(c(0,120),c(0,120))
#'
#' # Conifer ####
#' dbh. <-  DBH_cah(ca[conif],h[conif],abc=DBH_cah_abc$conifer)
#' plot(dbh[i],dbh.,xlab="DBH (field)",ylab="DBH_ca_h",main="Conifer",col="red")
#'(lm.conif <- summary(lm(dbh[i]~dbh.-1)))
#'lines(c(0,120),c(0,120))
#'
#'# Broadleaved ####
#' dbh. <-  DBH_cah(ca[!i],h[!i],abc=DBH_cah_abc$broadleaved)
#' plot(dbh[!i],dbh.,xlab="DBH (field)",ylab="DBH_ca_h",main="Broadleaved")
#' (lm.broad <-summary(lm(dbh[!i]~dbh.-1)))
#' lines(c(0,120),c(0,120))
#' # statistical table ####
#' adj.r.squared <- c(lm.all$adj.r.squared,lm.broad$adj.r.squared,lm.conif$adj.r.squared)
#' P <- c(lm.all$coefficients[, "Pr(>|t|)"],lm.broad$coefficients[, "Pr(>|t|)"],lm.conif$coefficients[, "Pr(>|t|)"])
#' data.frame(t(DBH_cah_abc),adj.r.squared,P)
#'
DBH_cah<-function(ca,h, abc=DBH_cah_abc$all){
  return(abc[1]*ca^abc[2]*h^abc[3])　　
}


# 材積式　####
# TV class for TreeTrunk_cah ####
#' S4 class object for tree trunk volume regression analysis
#'
#' Vrin: Tree trunk volume calculated from DBH and tree height with Japanese Rinyacho Zaisekishiki (Table of tree trunk volume),
#' Vcah: Tree trunk volume calculated from crown area and tree height.
#' The class object "TrunkVolume_cah" is used for regression analysis between Vrin and Vcah.
#'
#' @slot conifer_vv data.frame.
#' @slot conifer_reg data.frame.
#' @slot broadleaved_vv data.frame.
#' @slot broadleaved_reg data.frame.
#' @slot all_vv data.frame.
#' @slot all_reg data.frame.
#'
#' @return S4 class object of "TrunkVolume_cah"
#'
#' @export
#'
#' @examples
#' TVcah <- new("TrunkVolume_cah")
#' slotNames(TVcah)
#' TVcah
#' # save(TVcah, file = "data/TV.RData")

setClass("TrunkVolume_cah",
         slots = c(
           conifer_vv = "data.frame",
           conifer_reg = "data.frame",
           broadleaved_vv = "data.frame",
           broadleaved_reg = "data.frame",
           all_vv = "data.frame",
           all_reg = "data.frame"
         )
)

#
#' This calculates tree trunk volume from crown area and tree height
#'
#'
#' Estimation of Coniferous Standing Tree Volume Using Airborne LiDAR and Passive Optical Remote Sensing
#' (Nakai et al. 2010)
#'
#' @param ca crown area
#' @param h  tree height
#' @param abc parameters default : abc=c(4.139e-05,0.3018000,2.958000) For all tree species in Kuraiyama natural forest.
#'
#' @return trunk volume
#' @export
#'
#' @examples
#' TrunkVolume_cah(25,30,c(10^-5.19,1.19,3.19))  #Cryptomeria japonica  (Nakai et al. 2010)
#' #abc_all        <- c(4.139e-05,0.3018000,2.958000) # all tree species in Kuraiyama natural forest.
#' #abc_conifer    <- c(5.191e-04,0.5569842,2.005132) # conifer
#' #abc_broadleabed<- c(1.422e-02,0.5073300,0.879260) # broadleaved
#'  abc_all         <-c(4.147e-05,3.017e-01,2.958e+00)
#'  abc_conifer     <-c( 0.0003358,0.5014047,2.1866917)
#'  abc_broadleabed<-c(0.04063, 0.60575,0.38452)
#'  TrunkVolume_cah(15:25,20:30,abc_conifer)
#'
TrunkVolume_cah <- function(ca,h,abc=c(4.147e-05,0.3017000,2.958000)){
  #  abc <-c(a=4.147e-05,b=3.017e-01,c=2.958e+00) #all
  #  abc <-c( a=0.0003358,b=0.5014047,c=2.1866917)#conifer
  #  abc <-c(a=0.04063, b=0.60575,c=0.38452)      #broadleabed
  a=abc[1] ;  b=abc[2] ;  c=abc[3]
  v <-  a * ca^b * h ^c
  return(v)
}

#' Figure of tree trunk volumes (field (Rinya v=f(dbh,h))vs. drone(a ca^b H^c))
#'
#' This makes figure of relationships between
#' tree trunk volumes calculated from DBH measured in field survey and
#' calculated from  crown area  measured with drone ortho ("m^2")"
#' (old name of this function : ggplot_vv)
#'
#' @param vv   data.frame(v_=field, v.=drone)
#' @param lm_vv_ statistical data (lm,n ,a ,b,c,lm_a,lm_b,lm_adj.r.squared,lm_se)
#'
#' @return graphics of relationships two(field vs. drone) tree trunk volumes
#' @export
#'
#' @examples
#'g1<-TrunkVolume_cah_fig(TV2@conifer_vv,TV2@conifer_reg,"Conifer ")
#'g2<-TrunkVolume_cah_fig(TV2@broadleaved_vv,TV2@broadleaved_reg,"Broadleaved ")
#'g3<-TrunkVolume_cah_fig(TV2@all_vv,TV2@all_reg,"All tree species")
#'gridExtra::grid.arrange(g1, g2, g3,nrow=1)
TrunkVolume_cah_fig<-function(vv=vv,lm_vv_=lm_vv_,title_="conif"){
  s <-lm_vv_
  a10<-round(log(s$a,10),2) ; b2<-round(s$b,2) ;c2<-round(s$c,2)
  rr <- round(s$lm_adj.r.squared,2)
  se <- s$lm_s

  p <- ggplot(vv, aes(x=v_, y=v.))+geom_point(alpha = 1)+
    ggtitle(title_)+
    theme(plot.margin= unit(c(1, 2, 1, 1), "lines"))+
    #theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=15,hjust = 0.5))+
    labs(x = expression("calculated from DBH measured in field survey ("*m^2*")"),
         y =expression("calculated from  crown area \n measured with drone ortho ("*m^2*")"))+
    theme(axis.text.x = element_text(size = 10),axis.title.x = element_text(size =12))+
    theme(axis.text.y = element_text(size = 10),axis.title.y = element_text(size =12))+
    annotate("text", x = 2, y = 9.2,
             label =bquote(italic("V= 10"^.(a10)~"   CA "^.(b2)~"   H"^.(c2))),
             col = "red", size = 4)+
    annotate("text",x=1.5,y=8.5,label=bquote(italic("R"^2~"="~.(rr))),
             size=3,col="red")+
    annotate("text", x = 1.5, y = 7.7,
             label =bquote(italic("SE = "~.(round(se,3))))
             ,size=3,col="red")+
    geom_abline(intercept = 0, slope = s$lm_a)  +
    scale_x_continuous(breaks=seq(0,10,1),limits=c(0,10))+
    scale_y_continuous(breaks=seq(0,10,1),limits=c(0,10))
  plot(p)
}

##ヒノキ材積（岐阜地方：天然生林）####
#' Japan Rinyacho Zaisekishiki Hinoki
#'
#' @param DBH numeric vector of DBH
#' @param H numeric vector of tree heights
#'
#' @return numeric vector of tree trunk volumes
#' @export
#'
#' @examples
#'
#' DBH<-10:80
#' H <- DBH^0.8
#' TrunkVolume_hinoki(DBH,H)
TrunkVolume_hinoki<-function(DBH,H){
  v<-ifelse((6<=DBH)&(DBH<22),
         10^(-5+0.70320+1.88715*log10(DBH)+1.04190*log10(H)),
      ifelse((22<=DBH)&(DBH<32),
          10^(-5+0.92010+1.77770*log10(DBH)+0.99996*log10(H)),
      ifelse((32<=DBH)&(DBH<42),
          10^(-5+0.60072+1.95151*log10(DBH)+1.04225*log10(H)),
       ifelse(42<=DBH,
              10^(-5+0.86684+1.74574*log10(DBH)+1.09378*log10(H)),
            NA))))
  return(v)

   }


##サワラ、ヒバ材積(岐阜地方天然生林）####
#' Japan Rinyacho Zaisekishiki Sawara, Hiba
#'
#' @param DBH numeric vector of DBH
#' @param H numeric vector of tree heights
#'
#' @return numeric vector of tree trunk volumes
#' @export
#'
#' @examples
#'
#' DBH<-10:80
#' H <- DBH^0.8
#' TrunkVolume_sawara(DBH,H)
TrunkVolume_sawara<-function(DBH,H){
 v<- ifelse((6<=DBH)&(DBH<52),
      10^(-5+0.814062+1.864437*log10(DBH)+0.977728*log10(H)),
       ifelse(52<=DBH,
        10^(-5+0.971821+1.784240*log10(DBH)+0.960310*log10(H)),
                  NA)
           )
 return(v)
}

##スギ材積式（岐阜地方）####

#' Japan Rinyacho Zaisekishiki Sugi (Cryptomeria japonica)
#'
#' @param DBH numeric vector of DBH
#' @param H numeric vector of tree heights
#'
#' @return numeric vector of tree trunk volumes
#' @export
#'
#' @examples
#'
#' DBH<-10:80
#' H <- DBH^0.8
#' TrunkVolume_sugi(DBH,H)
#'
TrunkVolume_sugi<-function(DBH,H){
  v<-ifelse(0<=DBH & DBH<12,
         10^(-5+0.770734+1.967735*log10(DBH)+0.874649*log10(H)),
         ifelse(12<=DBH & DBH<32,
                10^(-5+0.734778+1.864665*log10(DBH)+1.023757*log10(H)),
                ifelse(32<=DBH & DBH<42,
                       10^(-5+0.931815+1.687367*log10(DBH)+1.079349*log10(H)),
                       ifelse(42<=DBH,
                              10^(-4+0.076451+1.728859*log10(DBH)+0.927572*log10(H)),NA
                       ))))
  return(v)
}

##モミ等、その他針葉樹材積式（岐阜地方）####
#' Japan Rinyacho Zaisekishiki other species ex. Abies
#'
#' @param DBH numeric vector of DBH
#' @param H numeric vector of tree heights
#'
#' @return numeric vector of tree trunk volumes
#' @export
#'
#' @examples
#'
#' DBH<-10:80
#' H <- DBH^0.8
#' TrunkVolume_momi(DBH,H)
#'
TrunkVolume_momi <- function(DBH,H){
 v <-  ifelse((6<=DBH)&(DBH<12),
         10^(-5+0.905547+1.953184*log10(DBH)+0.784224*log10(H)),
        ifelse((12<=DBH)&(DBH<22),
         10^(-5+0.848238+1.922623*log10(DBH)+0.883683*log10(H)),
         ifelse((22<=DBH)&(DBH<32),
          10^(-5+0.799349+1.931306*log10(DBH)+0.919360*log10(H)),
          ifelse((32<=DBH)&(DBH<52),
           10^(-5+0.807404+1.858442*log10(DBH)+1.000080*log10(H)),
           ifelse(52<=DBH,
            10^(-5+0.749519+1.816918*log10(DBH)+1.095799*log10(H)),
            NA)))))
   return(v)
  }

##広葉樹材積量（岐阜地方）####
#' Japan Rinyacho Zaisekishiki broadleaved species
#'
#' @param DBH numeric vector of DBH
#' @param H numeric vector of tree heights
#'
#' @return numeric vector of tree trunk volumes
#' @export
#'
#' @examples
#'
#' DBH<-10:80
#' H <- DBH^0.8
#' TrunkVolume_broardleaved(DBH,H)
#'
TrunkVolume_broardleaved<-function(DBH,H){
  v<-ifelse(DBH>0 & DBH<12,
         10^(-5+0.833161+1.852021*log10(DBH)+0.896175*log10(H)),
         ifelse((12<=DBH)&(DBH<22),
                10^(-5+0.633925+1.857805*log10(DBH)+1.084483*log10(H)),
                ifelse((22<=DBH)&(DBH<32),
                       10^(-5+0.467058+2.048645*log10(DBH)+1.013891*log10(H)),
                       ifelse((32<=DBH)&(DBH<62),
                              10^(-5+0.813392+1.846988*log10(DBH)+0.972756*log10(H)),
                              ifelse(62<=DBH,
                                     10^(-4+0.214444+1.585865*log10(DBH)+1.025991*log10(H)),NA
                              )))))
  return(v)
}


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
#' TrunkVolume(c("スギ","ヒノキ","アスナロ","ブナ"),c(50,30,80),c(20,15,25))
TrunkVolume <- function(sp,dbh,h){
  v=ifelse(sp=="スギ",TrunkVolume_sugi(dbh,h),
      ifelse(sp=="ヒノキ",TrunkVolume_hinoki(dbh,h),
        ifelse(sp=="サワラ",TrunkVolume_sawara(dbh,h),
          ifelse(sp=="アスナロ",TrunkVolume_sawara(dbh,h),
              TrunkVolume_broardleaved(dbh,h)))))
  return(v)
}

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
#' TrunkVolume_roman(c("sugi","hinoki","asunaro","buna"),c(50,30,80),c(20,15,25))
TrunkVolume_roman <- function(sp,dbh,h){
  v=ifelse(sp=="sugi",TrunkVolume_sugi(dbh,h),
           ifelse(sp=="hinoki",TrunkVolume_hinoki(dbh,h),
                  ifelse(sp=="sawara",TrunkVolume_sawara(dbh,h),
                         ifelse(sp=="asunaro",TrunkVolume_sawara(dbh,h),
                                TrunkVolume_broardleaved(dbh,h)))))
  return(v)
}

# TrunkVolume_cah_model.R ####

#' Estimate parameters for function of tree trunk volume with crown area and tree height
#'
#' TrunkVolume = a * CrownArea^b * TreeHeight^c
#'
#' @param d data.frame
#'
#' @return TV list of nls results (TV$all, TV$conifer, TV$broadleaved)
#' @export
#'
#' @examples
#' # data set from  k123_field ####
#' d <- k123_field
#' sp <- d$sp ; dbh <- d$dbh ;  h <- d$h ;  ca <- d$ca
#' conif <- d$conif==1 ; crown <- d$crown==1
#' v <-TrunkVolume_roman(sp,dbh,h)
#' d <- data.frame(sp,dbh,h,ca,v,conif,crown)
#' d <- na.omit(d)
#' head(d)
#' # all tree species ####
#'
#' all <- TrunkVolume_cah_model(d)
#' summary(all)
#'
#' # coniferous species ####
#' conifer <- TrunkVolume_cah_model(subset(d,conif))
#' summary(conifer)
#'
#' # broadleaved tree species ####
#' broadleaved <- TrunkVolume_cah_model(subset(d,!conif),a=0.01136,b=0.65086,c=0.78120)
#' summary(broadleaved)
#' TV <- list(all=all,conifer=conifer,broadleaved=broadleaved)
#' TV
#' # save(TV,file="TrunkVolume_cah_TV.RData")
#' # load("TrunkVolume_cah_TV.RData")
#' l <- lapply(TV,nls_model_summary)
#'
#' df<-data.frame(n=rbind(l$all$n,l$conifer$n,l$broadleaved$n),row.names=c("all","conifer","bloadleaved"))
#' df<-data.frame(df,rbind(l$all$coef,l$conifer$coef,l$broadleaved$coef))
#' df<-data.frame(df,rbind(l$all$ast,l$conifer$ast,l$broadleaved$ast),rbind(l$all$P,l$conifer$P,l$broadleaved$P))
#' df<-data.frame(df,ResidualStandardError=rbind(l$all$se,l$conifer$se,l$broadleaved$se))
#' df<-data.frame(df,Adjusted_R_squared=rbind(l$all$adjR2,l$conifer$adjR2,l$broadleaved$adjR2))
#' df
#' # save(df,file="TrunkVolume_cah_df.RData")
TrunkVolume_cah_model　<-function(d,a = 4.147e-05, b = 0.3017, c = 2.958){
  # caret用のカスタムトレーニング関数の定義
  nlsFit <- function(x, y, wts, param, lev, last, classProbs, ...) {
    df <- data.frame(v = y, ca = x[,1], h = x[,2])
    mod <- nls(v ~ a * ca^b * h^c, data = df, start = list(a = param$a, b = param$b, c = param$c))
    mod
  }

  # 予測関数の定義
  nlsPredict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata = data.frame(ca = newdata[,1], h = newdata[,2]))
  }

  # モデルの格納
  nlsModel <- list(
    type = "Regression",
    library = NULL,
    loop = NULL,
    parameters = data.frame(parameter = c("a", "b", "c"),
                            class = rep("numeric", 3),
                            label = c("a", "b", "c")),
    grid = function(x, y, len = NULL, search = "grid") {},
    fit = nlsFit,
    predict = nlsPredict,
    prob = NULL,
    varImp = NULL
  )


  # トレーニングデータとテストデータに分割
  set.seed(123)
  trainIndex <- createDataPartition(d$v, p = .8, list = FALSE)
  trainData <- d[trainIndex,]
  testData  <- d[-trainIndex,]

  # k分割交差検証の設定
  set.seed(123)
  train_control <- trainControl(method = "cv", number = 10)

  # モデルのトレーニング
  tunedParams <- expand.grid(a = a, b = b, c = c)
  nls_fit <- train(
    x = trainData[, c("ca", "h")],
    y = trainData$v,
    method = nlsModel,
    trControl = train_control,
    tuneGrid = tunedParams
  )

  # モデルの結果を表示
  #print(nls_fit)


  # クロスバリデーションで得られた最適なパラメータを使用して、全データで最終モデルをフィッティング
  final_model <- nls(v ~ a * ca^b * h^c, data = d, start = list(a = a, b = b, c = c))

  # 最終モデルの要約を表示
  #summary(final_model)
  return(final_model)
}

#' summarize of nls(Nonlinear Least Squares) result
#'
#' @param nls_res nls(Nonlinear Least Squares) result
#'
#' @return data frame of summarized nls result
#'
#' @export
#'
#' @examples
#' nls_model_summary(TV$all)
#' nls_model_summary(TV$conifer)
#' nls_model_summary(TV$broadleaved)
nls_model_summary <- function(nls_res=TV$all){
  # パラメーターの推定値と標準誤差の抽出
  model_summary <- summary(nls_res)



  # パラメーターの推定値
  params <- coef(model_summary)
  # 標準誤差
  stderr <- coef(model_summary, se = TRUE)[, "Std. Error"]

  # t値
  tvalues <- params / stderr

  # p値の計算
  pvalues <- 2 * pt(abs(tvalues), df.residual(nls_res), lower.tail = FALSE)

  # 結果をデータフレームにまとめる
  result <- data.frame(
    #Parameter = names(params),
    Estimate = params,
    Std.Error = stderr,
    t.value = tvalues,
    p.value = pvalues
  )
  result <- data.frame(
    #Parameter = names(params),
    params,
    stderr,
    tvalues,
    pvalues
  )

  # return(result)

  residual_standard_error <- model_summary$sigma
  # 調整済み決定係数の計算
  residuals <- resid(nls_res)
  sse <- sum(residuals^2)  # 残差平方和 (Sum of Squared Errors)
  sst <- sum((d$v - mean(d$v))^2)  # 全平方和 (Total Sum of Squares)
  r_squared <- 1 - (sse / sst)  # 決定係数 (R-squared)
  n <- length(residuals)  # 観測数
  p <- length(coef(nls_res))  # パラメーター数
  adjusted_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))  # 調整済み決定係数

  # summary list
  coef_summary <- model_summary$coefficients
  n <- length(predict(nls_res))
  coef <- coef_summary[,"Estimate"]
  P <- coef_summary[,"Pr(>|t|)"]
  ast <- sapply(P,p_value_to_asterisk )
  se <- residual_standard_error
  adjR2 <- adjusted_r_squared
  l <- list(n=n,coef=coef,P=P,ast=ast,se=se,adjR2=adjR2)

  return(l)

}


# p値をアスタリスクに変換する関数 ####
#
#' p_value_to_asterisk
#'
#' @param a P values
#'
#' @return strings P<0.05 "*", 0.01 "**", 0.001 "***"
#' @export
#'
#' @examples
#' p<-c(5.588200e-03,3.021976e-26,1.196543e-97,0.2,0.05,0.01 )
#' sapply(p,p_value_to_asterisk )

p_value_to_asterisk <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}





# 収量密度図　####


#' YN plot
#'
#' @param v volume of trees
#' @param a area of plot
#' @return data frame of yn (number of trees ordered with size , yield of cumulative volume)
#' @export
#'
#' @examples
#' #### simple example
#'
#' v=1:100  ; a=10000
#' plot(yield_density(v,a))
#'
#' #### example of Kuraiyama K123
#'
#' pn <- 2 # plot number
#' d<-subset(k123_field,plot==pn)
#' a<-k123_site$area[pn]
#' v<-TrunkVolume(d$sp,d$dbh,d$h)
#' NY <- yield_density(v,a)
#' plot(NY,
#'  xlab="N",
#'  ylab="Cummulative trunk volume(m^3/ha)",
#'  main=paste("Site :",pn))
#'
yield_density<-function(v,a){
  v<-v[order(-v)]
  vv<-cumsum(v)*10000/a
  nn<-1:length(v)*10000/a
  return(data.frame(n=nn,v=vv))
}








