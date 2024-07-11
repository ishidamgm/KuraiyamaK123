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

#' statistical information of regression analysis (lm y=ax)
#'
#' regression of intercept = 0
#'
#' @param v. vector of variable
#' @param v_ vector of variable
#'
#' @return data frame of statistical information
#' @export
#'
#' @examples
lm_vv <-function(v.,v_){
  (lm.<-summary(lm(v_~v.+0)))
  lm.. <- data.frame(
    lm=as.character(lm.[[1]])[2],
    n=length(v_),
    a=a,b=b,c=c,
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
#' @param ca
#'
#' @return
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
#' @param ca
#'
#' @return
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
#' @param ca
#'
#' @return
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
#' @param ca
#'
#' @return
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
#' @param ca
#'
#' @return
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
#' @return
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
#' @param ba
#'
#' @return
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
#' @return
#' @export
#'
#' @examples
#' G(PP2)
G<-function(P){
  par(mfrow=c(1,1))
  conif<-P$conif
  plot(P$dbh[!conif],P$h_drn[!conif],xlab="DBH (cm)",ylab="Tree Height (m)",xlim=c(0,110),ylim=c(0,50))
  points(P$dbh[conif],P$h_drn[conif],col="red",pch=2)
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
#'
#' G(P)
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

# 材積式　####

#
#' This calculates tree trun volume from crown area and tree height
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


##サワラ、ヒバ材積(岐阜地方天然生林）##
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

##スギ材積式（岐阜地方）##

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

##広葉樹材積量（岐阜地方）##
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



# 収量密度図　####


#' Title
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
#' d<-subset(K,plt==pn)
#' a<-as.numeric(st_area(k123plot[[pn]]))
#' v<-TrunkVolume(d$sp,d$dbh,d$h)
#' NY <- yield_density(v,a)
#' plot(NY,
#'  xlab="N",
#'  ylab="Cummulative trunk volume(m^3/ha)")
#'
yield_density<-function(v,a){
  v<-v[order(-v)]
  vv<-cumsum(v)*10000/a
  nn<-1:length(v)*10000/a
  return(data.frame(n=nn,v=vv))
}


# Data handling ####

#' return list from dara frame
#'
#' @param d3  data frame
#' @param index characters,a name of index  in data frame columns
#' @param col vector of characters, data frame column names to include in list created
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
#' tmp. <- sapply(L,polygon)
#'
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




#' mtime_stamp
#'
#' @param fn vector of file names
#'
#' @return vector of mtime stamp of files
#' @export
#'
#' @examples
#' filename <- dir()
#' data.frame(filename,mtime=mtime_stamp(filename))
mtime_stamp <- function(fn){
  mt <- file.info(fn)$mtime
  return(gsub("[^0-9]","",mt))
}

#' All file path in working directory
#'
#' @param wd working directory
#'
#' @return data frame of full path, time , size of all files in working directory
#' @export
#'
#' @examples
#'
#' fs::dir_tree(path =getwd(), recurse = TRUE,type="dir")
#' fullpath()
fullpath <- function(wd=getwd()){
  hd<-getwd()
  setwd(wd)
  f<-dir(recursive =TRUE)
  f.<-sapply(f,function(s)rev(unlist(strsplit(s,"/")))[1],USE.NAMES =FALSE)
  time = file.mtime(f) ; size=file.size(f)
  files<-data.frame(path=f,file=f.,time,size)
  setwd(hd)
  return(files)
}

#' return file paths including a keyword
#'
#' @param ff
#' @param keyword
#'
#' @return
#' @export
#'
#' @examples
#' (ff<-fullpath())
#' file_search(ff,"k123")
file_search <- function(ff,keyword){
  (d. <-ff[ grep(keyword,ff$path),])
  return((d.<-d.[order(d.$path,d.$time,d.$size),]))
}

#' return data frame of no. and file path  with a keyword
#'
#' @param paths vector of file names (full path)
#' @param text character of keyword
#'
#' @return data frame of no. and file path  with a keyword in paths
#' @export
#'
#' @examples
#'
#' (f<-dir(pattern="\\.R$",recursive =TRUE) ) #
#' keyword <- "ForestTools"    # "library\\(sf"
#' (f. <- files_keyword (f,keyword ))
#' i<-11 ;  readLines(f[i]); grep(keyword ,readLines(f[i]))
#' edit(file.info(f.)$file)
files_keyword <- function(f, keyword="data")
{
  dat<-c()
  for (i in 1:length(f)){
    l<-grep(keyword,readLines(f[i]))
    if(length(l)!=0)  dat<-rbind(dat,c(i,f[i]))
  }
  dat <- data.frame(no=dat[,1],file=dat[,2])
  return(dat)
}




