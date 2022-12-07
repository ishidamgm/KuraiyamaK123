# calculation.R

#### 多角形の面積を求める関数　
#' area return a  area from polygon xy coordinates
#'
#' @param xy a atrix or data frame of xy coordinates
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

# 樹冠面積　⇒　胸高直径  #####
ca2ba       <- function(ca){
  return(0.0044189*ca)
}
ca2ba_broad <- function(ca){
  return(0.002090*ca)
}

ca2ba_conif <- function(ca){
  return(0.0075718*ca)
}


#' Title
#'
#' @param ca
#'
#' @return
#' @export
#'
#' @examples
#'  ca2dbh(100)
#'
ca2dbh	　　　 <- function(ca){
  return(2*sqrt(0.0044189*ca/pi)*100)
}

ca2dbh_broad <- function(ca){
  return(2*sqrt(0.002090*ca/pi)*100)
}

ca2dbh_conif <- function(ca){
  return(2*sqrt(0.0075718*ca/pi)*100)
}

if(0){

  ca2dbh_broad(100)
  ca2dbh_conif(100)

}

ba2dbh<-function(ba){
  return(100*2*sqrt(ba/pi))
}
# 胸高直径―樹高　####

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
#' plot(dbh,h_conif,type="l",xlab="DBH (cm)",ylab="Tree Height (m)")
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
#' Japan Rinyacho Zaisekishiki bloadleaved species
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
#' TrunkVolume_bloardleaved(DBH,H)
#'
TrunkVolume_bloardleaved<-function(DBH,H){
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



# 収量密度図　####
#' Title
#'
#' @param dbh_
#'
#' @return
#' @export
#'
#' @examples
dbh.points<-function(dbh_){ #dbh_<-ba2dbh(ba)  #ordered
  dbh_cls<-seq(120,0,-10)
  dbh_cls_n<-rep(NA,length(dbh_cls))
  for (i in 1:length(dbh_cls)){dbh_cls_n[i]<-rev(which(dbh_>dbh_cls[i]))[1]}
  points(nn[dbh_cls_n],vv[dbh_cls_n])
  text(nn[dbh_cls_n],vv[dbh_cls_n],dbh_cls)
}

#' Title
#'
#' @param v volume of trees
#' @param a area of plot
#' @return data frame of yn (number of trees ordered with size , yield of cumulative volume)
#' @export
#'
#' @examples
#' pn <- 1
#' d <- k123[[pn]]
#' v <- 0.5 * d$dbh * d$h
#' a<-as.numeric(st_area(k123plot[[pn]]))
#'
#' plot(yield_density(v,a))
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
#' mtime_stamp(dir())
mtime_stamp <- function(fn){
  mt <- file.info(fn)$mtime
  return(gsub("[^0-9]","",mt))
}





