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
#' @param v
#'
#' @return
#' @export
#'
#' @examples
yield_density<-function(v){
  v<-v[order(-v)]
  vv<-cumsum(v)*10000/a
  nn<-1:length(v)*10000/a
  return(data.frame(n=nn,v=vv))
}


# data handling ####

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
