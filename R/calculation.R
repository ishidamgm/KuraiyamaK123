# calculation.R

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
