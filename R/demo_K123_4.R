
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








