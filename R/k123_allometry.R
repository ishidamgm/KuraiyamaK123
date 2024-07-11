# k123_allometry.R

library(kuraiyamaK123)

#' figure of ralatinshps between DBH (cm)and Tree Height (m) for k123
#'
#' @return
#' @export
#'
#' @examples
#' G()
G<-function(){
  par(mfrow=c(1,1))
  plot(P$dbh[!conif],P$h[!conif],xlab="DBH (cm)",ylab="Tree Height (m)",xlim=c(0,110),ylim=c(0,50))
  points(P$dbh[conif],P$h[conif],col="red",pch=2)
  legend(60,15,c("broad leaved","conifer"),col=c("black","red"),pch=c(1,2))
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
#' G()
#'legend(82,15,c("all species","broad leaved","conifer"),
#'       col=c("blue","black","red"),lty=c(1,2,2),lwd=c(2,2,2))
#'
#'dbh. <- 1:100
#'lines(dbh.,TreeHeight_CR(dbh.,a=36.17,b=0.02888,c=0.997),col="blue",lwd=3) # all species
#'lines(dbh.,TreeHeight_CR(dbh.,a=33.37,b=0.03461,c=1.006751),col="black",lwd=2,lty=2) # broadleaved
#'lines(dbh.,TreeHeight_CR(dbh.,a =41.26,b =0.02189,c=1.007),col="red",lwd=2,lty=2) # conifere
TreeHeight_CR <- function(dbh,a=22.5,b=0.046,c=1.52){
  return(1.3+a*((1-exp(-b*dbh)^c)))
}


#' Estimate DBH (diameter at breast height) from crown area and tree height
#'
#'  "dbh = a * ca ^b * h^c"
#'   all species   a=31.21470 ; b= 0.02329  ; c=0.42632
#'   conifer      a=30.53526  ; b= 0.04826 ; c=0.37625
#    broadleaved a=10.9143  ; b= 0.3209 ; c=0.1056
#'
#' @param ca vector of  crown area
#' @param h  vector of
#' @param a  a numeric parameter of a ,default "all species"
#' @param b b numeric parameter of a  ,default "all species"
#' @param c c numeric parameter of a  ,default "all species"
#'
#' @return vector of DBH
#' @export
#'
#' @examples
#' p<-subset(PP9,vital>0 & crown)
#' ca<-p$a ; h<- p$v_cah3 ; dbh <- p$dbh
#' i<-p$conif
#'
#' par(mfrow=c(1,3))
#' # all tree species ####
#' dbh. <-  DBH_cah(ca,h)
#' plot(dbh,dbh.,xlab="DBH (field)",ylab="DBH=31.21470*ca^0.02329*h^0.42632",main="All species    adj.R^2  0.9524 ")
#' points(dbh[i],dbh.[i],col="red")
#'summary(lm(dbh~dbh.-1))
#' lines(c(0,120),c(0,120))
#'
#' # Conifer ####
#' dbh. <-  DBH_cah_conifer(ca[i],h[i])
#' plot(dbh[i],dbh.,xlab="DBH (field)",ylab="DBH=30.53526*ca^0.04826*h^0.42632",main="Conifer   adj.R^2  0.9583 ",col="red")
#'summary(lm(dbh[i]~dbh.-1))
#'lines(c(0,120),c(0,120))
#'
#'# Bloadleaved ####
#' dbh. <-  DBH_cah_broadleaved(ca[!i],h[!i])
#' plot(dbh[!i],dbh.,xlab="DBH (field)",ylab="DBH=10.9143*ca^0.3209*h^0.1056",main="Bloadleaved    adj.R^2  0.9397 ")
#'summary(lm(dbh[!i]~dbh.-1))
#' lines(c(0,120),c(0,120))
#'
DBH_cah<-function(ca,h, a=31.21470 ,b= 0.02329,c=0.42632){
  # conifer      a=30.53526,b= 0.04826,c=0.37625
  # broadleaved a=10.9143,b= 0.3209,c=0.1056
  return(a*ca^b*h^c)　　
}

DBH_cah_conifer<-function(ca,h, a=30.53526,b= 0.04826,c=0.37625){
  # conifer      a=30.53526  ; b= 0.04826 ; c=0.37625
  # broadleaved a=10.9143  ; b= 0.3209 ; c=0.1056
  return(a*ca^b*h^c)　　
}

DBH_cah_broadleaved<-function(ca,h, a=10.9143,b= 0.3209,c=0.1056){
  # conifer      a=30.53526  ; b= 0.04826 ; c=0.37625
  # broadleaved a=10.9143  ; b= 0.3209 ; c=0.1056
  return(a*ca^b*h^c)　　
}

