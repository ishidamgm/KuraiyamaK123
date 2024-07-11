# _k123_source.R ####

#' Correlation test y=ax
#'
#' @param v.  experimental variables
#' @param v_  objective variables
#'
#' @return  regression analysis coefficients
#' @export
#'
#' @examples
#'
#' x<-1:10
#' y<- jitter(1:10)
#' plot(x,y)
#' lm_xy(x,y)
#'
lm_xy <-function(v.,v_){
  (lm.<-summary(lm(v_~v.+0)))
  lm.. <- data.frame(
    lm=as.character(lm.[[1]])[2],
    n=length(v_),
    lm_a=lm.$coefficients[[1]],
    lm_b=lm.$coefficients[[4]],
    lm_adj.r.squared=lm.$adj.r.squared,
    lm_se=lm.$sigma
  )
  return(lm.. )
}

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
#' # old #
#' #abc_all        <- c(4.139e-05,0.3018000,2.958000) # all tree species in Kuraiyama natural forest.
#' #abc_conifer    <- c(5.191e-04,0.5569842,2.005132) # conifer
#' #abc_broadleabed<- c(1.422e-02,0.5073300,0.879260) # broadleaved
#' # new #
#'  abc_all         <-c(a=4.147e-05,b=3.017e-01,c=2.958e+00)
#'  abc_conifer     <-c( a=0.0003358,b=0.5014047,c=2.1866917)
#'  abc_broadleabed<-c(a=0.04063, b=0.60575,c=0.38452)
#'
TrunkVolume_cah <- function(ca,h,abc=c(4.147e-05,0.3017000,2.958000)){
  #  abc <-c(a=4.147e-05,b=3.017e-01,c=2.958e+00) #all
  #  abc <-c( a=0.0003358,b=0.5014047,c=2.1866917)#conifer
  #  abc <-c(a=0.04063, b=0.60575,c=0.38452)      #broadleabed
  a=abc[1] ;  b=abc[2] ;  c=abc[3]
  v <-  a * ca^b * h ^c
  return(v)
}
