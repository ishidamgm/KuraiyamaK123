# raster2
#
# GIS function samples


#  library(rgl,raster)
#

#' convert image matrix to raster matrix
#'
#' @param m　　"image" matrix
#'
#'
#' @return　"raster" matrix
#' @export
#'
#' @examples
#'
#' img2ras(img)
img2ras <- function(m)m[,ncol(m):1]    ####



#' ras2img
#'
#' convert from "raster" matrix to "image" matrix
#'
#' @param m "raster" matrix
#'
#' @return  "image" matrix
#' @export
#'
#' @examples
#' ras2img(ras)
ras2img <- function(m){t(m)[,nrow(m):1]}


#' xy2ij
#'
#' @param x  coordinates
#' @param y  coordinates
#' @param r  class of raster data
#'
#' @return
#' @export
#'
#' @examples
#' xy2ij(x,y,r)
xy2ij<-function(x,y,r){
  ext <- extent(r)
  x1 <- ext[1] ; x2 <- ext[2] ;y1 <- ext[3] ; y2 <- ext[4]
  dx <- x2-x1 ; dy <- y2-y1
  rn <- dim(r)[1];cn <- dim(r)[2]
  stpx_ <- dx/cn ;stpy_ <- dy/rn

  i<-floor((y2-y)/stpy_)+1 ; i[i>rn] <- rn
  j<-floor((x-x1)/stpy_)+1 ; j[j>cn] <- cn
  return(cbind(i,j))
}



#' xy2n
#' cell number from x,y
#' @param x x coordinate
#' @param y y coordinate
#' @param r raster
#'
#' @return integer, cell number
#' @export
#'
#' @examples
#' xy2n(x,y,r)
xy2n<-function(x,y,r){
  ij<-xy2ij(x,y,r)
  i <- ij[,1] ; j <- ij[,2]
  n <-ncol(r)*(i-1)+j
  return(n)
}


#' crop xyz data matrix or data frame form extent c(x1,x2,y1,y2)
#'
#' @param xyz
#' @param ext
#'
#' @return xyz data matrix or data frame
#' @export
#'
#' @examples
#' xyz_ext(dtm)
xyz_ext <- function(xyz,ext){
  x <- xyz[,1] ; y <- xyz[,2]
  i <- ext[1] <= x & x<= ext[2] & ext[3] <= y & y<= ext[2]
  return(xyz[i,])
}

#' surface3dr
#' draw surface 3d from raster data
#' @param r
#'
#' @return
#' @export
#'
#' @examples
#' r<-raster("07ke013_dchm_NA_filled.tif")
#' plot(r)
#' open3d()
#' surface3dr(r,col="green")
#
surface3dr<-function(r,...){
  ext.<-extent(r)
  rcn.<-dim(r)
  xaxis.<- seq(ext.[1],ext.[2],length=rcn.[2])
  yaxis.<- seq(ext.[3],ext.[4],length=rcn.[1])
  m<-ras2img(as.matrix(r))
  surface3d(xaxis.,yaxis.,m,...)
}


#' dsm_files
#'
#' @param ext_
#' @param files
#' @param extents
#' @param rect_drow draw the extent on map
#'
#' @return file names of dsm files including extents from horizontal coordinates
#' @export
#'
#' @examples
#' # you need files "../gis/dsm/extents_dsm.csv"
#' ext_dsm<-read.csv("../gis/dsm/extents_dsm.csv")
#' ext_ <- c(5100,5200,-1400,1100)
#' dsm_files(ext_)
dsm_files<-function(ext_=c(5100,5200,-1400,1100),files=ext_dsm$f,extents=ext_dsm[,c("x1","x2","y1","y2")],rect_drow=F){
  if(rect_drow)rect(ext_[1],ext_[3],ext_[2],ext_[4],border="red")
  x1 <- extents[,1] ; x2 <- extents[,2] ; y1 <- extents[,3] ; y2 <- extents[,4]
  i<-which( (x1<=ext_[1] & x2<=ext_[2]) & (y1<=ext_[3] & y2<=ext_[4]))
  return(ext_dsm$f[i])
}



#' Title
#'
#' @param x
#' @param y
#' @param x_axis
#' @param y_axis
#' @param m
#'
#' @return return z (ground levels) form x , y using DTM matrix
#' @export
#'
#' @examples
#'
dtm_z<-function(x,y,x_axis,y_axis,m){
  i <- round((x-x1)/xstp) ;i[i==0]<-1 #range(i)
  j <- round((y-y1)/ystp) ;j[j==0]<-1 # range(j)
  return(m[cbind(i,j)])
}




#' Title
#'
#' @param x
#' @param y
#' @param h
#' @param m
#' @param x1
#' @param xstp
#' @param y1
#' @param ystp
#'
#' @return
#' @export
#'
#' @examples
#'
xy_dchm_max <- function(x,y,h,m,x1=x1,xstp=0.5,y1=y1,ystp=0.5){
  #x=dsm$x;y=dsm$y;z=dsm$z;xstp=0.5;ystp=0.5;h=dchm$h
  nr <- nrow(m);nc <- ncol(m) #nr*nc
  m2 <- matrix(0,nrow=nr, ncol=nc)

  i <- round((x-x1)/xstp) ;i[i==0]<-1 #range(i)
  j <- round((y-y1)/ystp) ;j[j==0]<-1 # range(j)
  n <- nr*(j-1)+i#length(n)   str(h) 704226 range(n)
  maxh<-tapply(h,n,max)#
  k<-as.numeric(names(maxh))
  m2[k]<-maxh
  return(dchm_max=m2)
}



