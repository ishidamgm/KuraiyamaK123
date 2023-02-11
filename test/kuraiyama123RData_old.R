#' read shapefile points
#'
#' @param filename
#'
#' @return data frame of shapefile points
#' @export
#'
#' @examples
#' #setwd("../tree-point")
#'k1<- ReadShapefile_Points("k1_2022")
#'k2<-　ReadShapefile_Points("k2_2022_2")
#'k3<-  ReadShapefile_Points("k3_2022")
#'k123_points<-list(k1=k1,k2=k2,K3=k3)
#'save(k123_points,file="k123_points.RData")
ReadShapefile_Points <-function(filename="k2_2022_2"){


  d<-sf::st_read(".",filename)
  return(data.frame(d))
  ############################################################
  if(0){d<-shapefiles::read.shapefile(filename)　
  shp<-d$shp$shp;
  dbf<-d$dbf$dbf;	names(dbf)
  d<-data.frame(dbf,shp)
  d$sp<-iconv(d$sp, from = "shift-jis", to = "utf8")
  }
}


#' read shapefile of crown polygon
#'
#' @param filename
#' @param ring.tag
#'
#' @return
#' @export
#'
#' @examples
#'  #setwd("../crown_polygon")
#'fn <- c("polygon_k1_2022","polygon_k2_2022_2","polygon_k1_2022")
#'
#' require(rgdal)
#' dir()
# shape <- readOGR(dsn = ".", layer = "k1_2022")
#'
#'
ReadShapefile_CrownPolygon <-function(filename="polygon_k1_2022",ring.tag="id"){
  d<-st_read(dsn = ".", layer = filename)  #,options = "ENCODING=CP932")

  d<-read_sf(dsn = ".", layer = filename)
  d
  plot(d)
  data.frame(d)$geometry

  d<-shapefiles::read.shapefile(filename)  	### ライン・データの読み込み , encoding = "SHIFT-JIS"
  ring<-as.numeric(as.vector(d$dbf$dbf[,ring.tag]))		###　年輪ポリゴン年数  #!!! 朝原さん用 $id

  # 入力順に格納されているのを小さい順に直す　jjjはその順番 #
  jjj <- order(ring)
  (ln<-length(d$shp$shp))
  L<-c()
  for (i in 1:ln)L<-c(L,list(as.matrix(d$shp$shp[[jjj[i]]]$points))) #### Lの追加

  (YR_L<-c(as.numeric(as.vector(d$dbf$dbf[,ring.tag])))[jjj])

  names(L)<-YR_L #　年輪数の名前を追加 #
  return(L)
}
