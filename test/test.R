
iii <- 1　　# k plot number　set　####
survey_area <- k123_site$area[iii]  ## fin_area_polygon.shp
d<-k123crown[[iii]]
names(d)
d<-subset(d,!is.na(dbh))
sp0<-d$species　; ca0 <-d$area ; h0 <- d$height ; lbl <- d$label
lbl_<-k123[[iii]]$label
lbl_<-gsub("[A-Z]","",lbl_)

dbh <- d$dbh
ca <- d$area
sp<-d$sp
#x <- d$X ; y <-d$Y
xy<-st_coordinates(d) ; x<-xy[,1];  y <-xy[,2]
ba <- (dbh/200)^2*pi ## ba00<-ba
v<-0.5*ba*h0

conif_sp<-c("アスナロ","スギ","サワラ")
conif<-is.element(sp,conif_sp)
sp[conif]
