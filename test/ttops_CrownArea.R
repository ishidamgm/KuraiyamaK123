# ttops_CrownArea.R
tt<-read.csv("../gis/csv/kuraiyama_ttops_CrownArea.csv")

iii<-1
ttop<-k123ttop[[iii]]
#plot(ttop)
names(ttop)
id.ttop<-ttop$treeID

match(id.ttop,tt$id)
length(tt$id)

# matchがない

#
library(raster)
r<-raster("../all/R/kuraiyama_crown.tif")
plot(r)
v<-values(r)

t<-table(v)
t<-data.frame(t)
head(t)

CrownArea<-t[,2]*0.5^2

id0<-as.numeric(as.vector((t[,1])))
summary(id0)
#match(id.ttop,id0) #とれる
ttops_CrownArea <- c()
for(iii in 1:3){
  id<-k123ttop[[iii]]$treeID
  i<-match(id,id0)
  ttCA<-data.frame(id,CrownArea=CrownArea[i])
  ttops_CrownArea <- c(ttops_CrownArea,list(ttCA))
}

#save(ttops_CrownArea,file="./data/K123ttops_CrownArea.RData")



