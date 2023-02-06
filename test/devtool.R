#https://github.com/ishidamgm/KuraiyamaK123
# rm(list=ls())
library(devtools)
devtools::install_github("ishidamgm/kuraiyamaK123")
devtools::install_github("ishidamgm/nenrin")
library(kuraiyamaK123)
help(package="kuraiyamaK123")
data(package = "kuraiyamaK123")
data("kuraiyamaK123")
d
d2
edit(d3)



# data check ####
dir000 <- getwd()
setwd(paste0(.libPaths()[2],"/kuraiyamaK123/data/"))
file.info(dir())
setwd(dir000)

#
demo_plot_K123
k123plot
