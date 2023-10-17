#  files.R


#' return data frame of no. and file path  with a keyword
#'
#' @param paths vector of file names (full path)
#' @param text character of keyword
#'
#' @return data frame of no. and file path  with a keyword in paths
#' @export
#'
#' @examples
#'
#' (f<-dir(pattern="\\.R$",recursive =TRUE) ) #
#' keyword <- "library\\(sf"
#' files_keyword (f,keyword )
#' i<-11 ;  readLines(f[i]); grep(keyword ,readLines(f[i]))
#'
files_keyword <- function(f, keyword="data")
{
  dat<-c()
  for (i in 1:length(f)){
    l<-grep(keyword,readLines(f[i]))
    if(length(l)!=0)  dat<-rbind(dat,c(i,f[i]))
  }
  dat <- data.frame(no=dat[,1],file=dat[,2])
  return(dat)
}

#' Qgis layer name and path
#'
#' @param filename.qgs
#'
#' @return matrix of
#' @export
#'
#' @examples
#'
#' filename.qgs <- "./test/qgis/KuraiyamaK123_Qgis_2023.qgs"
#' edit(qgs_path_name(filename.qgs))
qgs_path_name <- function(filename.qgs){
  (l<-readLines(filename.qgs) )
  (q <- l[grep("<layer-tree-layer source=",l)])
  path_name <- c()
  for (i in 1:length(q)){
    path_name <- rbind(path_name,strsplit(q[i],"\"")[[1]][c(2,16)])
  }
  return(data.frame(name=path_name[,2],path=path_name[,1]))
}
