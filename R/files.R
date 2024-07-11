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

#' All file path in working directory
#'
#' @param wd working directory
#'
#' @return data frame of full path, time , size of all files in working directory
#' @export
#'
#' @examples
#'
#' fs::dir_tree(path =getwd(), recurse = TRUE,type="dir")
#' fullpath()
fullpath <- function(wd=getwd()){
  hd<-getwd()
  setwd(wd)
  f<-dir(recursive =TRUE)
  f.<-sapply(f,function(s)rev(unlist(strsplit(s,"/")))[1],USE.NAMES =FALSE)
  time = file.mtime(f) ; size=file.size(f)
  files<-data.frame(path=f,file=f.,time,size)
  setwd(hd)
  return(files)
}

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
#' keyword <- "ForestTools"    # "library\\(sf"
#' (f. <- files_keyword (f,keyword ))
#' i<-11 ;  readLines(f[i]); grep(keyword ,readLines(f[i]))
#' edit(file.info(f.)$file)
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

#' return file paths including a keyword
#'
#' @param ff
#' @param keyword
#'
#' @return
#' @export
#'
#' @examples
#' (ff<-fullpath())
#' file_search(ff,"k123")
file_search <- function(ff,keyword){
  (d. <-ff[ grep(keyword,ff$path),])
  return((d.<-d.[order(d.$path,d.$time,d.$size),]))
}
