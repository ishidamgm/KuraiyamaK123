# yn.R

#' calculate tree height from species and dbh using with function (Kato)
#'
#' @param sp  character vector of Japanese species name (Katakana)
#' @param dbh vector of dbh (cm)
#'
#' @return    vector of tree height
#' @export
#'
#' @examples
#' conif_sp<-c("アスナロ","スギ","サワラ")
#' TreeHeight(conif_sp,c("スギ","ヒノキ","ブナ"),c(50,30,80))
TreeHeight <- function(conif_sp,sp,dbh){
  h=ifelse(is.element(sp,conif_sp),TreeHeight_conif(dbh),
           TreeHeight_broad(dbh))
  return(h)
}
