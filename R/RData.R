# RData.R

# roxigen2 documents of RData for R package : ishidamgm/KuraiyamaK123

# TreeHeight_CR_abc ####

#' Parameters of relationship between DBH and tree height (Chapman-Richards)
#'
#' H=a*(1-exp(-b*DBH))^c + 1.3 (Chapman-Richards)
#'
#'
#' @format A data frame for regression analysis
#' \describe{
#'   \item{all}{a,b,c}
#'   \item{broadleaved}{a,b,c}
#'   \item{conife}{a,b,c}

#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @seealso \link{TreeHeight_CR}
#'
"TreeHeight_CR_abc"

# TreeVolume_cah_TV2 ####

#' S4 class object for tree trunk volume regression analysis
#'
#' TrunkVolume=a*(CrownArea^b)*(TreeHeight^c)  (Nakai et al.)
#'
#' @format A S4 class object for tree trunk volume regression analysis
#' slot
#' \describe{
#'   \item{@conifer_vv}{data.frame v_: v_rinya, v.; v_cah}
#'   \item{@conifer_reg}{data.frame}
#'   \item{@broadleaved_vv}{data.frame}
#'   \item{@broadleaved_reg}{data.frame}
#'   \item{@all_vv}{data.frame}
#'   \item{@all_reg}{data.frame}
#'
#'
#'   }
#'
#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @seealso \link{TrankVolume_cah_TV2}
#'
#' @examples
#' plot(TV2@conifer_vv)
#' TV2@conifer_reg
"TV2"

# TreeVolume_cah_TV ####

#' list for tree trunk volume estimation model from crown area and tree height
#'
#' TrunkVolume=a*(CrownArea^b)*(TreeHeight^c)  (Nakai et al.)
#'
#' @format A list of nls results for all, conifer,broadleaved tree species
#' slot
#' \describe{
#'   \item{TV$all}{nls results}
#'   \item{TV$conifer}{nls results}
#'   \item{TVbroadleaved}{nls results}
#'   }
#'
#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @seealso \link{TrankVolume_cah_TV2}
#'
#' @examples
#' TV$all
#' TV$conifer
#' TV$broadleaved
"TV"

# TreeVolume_cah_TV ####

#' data frame for summary of tree trunk volume estimation model with crown area and tree height
#'
#' TrunkVolume=a*(CrownArea^b)*(TreeHeight^c)  (Nakai et al.)
#'
#' @format A data frame of nls results for all, conifer,broadleaved tree species
#' slot
#' \describe{
#'   \item{n}{number of samples}
#'   \item{a}{paremeter a}
#'   \item{b}{paremeter b}
#'   \item{c}{paremeter c}
#'   other statistical information ...
#'   }
#'
#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @seealso \link{TV} \link{TrunkVolume_cah_model}
#'
#' @examples
#' df
"df"


# k123_area_vector ####
#'  K123 sites area polygon (sf class)
#'
#'  list of  simplified sf object from shape file of sites area polygon
#'
#'
#'#' @format A sf object of  shape file
#' \describe{
#'   \item{k1}{Geometry type: POLYGON}
#'   \item{k2}{Geometry type: POLYGON}
#'   \item{k3}{Geometry type: POLYGON}
#'   }
#'
#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @examples
#' par(mfrow=c(1,3));sapply(k123_area_vector,plot)
#' sapply(k123_area_vector,st_area)
#'
"k123_area_vector"

# k123_field ####
#' Forest stand data from field survey
#'
#' for every trees (dbh>10cm) in each K123 site
#'
#'
#' ""      ""  "vital"    "memo"     "crown"
#' "conif"    "id_drone" "ca"       "h"        "V_dh"
#' "V_Dah2"   "V_Dah"    "geometry"
#'
#'
#' @format A sf object of  shape file (points)
#' data.frame
#' \describe{
#'   \item{plot}{site number 1,2,3}
#'   \item{label}{tag numbers of each tree}
#'   \item{id}{id for dorne survey}
#'   \item{id_ttops}{id for LiDar survey (ForestTools)}
#'   \item{dbh}{species name}
#'   \item{id_drone}{id for LiDar survey (ForestTools)}
#'   \item{vital}{id for LiDar survey (ForestTools)}
#'   \item{memo}{id for LiDar survey (ForestTools)}
#'   \item{crown}{id for LiDar survey (ForestTools)}
#'   \item{conif}{id for LiDar survey (ForestTools)}
#'   \item{id_drone}{id for LiDar survey (ForestTools)}
#'   \item{ca}{id for LiDar survey (ForestTools)}
#'   \item{h}{id for LiDar survey (ForestTools)}
#'   ...
#'
#'  }
#'
#' @source from natural forest in Kuaraiayama forest of Gigu university (Sanchikanri Labo)
#'
#' @examples
#' par(mfrow=c(1,3));sapply(k123_area_vector,plot)
#' sapply(k123_area_vector,st_area)
#'
#' @examples
#' names(k123_field)
#' plot(k123_field["dbh"])
"k123_field"

# k123_lidar ####
#' Forest stand data (ttops) calculated with aerial LiDar (DSM,DTM,DCHM) with ForestTools
#'
#'
#' @format A sf object of  shape file (points)
#' data.frame
#' \describe{
#'   \item{plot}{site number 1,2,3}
#'   \item{treeID}{treeID(ForestTools)}
#'   \item{lbl}{label number of tree tags}
#'   \item{height}{tree height}
#'   \item{ca}{crown area}
#'   \item{v}{Trank volume calculated from crown area and tree height}
#'   \item{winRadius}{winRadius(ForestTools)}
#'   \item{geometry}{geometry of tree positions}

#'  }
#'
#' @source
#' LiDar from Gifu prefecture
#' Forest stand (kuraiyamaK123) from natural forest in Kuaraiayama forest of Gigu university
#'
#' @examples
#' names(k123_lidar)
#' plot(k123_lidar["height"])

"k123_lidar"

# k123_drone ####
#' Forest stand data distinguished with drone orthophoto and DSM
#'
#' Drone orthophoto and DSM was generated with Metashape.
#'
#'
#' @format A sf object of  shape file (polygon)
#' data.frame
#' \describe{
#'   \item{id}{site number 1,2,3}
#'   \item{a}{treeID(ForestTools)}
#'   \item{h}{label number of tree tags}
#'   \item{sp}{tree height}
#'   \item{lbl}{crown area}
#'   \item{dbh}{Trank volume calculated from crown area and tree height}
#'   \item{plot}{winRadius(ForestTools)}
#'   \item{geometry}{geometry of tree positions}

#'  }
#'
#' @source
#' From Forest stand (kuraiyamaK123) from natural forest in Kuaraiayama forest of Gigu university
#'
#' @examples
#' names(k123_drone)
#' plot(k123_drone["h"])

"k123_drone"


