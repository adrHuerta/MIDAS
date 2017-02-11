#' @title drougth clasification function
#' @description drougth clasification function for dekadal precipitation
#' @author Adrian Huerta
#' @param r_dek a raster object (a dekadal precipitation raster)
#' @param p10 a raster object (percentile 10th of climatology dekadal precipitation)
#' @param p20 a raster object (percentile 20th of climatology dekadal precipitation)
#' @param p30 a raster object (percentile 30th of climatology dekadal precipitation)
#' @param p70 a raster object (percentile 70th of climatology dekadal precipitation)
#' @param p80 a raster object (percentile 80th of climatology dekadal precipitation)
#' @param p90 a raster object (percentile 90th of climatology dekadal precipitation)
#' @seealso \url{http://www.crc-sas.org/es/content/monitoreo/reporte_sequias.pdf}
#' @return a raster object
#' @export

drought_class <- function(r_dek, p10, p20, p30, p70, p80, p90){

  ifelse(r_dek <= p10, 1,  #Sequía extrema
         ifelse(r_dek > p10 & r_dek <= p20, 2,  #Sequía severa
                ifelse(r_dek > p20 & r_dek <= p30, 3,  #Sequía moderada
                       ifelse(r_dek > p30 & r_dek <= p70, 4,  #Normal
                              ifelse(r_dek > p70 & r_dek <= p80, 5,  #Moderadamente húmedo
                                     ifelse(r_dek > p80 & r_dek <= p90, 6,  #Severamente húmedo
                                            7))))))   #Extremadamente  húmedo

}
