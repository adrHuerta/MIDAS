#' @title calculation of drougth classification
#' @description calculation of drougth classification
#' @author Adrian Huerta
#' @importFrom raster overlay brick
#' @param r a raster object (output of \link[MIDAS]{dekadal_cal})
#' @param p_files percentile files (output of \link[MIDAS]{call_Perc_files})
#' @param fun_ a function that classify drougth
#' @return a raster file object
#' @export

drougth_classification <- function(r = dekadal_cal,
                                   p_files = call_Perc_files(),
                                   fun_ = drought_class){

  p_files <- lapply(p_files, brick)


  b <- overlay(r,
               p_files$p10,
               p_files$p20,
               p_files$p30,
               p_files$p70,
               p_files$p80,
               p_files$p90, fun = fun_)

  return(b)
}
