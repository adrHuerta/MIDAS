#' @title calculation of dekadal amount
#' @description calculation of dekadal amount of precipitation
#' @author Adrian Huerta
#' @importFrom raster brick raster
#' @param r_files a vector several raster directories (output of \link[MIDAS]{call_n_pfiles})
#' @return a raster file object
#' @export

dekadal_cal <- function(r_files = call_n_pfiles()){

  brick(lapply(as.list(r_files), function(z){
    raster(z)
  }))

}
