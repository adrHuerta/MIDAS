#' @title make raster files
#' @description make raster files to save it in a specific directory
#' @author Adrian Huerta
#' @param dekada_date a character object (a date in format "YYYY/MM/DD")
#' @return a list object of four names directories
#' @examples
#' make_raster_filename(dekada_date = "2017-01-11")
#' make_raster_filename(dekada_date = "2017-01-21")
#' make_raster_filename(dekada_date = "2017-02-01")
#' @export

make_raster_filename <- function(dekada_date = Sys.Date()){

  check_date_dek(dekada_date)

  dekada_date <- as.Date(dekada_date)
  daily_val <- format(dekada_date, "%d")

  raster_dek <- "DEK_%s.tif"
  raster_dek_stat <- "DEK_%s_stat.tif"
  raster_dek_png <- "DEK_%s.png"
  raster_dek_stat_png <- "DEK_%s_stat.png"

  if( daily_val == "01" ) {

    date_for <- paste(format(dekada_date, "%Y"), format(dekada_date, "%m"), "-3", sep = "")
    raster_dek <- sprintf(raster_dek, date_for)
    raster_dek_stat <- sprintf(raster_dek_stat, date_for)
    raster_dek_png <- sprintf(raster_dek_png, date_for)
    raster_dek_stat_png <- sprintf(raster_dek_stat_png, date_for)

  } else if ( daily_val == "11") {

    date_for <- paste(format(dekada_date, "%Y"), format(dekada_date, "%m"), "-1", sep = "")
    raster_dek <- sprintf(raster_dek, date_for)
    raster_dek_stat <- sprintf(raster_dek_stat, date_for)
    raster_dek_png <- sprintf(raster_dek_png, date_for)
    raster_dek_stat_png <- sprintf(raster_dek_stat_png, date_for)

  } else if ( daily_val == "21") {

    date_for <- paste(format(dekada_date, "%Y"), format(dekada_date, "%m"), "-2", sep = "")
    raster_dek <- sprintf(raster_dek, date_for)
    raster_dek_stat <- sprintf(raster_dek_stat, date_for)
    raster_dek_png <- sprintf(raster_dek_png, date_for)
    raster_dek_stat_png <- sprintf(raster_dek_stat_png, date_for)

  }

  return(list(raster_dek = raster_dek,
              raster_dek_stat = raster_dek_stat,
              raster_dek_png = raster_dek_png,
              raster_dek_stat_png = raster_dek_stat_png))

}
