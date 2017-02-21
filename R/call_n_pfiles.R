#' @title call raster Pisco files
#' @description calling raster Pisco files accordint to the date and directory
#' @author Adrian Huerta
#' @param dir_pisco a directory format
#' @param dekada_date a character object (a date in format "YYYY/MM/DD")
#' @return a character object of several raster directories according to the date
#' @examples
#' call_n_pfiles(dekada_date = "2017-01-11")
#' call_n_pfiles(dekada_date = "2017-01-21")
#' call_n_pfiles(dekada_date = "2017-02-01")
#' @export


call_n_pfiles <- function(dir_pisco = "C:/Users/usuario 01/Desktop/Google drive",
                          dekada_date = "2017-02-21"){

  check_date_dek(dekada_date)

  dekada_date <- as.Date(dekada_date)
  daily_val <- format(dekada_date, "%d")

  if(daily_val == "11" | daily_val == "21"){

    raster_dates <- dekada_date - c(10:1)
    raster_names <- paste("SONICS_", format(raster_dates, "%Y"), format(raster_dates, "%m"), format(raster_dates, "%d"), "-D1-1.tif", sep=  "")
    return(file.path(dir_pisco, raster_dates, "raster", raster_names))

  } else {

    d_1 <- format(dekada_date - 1, "%d")
    raster_dates <- seq(dekada_date - as.numeric(d_1), dekada_date - 1, by = "day")
    raster_dates <- raster_dates[-c(1:20)]
    raster_names <- paste("SONICS_", format(raster_dates, "%Y"), format(raster_dates, "%m"), format(raster_dates, "%d"), "-D1-1.tif", sep=  "")
    return(file.path(dir_pisco, raster_dates, "raster", raster_names))

  }


}
