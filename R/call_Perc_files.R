#' @title call raster percentile files
#' @description calling raster percentile files accordint to the date and directory
#' @author Adrian Huerta
#' @param dir_percent a directory format
#' @param dekada_date a character object (a date in format "YYYY/MM/DD")
#' @return a list object of several raster directories according to the percentile values
#' @examples
#' call_Perc_files(dekada_date = "2017-01-11")
#' call_Perc_files(dekada_date = "2017-01-21")
#' call_Perc_files(dekada_date = "2017-02-01")
#' @export

call_Perc_files <- function(dir_percent = "C:/Users/usuario 01/Desktop/Clim_dek_percentiles",
                            dekada_date = "2017-02-21"){

  check_date_dek(dekada_date)

  dekada_date <- as.Date(dekada_date)
  daily_val <- format(dekada_date, "%d")
  n_p <- c("p10","p20","p30","p70","p80","p90")

  if(daily_val == "11"){

    p_clim_files <- paste(n_p, "_PISCO_DK_CLIM_", format(dekada_date, "%m"), "_01", ".tif", sep = "")
    p_clim_files <- file.path(dir_percent, n_p, p_clim_files)

  } else if(daily_val == "21") {

    p_clim_files <- paste(n_p, "_PISCO_DK_CLIM_", format(dekada_date, "%m"), "_02", ".tif", sep = "")
    p_clim_files <- file.path(dir_percent, n_p, p_clim_files)


  } else {

    p_clim_files <- paste(n_p, "_PISCO_DK_CLIM_", format(dekada_date-1, "%m"), "_03", ".tif", sep = "")
    p_clim_files <- file.path(dir_percent, n_p, p_clim_files)


  }

  p_clim_files <- as.list(p_clim_files)
  names(p_clim_files) <- n_p

  return(p_clim_files)


}
