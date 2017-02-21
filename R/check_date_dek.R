#' @title checking date of calculation of dekedal amounts
#' @description checking date of calculation of dekedal amounts of precipitation
#' @author Adrian Huerta
#' @param z a character object (a date in format "YYYY/MM/DD")
#' @return a stop message if the date is different of 1, 11, or 21
#' @examples
#' check_date_dek("2017-01-01")
#' check_date_dek("2017-01-02")
#' @export

check_date_dek <- function(z){

  z <- as.Date(z)
  day_z <- substr(as.character(format(z, "%d")), 2, 2)

  if( day_z != "1" )
    stop("In this date should not be calculated a dekadal amount")

}
