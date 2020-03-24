
# function to return tibble with NAs as specified
# list_NA(c("mean", "lwr.ci", "upr.ci"))
list_NA <- function(...) {
  names <- c(...)
  results <- as.list(c(rep(NA, length(names))))
  names(results) <- names
  results <- tibble::as_tibble(results)
  results[1:length(results)] <- as.numeric(results[1:length(results)])
  return(results)
}


#' Open Window
#' 
#' calculates the period at each row from the row of interest
#' @param x dateframe
#' @param tcolumn time column
#' @param unit unit
#' @param n desired n
#' @param i row number
#'
#' @importFrom lubridate interval
#' @importFrom lubridate as.period
#' @importFrom lubridate period
#' @return vector
#' @export
#' @keywords internal
#'
open_window <- function(x,
                        tcolumn,
                        unit = "years",
                        n, 
                        i) {
  #p <- lubridate::as.period(difftime(tcolumn[i], tcolumn), unit = unit)
  p <- lubridate::interval(tcolumn[i], tcolumn)
  p <- lubridate::as.period(p)
  window <- x[p <= lubridate::period(0, unit) & p >= -lubridate::period(n, unit)]
  return(window)
  
}