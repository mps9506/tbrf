
#' List NA
#'
#' function to return tibble with NAs as specified
#' @param x named vector 
#'
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @return empty tibble
#' @keywords internal
list_NA <- function(x) {
  names <- x
  
  results <- tibble::enframe(names) %>%
    mutate(value = as.numeric(.data$value)) %>%
    tidyr::pivot_wider()
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