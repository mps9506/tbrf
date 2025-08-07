
#' List NA
#'
#' function to return tibble with NAs as specified
#' @param x named vector 
#'
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
#' @param x dataframe
#' @param tcolumn time column
#' @param unit unit
#' @param n desired n
#' @param i row number
#' @param na.pad logical if `na.pad = TRUE` incomplete windows (duration of the window < `n`) return `NA`.
#'
#' @return vector
#' @export
#' @keywords internal
#'
open_window <- function(x,
                        tcolumn,
                        unit = "years",
                        n, 
                        i,
                        na.pad) {
  
  ## if the duration between the current row and the first row is < n
  ## return NA

  p <- lubridate::interval(tcolumn[i], tcolumn)
  p <- lubridate::as.period(p, unit = unit)
  
  if (isTRUE(na.pad)) {
    if (-as.period(interval(tcolumn[i], tcolumn[1]), unit = unit) < lubridate::period(n, unit)) {
      return(NA)
      } else {
           window <- x[p <= lubridate::period(0, unit) & p >= -lubridate::period(n, unit)]
      return(window)
      }
  } else { 
  ## if na.pad == FALSE return all the calculations
  window <- x[p <= lubridate::period(0, unit) & p >= -lubridate::period(n, unit)]
  return(window)  
}
  
  
}