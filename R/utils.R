
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


open_window <- function(x,
                        tcolumn,
                        unit = "years",
                        n, 
                        i) {
  p <- as.period(difftime(tcolumn[i], tcolumn), unit = unit)
  window <- x[p >= period(0, unit) & p <= period(n, unit)]
  return(window)
  
}