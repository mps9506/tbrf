
#' Time-Based Rolling Sum
#'
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing the values to calculate the sum.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#'
#' @import dplyr
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @return dataframe with column for the rolling sum.
#' @export
tbr_sum <- function(.tbl, x, tcolumn, unit = "years", n) {

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate(sum = purrr::map(row_number(),
                            ~tbr_sum_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                      tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                      unit = unit,
                                      n = n,
                                      i = .x))) %>%
    tidyr::unnest(sum)

  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


#' Sum Based on a Time-Window
#'
#' @param x column containing the values to calculate the sum.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#'
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @return numeric value
#' @keywords internal
tbr_sum_window <- function(x, tcolumn, unit = "years", n, i) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", u)
  }

  # creates a time-based window
  temp <- x[lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) <= n & lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) >= 0]

  # calculates the sum
  results <- sum(temp)

  return(results)
}
