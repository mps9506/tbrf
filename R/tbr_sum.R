
#' Time-Based Rolling Sum
#'
#' @param .tbl a data frame with at least two variables; time column formatted
#'   as date, date/time and value column.
#' @param x column containing the values to calculate the sum.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours",
#'   "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param na.rm logical. Should missing values be removed?
#'
#' @import dplyr
#' @import rlang
#' @importFrom purrr map
#' @return dataframe with column for the rolling sum.
#' @export
#' @seealso \code{\link{sum}}
#' @examples
#' tbr_sum(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n =
#' 5)
tbr_sum <- function(.tbl, x, tcolumn, unit = "years", n, na.rm = FALSE) {

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate("sum" := purrr::map(row_number(),
                            ~tbr_sum_window(x = !! rlang::enquo(x), #column to sum
                                      tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                      unit = unit,
                                      n = n,
                                      i = .x,
                                      na.rm = na.rm))) %>%
    tidyr::unnest(.data$sum)

  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


#' Sum Based on a Time-Window
#'
#' @param x column containing the values to calculate the sum.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#' @param na.rm logical. Should missing values be removed?
#'
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @return numeric value
#' @keywords internal
tbr_sum_window <- function(x, tcolumn, unit = "years", n, i, na.rm) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  # create a time-based window by calculating the duration between current row
  # and the previous rows select the rows where 0 <= duration <= n
  window <- x[lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) <= n & lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) >= 0]

  # calculates the sum
  results <- sum(window, na.rm = na.rm)

  return(results)
}
