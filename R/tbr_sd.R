#' Time-Based Rolling Standard Deviation
#'
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing the values to calculate the standard deviation.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param na.rm logical. Should missing values be removed?
#'
#' @return tibble with column for the rolling sd.
#' @export
#' @seealso \code{\link{sd}}
#' @examples
#' tbr_sd(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n = 5)
tbr_sd <- function(.tbl, x, tcolumn, unit = "years", n, na.rm = FALSE) {

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate("sd" := purrr::map(row_number(),
                             ~tbr_sd_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                         tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                         unit = unit,
                                         n = n,
                                         i = .x,
                                         na.rm = na.rm))) %>%
    tidyr::unnest("sd")

  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}

#' Standard Deviation Based on a Time-Window
#'
#' @param x column containing the values to calculate the standard deviation.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#' @param ... additional arguments passed to base::sd()
#'
#' @importFrom stats sd
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @return numeric value
#' @keywords internal
tbr_sd_window <- function(x, tcolumn, unit = "years", n, i, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  # create a time-based window by calculating the duration between current row
  # and the previous rows select the rows where 0 <= duration <= n
  window <- open_window(x, tcolumn, unit = unit, n, i)
  
  # calculates the sd
  results <- sd(x = window, ...)

  return(results)
}
