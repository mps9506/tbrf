#' Time-Based Rolling Median
#'
#' Produces a a rolling time-window based vector of medians and confidence intervals.
#'
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing the numeric values to calculate the mean.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param conf.level confidence level of the interval. Default is .95.
#' @param ... additional arguments passed to DescTools::MedianCI()
#'
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @return tibble with columns for the rolling median and upper and lower confidence intervals.
#' @export
tbr_median <- function(.tbl, x, tcolumn, unit = "years", n, conf.level = 0.95, ...) {

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate(temp = purrr::map(row_number(),
                             ~tbr_median_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                        tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                        unit = unit,
                                        n = n,
                                        conf.level = conf.level,
                                        i = .x,
                                        ...))) %>%
    tidyr::unnest(temp)
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}

#' Median Based on a Time-Window
#'
#' @param x column containing the values to calculate the median.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param conf.level confidence level of the interval. Default is .95.
#' @param i row
#' @param ... additional arguments passed to DescTools::MedianCI()
#'
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @importFrom DescTools MedianCI
#' @return list
#' @keywords internal
tbr_median_window <- function(x, tcolumn, unit = "years", n, conf.level, i, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", u)
  }

  # creates a time-based window
  temp <- x[lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) <= n & lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) >= 0]

  # calculates the geometric mean with confidence interval
  results <- tibble::as_tibble(as.list(DescTools::MedianCI(x = temp, conf.level = conf.level, ...)))

  return(results)
}
