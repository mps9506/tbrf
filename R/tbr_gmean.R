
#' Time-Based Rolling Geometric Mean
#'
#' Produces a a rolling time-window based vector of geometric means and confidence intervals.
#'
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing the values to calculate the geometric mean.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param ... additional arguments passed to DescTools::Gmean
#'
#' @import dplyr
#' @import rlang
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @return tibble with columns for the rolling geometric mean and upper and lower confidence levels.
#' @export
tbr_gmean <- function(.tbl, x, tcolumn, unit = "years", n, ...) {

  #Match dots args: method = "boot", conf.level = 0.95, sides = "two.sided",
  #na.rm = FALSE, type = "basic", parallel = "boot.parallel", R = 999
  dots <- list(...)

  default_dots <- list(method = "boot", conf.level = 0.95,
                       sides = "two.sided", na.rm = FALSE,
                       type = "basic", parallel = "boot.parallel",
                       R = 999)

  default_dots[names(dots)] <- dots

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate("temp" := purrr::map(row_number(),
                             ~tbr_gmean_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                         tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                         unit = unit,
                                         n = n,
                                         i = .x,
                                         method = default_dots$method,
                                         conf.level = default_dots$conf.level,
                                         sides = default_dots$sides,
                                         na.rm = default_dots$na.rm))) %>%
    tidyr::unnest(!! "temp")
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


#' Geometric mean based on a time-window
#'
#' @param x column containing the values to calculate the geometric mean.
#' @param tcolumn formated time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#' @param ... additional arguments passed to Gmean
#'
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @return list
#' @keywords internal
tbr_gmean_window <- function(x, tcolumn, unit = "years", n, i, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  # if conf.level = NA return one column
  # else three columns
  dots <- list(...)

  if (is.na(dots$conf.level)) {
    resultsColumns <- c("mean")
  }

  else {
    resultsColumns <- c("mean", "lwr.ci", "upr.ci")
  }

  # do not calculate the first row, always returns NA
  if (i == 1) {
    results <- list_NA(resultsColumns)
    return(results)
  }
  else {
    # create a time-based window by calculating the duration between current row
    # and the previous rows select the rows where 0 <= duration <= n
    window <- x[lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) <= n & lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) >= 0]

    # if length is 1 or less, return NAs
    if (length(window) <= 1) {
      results <- list_NA(resultsColumns)
      # return some messages that NAs are returned
      return(results)
      message("NAs produced because the specified time window (n) is too short. Specify a larger n to eliminate NAs")
    }
    # else calculate the geometric mean with confidence interval
    else{

      if (is.na(dots$conf.level)) {
        results <- tibble::as.tibble(list(mean = Gmean(x = window, ...)))
      }

      else {
        results <- tibble::as_tibble(as.list(Gmean(x = window, ...)))
      }

      return(results)
    }
  }
}
