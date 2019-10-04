#' Use Generic Functions with Time Windows
#'
#' @param .tbl a data frame with at least two variables; time column formatted
#'   as date, date/time and value column.
#' @param x column containing the values the function is applied to.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param func specified function
#' @param ... optional additional arguments passed to function \code{func}
#'
#' @import rlang
#' @import dplyr
#' @importFrom tidyr unnest
#' @return tibble
#' @export
#' @examples
#' tbr_misc(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n = 5, func = mean)
tbr_misc <- function(.tbl, x, tcolumn, unit = "years", n, func, ...) {

  col_name <- as.character(substitute(func))

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate(!! col_name := purrr::map(row_number(),
                             ~func_window(x = !! rlang::enquo(x),
                                          tcolumn = !! rlang::enquo(tcolumn),
                                          unit = unit,
                                          n = n,
                                          i = .x,
                                          func = func))) %>%
    tidyr::unnest(!! col_name)
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


func_window <- function(x, tcolumn, unit = "years", n, i, func, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }
  window <-
    x[lubridate::as.duration(tcolumn[i] - tcolumn) / lubridate::duration(num = 1, units = unit) <= n &
        lubridate::as.duration(tcolumn[i] - tcolumn) / lubridate::duration(num = 1, units = unit) >= 0]
  date_window <-
    tcolumn[lubridate::as.duration(tcolumn[i] - tcolumn) / lubridate::duration(num = 1, units = unit) <= n &
                        lubridate::as.duration(tcolumn[i] - tcolumn) / lubridate::duration(num = 1, units = unit) >= 0]
  results <- tibble::tibble(results = func(window, ...), min_date = min(date_window), max_date = max(date_window))
  return(results)
}
