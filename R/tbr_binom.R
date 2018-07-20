
#' Time-Based Rolling Binomial Probability
#'
#' Produces a a rolling time-window based vector of binomial probability and confidence intervals.
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing "success" and "failure" observations as 0 or 1
#' @param tcolumn formated time column
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param alpha numeric, probability of a type 1 error, so confidence coefficient = 1-alpha
#' @param method
#'
#' @import dplyr
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @return tibble with columns for the rolling binomial probability and upper and lower confidence intervals.
#' @export
#' @examples
#' ## Generate Sample Data
#' df <- tibble::data_frame(
#' date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
#' value = rbinom(100, 1, 0.25)
#' )
#'
#' ## Run Function
#' df <- tbrf::roll_binom(df, x = value,
#' tcolumn = date, unit = "years", n = 5,
#' alpha = 0.1)
tbr_binom <- function(.tbl, x, tcolumn, unit = "years", n, alpha = 0.05) {

  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate(temp = purrr::map(row_number(),
                                  ~tbr_binom_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                               tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                               unit = unit,
                                               n = n,
                                               alpha = alpha,
                                               i = .x))) %>%
    tidyr::unnest(temp)
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


#' Binomial test based on time window
#'
#' @param x column containing "success" and "failure" observations as 0 or 1
#' @param tcolumn formated time column
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i rows
#' @param alpha numeric, probability of a type 1 error, so confidence coefficient = 1-alpha
#'
#' @return list
#' @keywords internal
tbr_binom_window <- function(x, tcolumn, unit = "years", n, i, alpha) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  # creates a time-based window
  temp <- x[lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) <= n & lubridate::as.duration(tcolumn[i] - tcolumn)/lubridate::duration(num = 1, units = unit) >= 0]
  df <- data_frame(temp) %>%
    summarise(n = n(), successes = as.integer(sum(temp)))

  # calculates the binomial test with confidence intervals
  results <- binconf(x = df$successes, n = df$n, alpha = alpha, return.df = TRUE)

  return(results)
}
