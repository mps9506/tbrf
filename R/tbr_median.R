#' Time-Based Rolling Median
#'
#' Produces a a rolling time-window based vector of medians and confidence intervals.
#'
#' @param .tbl a data frame with at least two variables; time column formatted as date, date/time and value column.
#' @param x column containing the numeric values to calculate the mean.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param ... additional arguments passed to \code{\link{median_ci}}
#'
#' @import rlang
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @return tibble with columns for the rolling median and upper and lower confidence intervals.
#' @export
#' @seealso \code{\link{median_ci}}
#' @examples
#' ## Return a tibble with new rolling median column
#' tbr_median(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years",
#' n = 5)
#' 
#' \dontrun{
#' ## Return a tibble with rolling median and 95% CI 
#' tbr_median(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n = 5, conf = .95)}
tbr_median <- function(.tbl, x, tcolumn, unit = "years", n, ...) {

  dots <- list(...)

  default_dots <- list(conf = NA,
                       na.rm = FALSE,
                       type = "basic",
                       R = 1000,
                       parallel = "no",
                       ncpus = getOption("boot.ncpus", 1L),
                       cl = NULL)

  default_dots[names(dots)] <- dots

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate("temp" := purrr::map(row_number(),
                             ~tbr_median_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                        tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                        unit = unit,
                                        n = n,
                                        i = .x,
                                        conf = default_dots$conf,
                                        na.rm = default_dots$na.rm,
                                        type = default_dots$type,
                                        R = default_dots$R,
                                        parallel = default_dots$parallel,
                                        ncpus = default_dots$ncpus,
                                        cl = default_dots$c))) %>%
    tidyr::unnest(.data$temp)
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}

#' Median Based on a Time-Window
#'
#' @param x column containing the values to calculate the median.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours", "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#' @param ... additional arguments passed to \code{\link{median_ci}}
#'
#' @importFrom lubridate as.duration duration
#' @importFrom tibble as.tibble
#' @return list
#' @keywords internal
tbr_median_window <- function(x, tcolumn, unit = "years", n, i, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  resultsColumns <- c("median", "lwr_ci", "upr_ci")

  if (i == 1) {
    results <- list(NA, NA, NA)
    names(results) <- c("median", "lwr_ci", "upr_ci")
    return(tibble::as_tibble(results))
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

    else {
      results <- tibble::as_tibble(as.list(median_ci(window = window, ...)))

      return(results)
    }
  }
}


#' Returns the median and CI
#'
#' Generates median and confidence intervals using bootstrap.
#' @param window vector of data values
#' @param conf confidence level of the required interval. \code{NA} if skipping
#'   calculating the bootstrapped CI
#' @param na.rm logical code{TRUE/FALSE}. Remove NAs from the dataset. Defaults
#'   \code{TRUE}
#' @param type character string, one of \code{c("norm","basic", "stud", "perc",
#'   "bca")}. \code{"all"} is not a valid value. See \code{\link{boot.ci}}
#' @param R the number of bootstrap replicates. see \code{\link{boot}}
#' @param parallel The type of parallel operation to be used (if any). see
#'   \code{\link{boot}}
#' @param ncpus integer: number of process to be used in parallel operation. see
#'   \code{\link{boot}}
#' @param cl optional parallel or snow cluster for use if \code{parallel =
#'   "snow"}. see \code{\link{boot}}
#'
#' @return named list with mean and (optionally) specified confidence
#'   interval
#' @import boot
#' @importFrom stats var
#' @importFrom stats median
#' @export
#'
#' @keywords internal
median_ci <- function(window, conf = 0.95, na.rm = TRUE, type = "basic",
                    R = 1000, parallel = "no", ncpus = getOption("boot.ncpus", 1L),
                    cl = NULL) {
  
  ## if conf is.na return just the gm_mean
  if (is.na(conf)) {
    results <- c(median = stats::median(window, na.rm = na.rm))
  }
  ## else return median + conf intervals
  
  else {
    
    ## type must be one of "norm", "basic", "stud", "perc", "bca"
    boot <- boot::boot(window, function(x,i) {
      md <- stats::median(x[i], na.rm = na.rm)
      n <- length(i)
      v <- (n - 1) * stats::var(x[i]) / n^2
      c(md, v)
    },
    R = R,
    parallel = parallel,
    ncpus = ncpus,
    cl = cl)
    
    
    ci <- boot::boot.ci(boot, conf = conf, type = type)
    
    if (type == "norm") {
      results <- c(median = ci[[2]],
                   lwr_ci = ci[[4]][[2]],
                   upr_ci = ci[[4]][[3]])
    }
    else {
      results <- c(median = ci[[2]],
                   lwr_ci = ci[[4]][[4]],
                   upr_ci = ci[[4]][[5]])
    }
  }
  return(results)
}