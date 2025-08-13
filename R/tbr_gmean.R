
#' Time-Based Rolling Geometric Mean
#'
#' Produces a a rolling time-window based vector of geometric means and
#' confidence intervals.
#'
#' @param .tbl a data frame with at least two variables; time column formatted
#'   as date, date/time and value column.
#' @param x column containing the values to calculate the geometric mean.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours",
#'   "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param na.pad logical. If `na.pad = TRUE` incomplete windows (duration of the window < `n`) return `NA`. Defatuls to `TRUE`
#' @param ... additional arguments passed to \code{\link{gm_mean_ci}}
#'
#' @return tibble with columns for the rolling geometric mean and upper and
#'   lower confidence levels.
#' @export
#' @seealso \code{\link{gm_mean_ci}}
#' @examples
#'
#' ## Return a tibble with new rolling geometric mean column
#' tbr_gmean(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n = 5)
#' 
#' \dontrun{
#' ## Return a tibble with rolling geometric mean and 95% CI
#' tbr_gmean(Dissolved_Oxygen, x = Average_DO, tcolumn = Date, unit = "years", n = 5, conf = .95)}
tbr_gmean <- function(.tbl, x, tcolumn, unit = "years", n, na.pad = TRUE, ...) {

  dots <- list(...)

  default_dots <- list(conf = NA,
                       na.rm = FALSE,
                       zero.propagate = FALSE,
                       type = "basic",
                       R = 1000,
                       parallel = "no",
                       ncpus = getOption("boot.ncpus", 1L),
                       cl = NULL)

  default_dots[names(dots)] <- dots

  # apply the window function to each row
  .tbl <- .tbl %>%
    arrange(!! enquo(tcolumn)) %>%
    mutate("temp" := map(row_number(),
                             ~tbr_gmean_window(x = !! enquo(x), #column that indicates success/failure
                                         tcolumn = !! enquo(tcolumn), #posix formatted time column
                                         unit = unit,
                                         n = n,
                                         i = .x,
                                         na.pad = na.pad,
                                         conf = default_dots$conf,
                                         na.rm = default_dots$na.rm,
                                         zero.propagate = default_dots$zero.propagate,
                                         type = default_dots$type,
                                         R = default_dots$R,
                                         parallel = default_dots$parallel,
                                         ncpus = default_dots$ncpus,
                                         cl = default_dots$cl))) %>%
    unnest("temp")
  .tbl <- as_tibble(.tbl)
  return(.tbl)
}


#' Geometric mean based on a time-window
#'
#' @param x column containing the values to calculate the geometric mean.
#' @param tcolumn formatted time column.
#' @param unit character, one of "years", "months", "weeks", "days", "hours",
#'   "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i row
#' @param ... additional arguments passed to gmean_ci
#'
#' @return list
#' @keywords internal
tbr_gmean_window <- function(x, tcolumn, unit = "years", n, i, na.pad, ...) {

  # checks for valid unit values
  u <- (c("years", "months", "weeks", "days", "hours", "minutes", "seconds"))

  if (!unit %in% u) {
    stop("unit must be one of ", paste(u, collapse = ", "))
  }

  # if conf.level = NA return one column
  # else three columns
  dots <- list(...)

  if (is.na(dots$conf)) {
    resultsColumns <- c("mean" = NA)
  }

  else {
    resultsColumns <- c("mean" = NA, 
                        "lwr_ci" = NA,
                        "upr_ci" = NA)
  }

  # do not calculate the first row, always returns NA
  if (i == 1) {
    results <- list_NA(resultsColumns)
    return(results)
  }
  else {
    # create a time-based window by calculating the duration between current row
    # and the previous rows select the rows where 0 <= duration <= n
    window <- open_window(x, tcolumn, unit = unit, n, i, na.pad)
    
    # if length is 1 or less, return NAs
    if (length(window) <= 1) {
      results <- list_NA(resultsColumns)
      # return some messages that NAs are returned
      return(results)
      message("NAs produced because the specified time window (n) is too short. Specify a larger n to eliminate NAs")
    }
    # else calculate the geometric mean with confidence interval
    else{
      
      if (is.na(dots$conf)) {
        results <- as_tibble(list(mean = gm_mean_ci(window = window,
                                                    conf = NA,
                                                    na.rm = dots$na.rm,
                                                    zero.propagate = dots$zero.propagate)))
      }
      
      else {
        results <- as_tibble(as.list(gm_mean_ci(window = window,
                                                na.rm = dots$na.rm,
                                                zero.propagate = dots$zero.propagate)))
      }
      
      return(results)
    }
  }
}



# gm_mean Calculate the Geometric Mean ------------------------------------



#' Calculates the Geometric Mean
#'
#' Originally from Paul McMurdie, Ben Bolker, and Gregor on Stack Overflow:
#' https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
#' @param x vector of numeric values
#' @param na.rm logical TRUE/FALSE remove NA values
#' @param zero.propagate logical TRUE/FALSE. Allows the optional propagation of
#'   zeros.
#'
#' @return the geometric mean of the vector
#' @export
#'
#' @keywords internal
gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
  if (any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  if (zero.propagate) {
    if (any(x == 0, na.rm = TRUE)) {
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
  }
}


#' Returns the Geomean and CI
#'
#' Generates Geometric mean and confidence intervals using bootstrap.
#' @param window vector of data values
#' @param conf confidence level of the required interval. \code{NA} if skipping
#'   calculating the bootstrapped CI
#' @param na.rm logical \code{TRUE/FALSE}. Remove NAs from the dataset. Defaults
#'   \code{TRUE}
#' @param zero.propagate logical \code{TRUE/FALSE} Allows the optional propagation of
#'   zeros.
#' @param type character string, one of \code{c("norm","basic", "stud", "perc",
#'   "bca")}. \code{"all"} is not a valid value. See \code{\link[boot]{boot.ci}}
#' @param R the number of bootstrap replicates. see \code{\link[boot]{boot}}
#' @param parallel The type of parallel operation to be used (if any). see
#'   \code{\link[boot]{boot}}
#' @param ncpus integer: number of process to be used in parallel operation. see
#'   \code{\link[boot]{boot}}
#' @param cl optional parallel or snow cluster for use if \code{parallel =
#'   "snow"}. see \code{\link[boot]{boot}}
#'
#' @return named list with geometric mean and (optionally) specified confidence
#'   interval
#' @export
#'
#' @keywords internal
gm_mean_ci <- function(window, conf = 0.95, na.rm = TRUE, type = "basic",
                       R = 1000, parallel = "no", ncpus = getOption("boot.ncpus", 1L),
                       cl = NULL, zero.propagate = FALSE) {

  ## if conf is.na return just the gm_mean
  if (is.na(conf)) {
    results <- c(mean = gm_mean(window, 
                                na.rm = na.rm, 
                                zero.propagate = zero.propagate))
  }
  ## else return gm_mean + conf intervals

  else {

    ## type must be one of "norm", "basic", "stud", "perc", "bca"
    gmboot <- boot(window, function(x,i) {
      gm <- gm_mean(x[i], na.rm = na.rm, zero.propagate = zero.propagate)
      n <- length(i)
      v <- (n - 1) * stats::var(x[i]) / n^2
      c(gm, v)
    },
    R = R,
    parallel = parallel,
    ncpus = ncpus,
    cl = cl)


    ci <- boot.ci(gmboot, conf = conf, type = type)

    if (type == "norm") {
      results <- c(mean = ci[[2]],
                   lwr_ci = ci[[4]][[2]],
                   upr_ci = ci[[4]][[3]])
    }
    else {
      results <- c(mean = ci[[2]],
                 lwr_ci = ci[[4]][[4]],
                 upr_ci = ci[[4]][[5]])
    }
  }
  return(results)
}
