
#' Time-Based Rolling Binomial Probability
#'
#' Produces a a rolling time-window based vector of binomial probability and
#' confidence intervals.
#' @param .tbl dataframe with two variables.
#' @param x indicates the variable column containing "success" and "failure"
#'   observations coded as 1 or 0.
#' @param tcolumn indicates the variable column containing Date or Date-Time
#'   values.
#' @param unit character, one of "years", "months", "weeks", "days", "hours",
#'   "minutes", "seconds"
#' @param n numeric, describing the length of the time window in the selected
#'   units.
#' @param alpha numeric, probability of a type 1 error, so confidence
#'   coefficient = 1-alpha
#'
#' @import dplyr
#' @import rlang
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @return tibble with binomial point estimate and confidence intervals.
#' @export
#' @seealso \code{\link{binom_ci}}
#' @examples
#' ## Generate Sample Data
#' df <- tibble::data_frame(
#' date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
#' value = rbinom(100, 1, 0.25)
#' )
#'
#' ## Run Function
#' tbr_binom(df, x = value,
#' tcolumn = date, unit = "years", n = 5,
#' alpha = 0.1)
tbr_binom <- function(.tbl, x, tcolumn, unit = "years", n, alpha = 0.05) {

  .tbl <- .tbl %>%
    arrange(!! rlang::enquo(tcolumn)) %>%
    mutate("temp" := purrr::map(row_number(),
                                  ~tbr_binom_window(x = !! rlang::enquo(x), #column that indicates success/failure
                                               tcolumn = !! rlang::enquo(tcolumn), #posix formatted time column
                                               unit = unit,
                                               n = n,
                                               alpha = alpha,
                                               i = .x))) %>%
    tidyr::unnest()
  .tbl <- tibble::as_tibble(.tbl)
  return(.tbl)
}


#' Binomial test based on time window
#'
#' @param x column containing "success" and "failure" observations as 0 or 1
#' @param tcolumn formatted time column
#' @param unit character, one of "years", "months", "weeks", "days", "hours",
#'   "minutes", "seconds"
#' @param n numeric, describing the length of the time window.
#' @param i rows
#' @param alpha numeric, probability of a type 1 error, so confidence
#'   coefficient = 1-alpha
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
  results <- binom_ci(x = df$successes, n = df$n, alpha = alpha, return.df = TRUE)

  return(results)
}




# Binomial CI -------------------------------------------------------------


## Copyright (C) 2001 Frank E Harrell Jr
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##

#' Confidence Intervals for Binomial Probabilities
#'
#' An implementation of the \code{binconf} function in Frank
#' Harrell's Hmisc package. Produces 1-alpha confidence intervals for binomial
#' probabilities.
#' @param x vector containing the number of "successes" for binomial variates.
#' @param n vector containing the numbers of corresponding observations.
#' @param alpha cprobability of a type I error, so confidence coefficient =
#'   1-alpha.
#' @param method character string specifing which method to use. The "exact"
#'   method uses the F distribution to compute exact (based on the binomial cdf)
#'   intervals; the "wilson" interval is score-test-based; and the "asymptotic"
#'   is the text-book, asymptotic normal interval. Following Agresti and Coull,
#'   the Wilson interval is to be preferred and so is the default.
#' @param return.df logical flag to indicate that a data frame rather than a
#'   matrix be returned.
#'
#' @importFrom stats qf qnorm
#' @export
#' @author Frank Harrell, modified by Michael Schramm
#' @keywords internal
#' @references A. Agresti and B.A. Coull, Approximate is better than "exact" for
#'   interval estimation of binomial proportions, \emph{American Statistician,}
#'   \bold{52}:119--126, 1998.
#'
#'   R.G. Newcombe, Logit confidence intervals and the inverse sinh
#'   transformation, \emph{American Statistician,} \bold{55}:200--202, 2001.
#'
#'   L.D. Brown, T.T. Cai and A. DasGupta, Interval estimation for a binomial
#'   proportion (with discussion), \emph{Statistical Science,}
#'   \bold{16}:101--133, 2001.
#' @examples binom_ci(46,50,method="wilson")

binom_ci <- function(x, n, alpha = 0.05,
                     method = c("wilson","exact","asymptotic"),
                     return.df = FALSE)
{
  ## ..modifications for printing and the addition of a
  ##   method argument and the asymptotic interval
  ##   and to accept vector arguments were
  ##   made by Brad Biggerstaff on 10 June 1999
  
  method <- match.arg(method)
  bc <- function(x, n, alpha, method)
  {
    nu1 <- 2 * (n - x + 1)
    nu2 <- 2 * x
    ll <- if (x > 0)
      x/(x + qf(1 - alpha/2, nu1, nu2) * (n - x + 1))
    else
      0
    
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (x < n)
      qf(1 - alpha/2, nu1p, nu2p)
    else
      1
    
    ul <- ((x + 1) * pp)/(n - x + (x + 1) * pp)
    zcrit <-  -qnorm(alpha/2)
    z2 <- zcrit * zcrit
    p <- x/n
    cl <- (p + z2/2/n + c(-1, 1) * zcrit *
             sqrt((p * (1 - p) + z2/4/n)/n))/(1 + z2/n)
    
    if (x == 1)
      cl[1] <-  -log(1 - alpha)/n
    
    if (x == (n - 1))
      cl[2] <- 1 + log(1 - alpha)/n
    
    asymp.lcl <- x/n - qnorm(1 - alpha/2) *
      sqrt(((x/n) * (1 - x/n))/n)
    
    asymp.ucl <- x/n + qnorm(1 - alpha/2) * sqrt(((x/n) * (1 - x/n)
    )/n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(x/n, 3), res)
    
    ##dimnames(res) <- list(c("Exact", "Wilson", "Asymptotic"), c(
    ## "Point Estimate", "Lower", "Upper"))
    switch(method,
           wilson =     res[2,  ],
           exact =      res[1,  ],
           asymptotic = res[3,  ])
  }
  
  if ((length(x) != length(n)) & length(x) == 1)
    x <- rep(x, length(n))
  if ((length(x) != length(n)) & length(n) == 1)
    n <- rep(n, length(x))
  
  mat <- matrix(ncol = 3, nrow = length(x))
  for (i in 1:length(x))
    mat[i,  ] <- bc(x[i], n[i], alpha = alpha, method = method)
  
  dimnames(mat) <- list(rep("", dim(mat)[1]),
                        c("PointEst", "Lower", "Upper"))
  
  if (return.df)
    mat <- as.data.frame(mat, row.names = NULL)
  
  mat
}

