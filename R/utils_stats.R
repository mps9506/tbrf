

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
#' Produces 1-alpha confidence intervals for binomial probabilities.
#' @param x vector containing the number of "successes" for binomial variates
#' @param n vector containing the numbers of corresponding observations
#' @param alpha cprobability of a type I error, so confidence coefficient = 1-alpha
#'
#' @keywords internal

binconf <- function(x, n, alpha = 0.05,
                    method = c("wilson","exact","asymptotic","all"),
                    include.x = FALSE, include.n = FALSE,
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
    ll <- if(x > 0)
      x/(x + qf(1 - alpha/2, nu1, nu2) * (n - x + 1))
    else
      0

    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if(x < n)
      qf(1 - alpha/2, nu1p, nu2p)
    else
      1

    ul <- ((x + 1) * pp)/(n - x + (x + 1) * pp)
    zcrit <-  - qnorm(alpha/2)
    z2 <- zcrit * zcrit
    p <- x/n
    cl <- (p + z2/2/n + c(-1, 1) * zcrit *
             sqrt((p * (1 - p) + z2/4/n)/n))/(1 + z2/n)

    if(x == 1)
      cl[1] <-  - log(1 - alpha)/n

    if(x == (n - 1))
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
           asymptotic = res[3,  ],
           all =        res,
           res)
  }

  if((length(x) != length(n)) & length(x) == 1)
    x <- rep(x, length(n))
  if((length(x) != length(n)) & length(n) == 1)
    n <- rep(n, length(x))
  if((length(x) > 1 | length(n) > 1) & method == "all") {
    method <- "wilson"
    warning("method=all will not work with vectors...setting method to wilson")
  }
  if(method == "all" & length(x) == 1 & length(n) == 1) {
    mat <- bc(x, n, alpha, method)
    dimnames(mat) <- list(c("Exact", "Wilson", "Asymptotic"),
                          c("PointEst", "Lower", "Upper"))
    if(include.n)
      mat <- cbind(N = n, mat)

    if(include.x)
      mat <- cbind(X = x, mat)

    if(return.df)
      mat <- as.data.frame(mat)

    return(mat)
  }

  mat <- matrix(ncol = 3, nrow = length(x))
  for(i in 1:length(x))
    mat[i,  ] <- bc(x[i], n[i], alpha = alpha, method = method)

  dimnames(mat) <- list(rep("", dim(mat)[1]),
                        c("PointEst", "Lower", "Upper"))
  if(include.n)
    mat <- cbind(N = n, mat)

  if(include.x)
    mat <- cbind(X = x, mat)

  if(return.df)
    mat <- as.data.frame(mat, row.names=NULL)

  mat
}


# Geomean CI] -------------------------------------------------------------
# Author:   Andri Signorell
# Version:	0.99.x
# LICENSE: GPL (>= 2)
# Original: DescTools v0.99.24
# Link: https://github.com/cran/DescTools

#' Geometric Mean and Standard Deviation
#'
#' Calculates the geometric mean, its confidence interval and the geometric standard deviation of a vector x.
#' @param x a positive numeric vector. An object which is not a vector is coerced (if possible) by as.vector.
#' @param method a vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"boot"}.
#' @param conf.level confidence level of the interval. Default is \code{NA}.
#' @param sides a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to \code{FALSE}.
#' @param ... further arguments are passed to the \code{\link{boot}} function. Supported arguments are \code{type} (\code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number of bootstrap replicates \code{R}. If not defined those will be set to their defaults, being \code{"basic"} for \code{type}, option \code{"boot.parallel"} (and if that is not set, \code{"no"}) for \code{parallel} and \code{999} for \code{R}.
#'
#' @return a numeric value.
#' @export
#' @author Andri Signorell <andri@signorell.net>
#' @keywords internal
Gmean <- function(x, method = c("classic", "boot"),
                  conf.level = NA, sides = c("two.sided","left","right"),
                  na.rm = FALSE, ...) {

  # see also: http://www.stata.com/manuals13/rameans.pdf

  if (na.rm) x <- na.omit(x)
  is.na(x) <- x < 0

  if (any(x == 0)) {
    if (is.na(conf.level))
      0

    else
      c(0, NA, NA)

  } else {

    if (is.na(conf.level))
      exp(mean(log(x)))

    else
      exp(MeanCI(x = log(x), method = method,
                 conf.level = conf.level, sides = sides, ...))
  }

}



# Mean CI -----------------------------------------------------------------
# Author:   Andri Signorell
# Version:	0.99.x
# LICENSE: GPL (>= 2)
# Original: DescTools v0.99.24
# Link: https://github.com/cran/DescTools



#' Confidence Interval for the Mean
#'
#' Collection of several approaches to determine confidence intervals for the mean. Both, the classical way and bootstrap intervals are implemented for both, normal and trimmed means.
#' @param x a (non-empty) numeric vector of data values.
#' @param sd the standard deviation of x. If provided it's interpreted as sd of the population and the normal quantiles will be used for constructing the confidence intervals. If left to \code{NULL} (default) the sample \code{sd(x)} will be calculated and used in combination with the t-distribution.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end of \code{x} before the mean is computed. Values of \code{trim} outside that range are taken as the nearest endpoint.
#' @param method A vector of character strings representing the type of intervals required. The value should be any subset of the values \code{"classic"}, \code{"boot"}.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped before the computation proceeds. Defaults to FALSE.
#' @param ... further arguments are passed to the \code{\link{boot}} function. Supported arguments are \code{type} (\code{"norm"}, \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel} and the number of bootstrap replicates \code{R}. If not defined those will be set to their defaults, being \code{"basic"} for \code{type},  option \code{"boot.parallel"} (and if that is not set, \code{"no"}) for \code{parallel}
#'
#' @return a numeric vector with 3 elements: \item{mean}{mean}, \item{lwr.ci}{lower bound of the confidence interval}, \item{upr.ci}{upper bound of the confidence interval}
#' @export
#' @author Andri Signorell <andri@signorell.net>
#' @keywords internal
MeanCI <- function (x, sd = NULL, trim = 0, method = c("classic", "boot"),
                    conf.level = 0.95, sides = c("two.sided","left","right"), na.rm = FALSE, ...) {

  if (na.rm) x <- na.omit(x)

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  winvar <- function(x, trim) {
    n <- length(x)
    # calculate the winsorized variance of x
    trn <- floor(trim * n) + 1

    # new 17.2.2015:
    minval <- sort(x, partial = trn)[trn]
    maxval <- sort(x, partial = max((n - trn + 1), 1))[max((n - trn + 1), 1)]
    winvar <- var(Winsorize(x, minval = minval, maxval = maxval))

    # This was an overkill, we need only the n-thest value here:
    # winvar <- var(Winsorize(x, minval=max(Small(x, trn)), maxval=min(Large(x, trn))))
    #
    # degrees of freedom
    DF <- n - 2*(trn-1) - 1
    return(c(var=winvar, DF=DF))
  }

  method <- match.arg(method, c("classic", "boot"))
  if(method == "classic"){
    if(trim != 0) {
      # see: http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v27.txt
      #      http://www.psychology.mcmaster.ca/bennett/boot09/rt2.pdf

      wvar <- winvar(x, trim)
      # the standard error
      se <- sqrt(wvar["var"]) / ((1 - 2*trim) * sqrt(length(x)))

      res <- mean(x, trim = trim) + c(0, -1, 1) * qt(1-(1-conf.level)/2, wvar["DF"]) * se
      names(res) <- c("mean", "lwr.ci", "upr.ci")

    } else {
      if(is.null(sd)) {
        a <- qt(p = (1 - conf.level)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
      } else {
        a <- qnorm(p = (1 - conf.level)/2) * sd/sqrt(length(x))
      }
      res <- c(mean = mean(x), lwr.ci = mean(x) + a, upr.ci = mean(x) - a)
    }

  } else {

    # see: http://www.psychology.mcmaster.ca/bennett/boot09/percentileT.pdf
    # this might contain an erroneuous calculation of boot variance...

    btype <- InDots(..., arg="type", default="basic")

    # we need separate functions for trimmed means and normal means
    if(trim != 0) {
      boot.fun <- boot::boot(x,
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE, trim = trim)
                         n <- length(i)
                         v <- winvar(x, trim)/((1-2*trim)*sqrt(length(x)))^2
                         c(m, v)
                       },
                       R=InDots(..., arg="R", default=999),
                       parallel=InDots(..., arg="parallel", default="no"))

    } else {
      boot.fun <- boot::boot(x,
                       function(x, i){
                         # this is according to the example in boot.ci
                         m <- mean(x[i], na.rm = FALSE)
                         n <- length(i)
                         v <- (n-1) * var(x[i]) / n^2
                         # v <- (sd(x[i]) / sqrt(n))^2  # following Bennet
                         c(m, v)
                         # IMPORTANT: boot.ci requires the estimated VARIANCE of the statistic
                         # pop sd estimated from bootstrapped sample
                       },
                       R=InDots(..., arg="R", default=999),
                       parallel=InDots(..., arg="parallel", default="no"))
    }
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)

    if(btype == "norm"){
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][2], upr.ci=ci[[4]][3])
    } else {
      res <- c(mean=boot.fun$t0[1], lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
    }
  }

  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf

  return(res)
}

# Median CI ---------------------------------------------------------------
# Author:   Andri Signorell
# Version:	0.99.x
# LICENSE: GPL (>= 2)
# Original: DescTools v0.99.24
# Link: https://github.com/cran/DescTools

#' Confidence Interval for the Median
#'
#' Calculates the confidence interval for the median.
#' @param x a (non-empty) numeric vector of data values.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence interval, must be one of \code{"two.sided"} (default), \code{"left"} or \code{"right"}. You can specify just the initial letter. \code{"left"} would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param na.rm logical. Should missing values be removed? Defaults to \code{FALSE}.
#' @param method defining the type of interval that should be calculated (one out of \code{"exact"}, \code{"boot"}). Default is \code{"exact"}. See Details.
#' @param R The number of bootstrap replicates. Usually this will be a single positive integer. See \code{\link{boot.ci}} for details.
#'
#' @return a numeric vector with 3 elements: \item{median}{median}, \item{lwr.ci}{lower bound of the confidence interval}, \item{upr.ci}{upper bound of the confidence interval}
#' @export
#' @author Andri Signorell <andri@signorell.net>
#' @keywords internal
MedianCI <- function(x, conf.level=0.95, sides = c("two.sided","left","right"), na.rm=FALSE, method=c("exact","boot"), R=999) {
  if(na.rm) x <- na.omit(x)

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)

  # alte Version, ziemlich grosse Unterschiede zu wilcox.test:
  # Bosch: Formelsammlung Statistik (bei Markus Naepflin), S. 95
  # x <- sort(x)
  # return( c(
  # x[ qbinom(alpha/2,length(x),0.5) ], ### lower limit
  # x[ qbinom(1-alpha/2,length(x),0.5) ] ### upper limit
  # ) )

  switch( match.arg(arg=method, choices=c("exact","boot"))
          , "exact" = { # this is the SAS-way to do it
            # https://stat.ethz.ch/pipermail/r-help/2003-September/039636.html
            r <- SignTest(x)$conf.int
          }
          , "boot" = {
            boot.med <- boot::boot(x, function(x, d) median(x[d], na.rm=na.rm), R=R)
            r <- boot::boot.ci(boot.med, conf=conf.level, type="basic")[[4]][4:5]
          } )

  med <- median(x, na.rm=na.rm)
  if(is.na(med)) {   # do not report a CI if the median is not defined...
    r <- rep(NA, 3)
  } else {
    r <- c(median=med, r)
  }
  names(r) <- c("median","lwr.ci","upr.ci")

  if(sides=="left")
    r[3] <- Inf
  else if(sides=="right")
    r[2] <- -Inf

  return( r )

}


