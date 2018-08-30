

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
#' An implementation of the \code{\link[Hmisc]{binconf}} function in Frank
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
#' @seealso \code{\link[Hmisc]{binconf}}
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




# Median CI ---------------------------------------------------------------
# Author:   Andri Signorell
# Version:	0.99.x
# LICENSE: GPL (>= 2)
# Original: DescTools v0.99.24
# Link: https://github.com/cran/DescTools

#' Confidence Interval for the Median
#'
#' An implementation of the \code{\link[DescTools]{MedianCI}} function in Andri
#' Signorell's DescTools package. Calculates the confidence interval for the
#' median.
#' @param x a (non-empty) numeric vector of data values.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#'   interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}. You can specify just the initial letter. \code{"left"}
#'   would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param na.rm logical. Should missing values be removed? Defaults to
#'   \code{FALSE}.
#' @param method defining the type of interval that should be calculated (one
#'   out of \code{"exact"}, \code{"boot"}). Default is \code{"exact"}. See
#'   Details.
#' @param R The number of bootstrap replicates. Usually this will be a single
#'   positive integer. See \code{\link{boot.ci}} for details.
#'
#' @return a numeric vector with 3 elements: \item{median}{median},
#'   \item{lwr.ci}{lower bound of the confidence interval}, \item{upr.ci}{upper
#'   bound of the confidence interval}
#' @importFrom stats median na.omit
#' @export
#' @author Andri Signorell <andri@signorell.net>
#' @keywords internal
#' @seealso \code{\link[DescTools]{MedianCI}}
median_ci <- function(x, conf.level=0.95, sides = c("two.sided","left","right"), na.rm=FALSE, method=c("exact","boot"), R=999) {
  if (na.rm) x <- na.omit(x)

  sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
  if (sides != "two.sided")
    conf.level <- 1 - 2*(1 - conf.level)

  # alte Version, ziemlich grosse Unterschiede zu wilcox.test:
  # Bosch: Formelsammlung Statistik (bei Markus Naepflin), S. 95
  # x <- sort(x)
  # return( c(
  # x[ qbinom(alpha/2,length(x),0.5) ], ### lower limit
  # x[ qbinom(1-alpha/2,length(x),0.5) ] ### upper limit
  # ) )

  switch( match.arg(arg = method, choices = c("exact","boot"))
          , "exact" = {# this is the SAS-way to do it
            # https://stat.ethz.ch/pipermail/r-help/2003-September/039636.html
            r <- SignTest(x)$conf.int
          }
          , "boot" = {
            boot.med <- boot::boot(x, function(x, d) median(x[d], na.rm = na.rm), R = R)
            r <- boot::boot.ci(boot.med, conf = conf.level, type = "basic")[[4]][4:5]
          } )

  med <- median(x, na.rm = na.rm)
  if (is.na(med)) {   # do not report a CI if the median is not defined...
    r <- rep(NA, 3)
  } else {
    r <- c(median = med, r)
  }
  names(r) <- c("median","lwr.ci","upr.ci")

  if (sides == "left")
    r[3] <- Inf
  else if (sides == "right")
    r[2] <- -Inf

  return( r )

}


# Winsorize ---------------------------------------------------------------

#' Winsorize
#'
#' @importFrom stats quantile
#' @keywords internal
Winsorize <- function(x, minval = NULL, maxval = NULL,
                      probs=c(0.05, 0.95), na.rm = FALSE) {

  # following an idea from Gabor Grothendieck
  # http://r.789695.n4.nabble.com/how-to-winsorize-data-td930227.html

  # in HuberM things are implemented the same way

  # don't eliminate NAs in x, moreover leave them untouched,
  # just calc quantile without them...

  # pmax(pmin(x, maxval), minval)

  # the pmax(pmin()-version is slower than the following

  if(is.null(minval) || is.null(maxval)){
    xq <- quantile(x=x, probs=probs, na.rm=na.rm)
    if(is.null(minval)) minval <- xq[1]
    if(is.null(maxval)) maxval <- xq[2]
  }

  x[x<minval] <- minval
  x[x>maxval] <- maxval

  return(x)

  # see also Andreas Alfons, KU Leuven
  # roubustHD, Winsorize

  # Jim Lemon's rather clumsy implementation:

  # #added winsor.var and winsor.sd and winsor.mean (to supplement winsor.means)
  # #August 28, 2009 following a suggestion by Jim Lemon
  # #corrected January 15, 2009 to use the quantile function rather than sorting.
  # #suggested by Michael Conklin in correspondence with Karl Healey
  # #this preserves the order of the data
  # "wins" <- function(x,trim=.2, na.rm=TRUE) {
  # if ((trim < 0) | (trim>0.5) )
  # stop("trimming must be reasonable")
  # qtrim <- quantile(x,c(trim,.5, 1-trim),na.rm = na.rm)
  # xbot <- qtrim[1]
  # xtop <- qtrim[3]
  # if(trim<.5) {
  # x[x < xbot]  <- xbot
  # x[x > xtop] <- xtop} else {x[!is.na(x)] <- qtrim[2]}
  # return(x) }

}


# Sign Test ---------------------------------------------------------------

#' Sign Test
#'
#' @importFrom stats binom.test complete.cases qbinom pbinom terms
#' @keywords internal

SignTest <- function (x, ...)  UseMethod("SignTest")

SignTest.formula <- function (formula, data, subset, na.action, ...) {

  # this is designed just like wilcox.test.formula

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- split(mf[[response]], g)
  names(DATA) <- c("x", "y")
  y <- DoCall("SignTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y
}

# test:
#  cbind( c(NA,sort(x)), 0:n, dbinom(0:n, size=n, prob=0.5),  pbinom(0:n, size=n, prob=0.5))

SignTest.default <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                             mu = 0, conf.level = 0.95, ...) {

  MedianCI_Binom <- function( x, conf.level = 0.95,
                              alternative = c("two.sided", "less", "greater"), na.rm = FALSE ){
    # http://www.stat.umn.edu/geyer/old03/5102/notes/rank.pdf
    # http://de.scribd.com/doc/75941305/Confidence-Interval-for-Median-Based-on-Sign-Test
    if(na.rm) x <- na.omit(x)
    n <- length(x)
    switch( match.arg(alternative)
            , "two.sided" = {
              k <- qbinom(p = (1 - conf.level) / 2, size=n, prob=0.5, lower.tail=TRUE)
              ci <- sort(x)[c(k, n - k + 1)]
              attr(ci, "conf.level") <- 1 - 2 * pbinom(k-1, size=n, prob=0.5)
            }
            , "greater" = {
              k <- qbinom(p = (1 - conf.level), size=n, prob=0.5, lower.tail=TRUE)
              ci <- c(sort(x)[k], Inf)
              attr(ci, "conf.level") <- 1 - pbinom(k-1, size=n, prob=0.5)
            }
            , "less" = {
              k <- qbinom(p = conf.level, size=n, prob=0.5, lower.tail=TRUE)
              ci <- c(-Inf, sort(x)[k])
              attr(ci, "conf.level") <- pbinom(k, size=n, prob=0.5)
            }
    )
    return(ci)
  }

  alternative <- match.arg(alternative)

  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")

  if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
        (conf.level > 0) && (conf.level < 1)))
    stop("'conf.level' must be a single number between 0 and 1")

  if (!is.numeric(x))
    stop("'x' must be numeric")

  if (!is.null(y)) {
    if (!is.numeric(y))
      stop("'y' must be numeric")
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")

    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    METHOD <- "Dependent-samples Sign-Test"
    x <- (x - y)

  } else {
    DNAME <- deparse(substitute(x))
    x <- x[is.finite(x)]
    METHOD <- "One-sample Sign-Test"
  }

  d <- (x - mu)

  # Naive version:
  n.valid <- sum(d > 0) + sum(d < 0)
  if(n.valid > 0) {
    RVAL <- binom.test(x=sum(d > 0), n=n.valid, p=0.5, alternative = alternative, conf.level = conf.level )
  } else {
    RVAL <- binom.test(x=1, n=1)
  }

  RVAL$method <- METHOD
  RVAL$data.name <- DNAME
  names(mu) <- if (!is.null(y)) "median difference" else "median"

  names(RVAL$statistic) <- "S"
  RVAL$estimate <- median(d + mu, na.rm=TRUE)
  names(RVAL$parameter) <- "number of differences"
  mci <- MedianCI_Binom(d + mu, conf.level=conf.level, alternative=alternative, na.rm=TRUE)
  RVAL$conf.int <- mci
  attr(RVAL$conf.int, "conf.level") = round(attr(mci,"conf.level"), 3)

  names(RVAL$estimate) <- "median of the differences"
  RVAL$null.value <- mu
  class(RVAL) <- "htest"
  return(RVAL)

}

DoCall <- function (what, args, quote = FALSE, envir = parent.frame())  {

  # source: Gmisc
  # author: Max Gordon <max@gforge.se>

  if (quote)
    args <- lapply(args, enquote)

  if (is.null(names(args)) ||
      is.data.frame(args)){
    argn <- args
    args <- list()
  }else{
    # Add all the named arguments
    argn <- lapply(names(args)[names(args) != ""], as.name)
    names(argn) <- names(args)[names(args) != ""]
    # Add the unnamed arguments
    argn <- c(argn, args[names(args) == ""])
    args <- args[names(args) != ""]
  }

  if (class(what) == "character"){
    if(is.character(what)){
      fn <- strsplit(what, "[:]{2,3}")[[1]]
      what <- if(length(fn)==1) {
        get(fn[[1]], envir=envir, mode="function")
      } else {
        get(fn[[2]], envir=asNamespace(fn[[1]]), mode="function")
      }
    }
    call <- as.call(c(list(what), argn))
  }else if (class(what) == "function"){
    f_name <- deparse(substitute(what))
    call <- as.call(c(list(as.name(f_name)), argn))
    args[[f_name]] <- what
  }else if (class(what) == "name"){
    call <- as.call(c(list(what, argn)))
  }

  eval(call,
       envir = args,
       enclos = envir)

}
