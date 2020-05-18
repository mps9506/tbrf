<!-- README.md is generated from README.Rmd. Please edit that file -->

tbrf
====

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/tbrf)](https://CRAN.R-project.org/package=tbrf)
[![R build
status](https://github.com/mps9506/tbrf/workflows/R-CMD-check/badge.svg)](https://github.com/mps9506/tbrf/actions)
[![Travis build
status](https://travis-ci.org/mps9506/tbrf.svg?branch=master)](https://travis-ci.org/mps9506/tbrf)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mps9506/tbrf?branch=master&svg=true)](https://ci.appveyor.com/project/mps9506/tbrf)
[![Coverage
status](https://codecov.io/gh/mps9506/tbrf/branch/master/graph/badge.svg)](https://codecov.io/github/mps9506/tbrf?branch=master)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end --> The goal of tbrf is to provide time-window based
rolling statistical functions. The package differs from other rolling
statistic packages because the intended use is for irregular measured
data. Although tbrf can be used to apply statistical functions to
regularly sampled data, [`zoo`](https://CRAN.R-project.org/package=zoo),
[`RcppRoll`](https://cran.r-project.org/package=RcppRoll), and other
packages provide fast, efficient, and rich implementations of
rolling/windowed functions.

An appropriate example case is water quality data that is measured at
irregular time intervals. Regulatory compliance is often based on a
statistical average measure or exceedance probability applied to all
samples collected in the previous 7-years. tbrf can be used to display
regulatory status at any sample point.

tbrf identifies the previous n measurements within the specified time
window, applies the function, and outputs a variable with the result of
the rolling statistical measure.

Installation
------------

tbrf is available on CRAN:

    install.packages("tbrf")

The development version is maintained on github and can be installed as:

    install.packages(remotes)
    remotes::install_github("mps9506/tbrf")

Available Functions
-------------------

-   `tbr_binom`: Rolling binomial probability with confidence intervals.

-   `tbr_gmean`: Rolling geometric mean with confidence intervals.

-   `tbr_mean`: Rolling mean with confidence intervals.

-   `tbr_median`: Rolling median with confidence intervals.

-   `tbr_misc`: Accepts user specified function.

-   `tbr_sd`: Rolling standard deviation.

-   `tbr_sum`: Rolling sum.

Usage
-----

See:

<a href="https://mps9506.github.io/tbrf/" class="uri">https://mps9506.github.io/tbrf/</a>

Example
-------

Plot a rolling 1-hour mean:

    library(tbrf)
    library(dplyr)
    library(ggplot2)
    library(ggalt)

    y = 3 * sin(2 * seq(from = 0, to = 4*pi, length.out = 100)) + rnorm(100)
    time = sample(seq(as.POSIXct(strptime("2017-01-01 00:01:00", "%Y-%m-%d %H:%M:%S")),
                      as.POSIXct(strptime("2017-01-01 23:00:00", "%Y-%m-%d %H:%M:%S")),
                      by = "min"), 100)

    df <- tibble(y, time)

    df %>%
      tbr_mean(y, time, "hours", n = 1) %>%
      ggplot() +
      geom_point(aes(time, y)) +
      geom_step(aes(time, mean))

<img src="man/figures/README-tbr_hour-1.png" width="672" />

Plot a rolling 3-hour mean:

    df %>%
      tbr_mean(y, time, "hours", n = 3) %>%
      ggplot() +
      geom_point(aes(time, y)) +
      geom_step(aes(time, mean))

<img src="man/figures/README-tbr_threehour-1.png" width="672" />

Contributing
------------

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/mps9506/tbrf/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

License
-------

tbrf code is released under GPL-3 | LICENSE.md

`binom_ci()` is an implementation of code licensed under GPL (&gt;=2) by
Frank Harrell’s [`Hmisc`](https://github.com/harrelfe/Hmisc) package.

If you can cite the use of this software, please use `citation("tbrf")`
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3727319.svg)](https://doi.org/10.5281/zenodo.3727319.)

Test Results
------------

    library(tbrf)

    date()
    ## [1] "Mon May 18 19:33:59 2020"

    devtools::test()
    ## ✔ |  OK [31mF[39m [35mW[39m [34mS[39m | Context
    ## ⠏ |   0       | core functions work in piped workflow⠋ |   1       | core functions work in piped workflow⠹ |   3       | core functions work in piped workflow⠴ |   6       | core functions work in piped workflow[32m✔[39m |   6       | core functions work in piped workflow[36m [0.4 s][39m
    ## ⠏ |   0       | core functions return expected errors and messages⠼ |   5       | core functions return expected errors and messages[32m✔[39m |   7       | core functions return expected errors and messages[36m [0.1 s][39m
    ## ⠏ |   0       | core functions return expected structures and values⠋ |   0   1   | core functions return expected structures and values⠸ |   3   1   | core functions return expected structures and values⠴ |   5   1   | core functions return expected structures and values⠦ |   6   1   | core functions return expected structures and values[32m✔[39m |   6   1   | core functions return expected structures and values[36m [1.6 s][39m
    ## ────────────────────────────────────────────────────────────────────────────────
    ## [1mtest-expectedValues.R:26: [35mwarning[39m: tbr_mean provides same results as mean[22m
    ## Column `results` has different attributes on LHS and RHS of join
    ## ────────────────────────────────────────────────────────────────────────────────
    ## ⠏ |   0       | internal statistical functions return expected values⠙ |   2       | internal statistical functions return expected values⠹ |   3       | internal statistical functions return expected values⠼ |   5       | internal statistical functions return expected values⠇ |   9       | internal statistical functions return expected values⠦ |  17       | internal statistical functions return expected values[32m✔[39m |  17       | internal statistical functions return expected values[36m [0.9 s][39m
    ## 
    ## ══ [1mResults[22m ═════════════════════════════════════════════════════════════════════
    ## [36mDuration: 3.1 s[39m
    ## 
    ## OK:       [32m36[39m
    ## Failed:   [32m0[39m
    ## Warnings: [35m1[39m
    ## Skipped:  [32m0[39m
