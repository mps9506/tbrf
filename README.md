
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tbrf

[![Travis build
status](https://travis-ci.org/mps9506/tbrf.svg?branch=master)](https://travis-ci.org/mps9506/tbrf)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The goal of tbrf is to provide time-window based rolling statistical
functions. The package differs from other rolling statistic packages
because the intended use is for irregular measured data. Althogh tbrf
can be used to apply statistical functions to regularly sampled data,
[`zoo`](https://CRAN.R-project.org/package=zoo) provides a richer
environment for working with time series data.

An appropriate example case is water quality data that is measured at
irregular time intervals. Regulatory compliance is often based on a
statistical average measure or exceedance probability applied to all
samples collected in the previous 7-years. tbrf can be used to display
regulatory status at any sample point.

tbrf identifies the previous n measurements within the specified time
window, applies the function, and outputs a varaible with the result of
the rolling statistical measure.

## Installation

tbrf is still under active development (do not expect stable behavior)
and can be installed from github with:

``` r
devtools::install.github("mps9506/tbrf")
```

## Available Functions

`tbr_binom`: Rolling binomal probability with confidence intervals.
`tbr_gmean`: Rolling geometric mean with confidence intervals.
`tbr_mean`: Rolling mean with confidence intervals. `tbr_median`:
Rolling median with confidence intervals. `tbr_misc`: Accepts user
specified function. `tbr_sd`: Rolling standard deviation. `tbr_sum`:
Rolling sum.

## Examples

Visualize the number of samples included in a tbrf time window:

``` r
library(tbrf)
library(dplyr)
library(ggplot2)

# Some sample data
df <- data_frame(date = sample(seq(as.Date('2000-01-01'),
                                   as.Date('2005-12-30'), by = "day"), 25)) %>%
  bind_rows(data.frame(date = sample(seq(as.Date('2009-01-01'),
                                         as.Date('2011-12-30'), by = "day"), 25))) %>%
  arrange(date) %>%
  mutate(value = 1:50)

# Use length function in tbr_misc to calculate n samples used in the rolling function
df <- df %>%
  tbr_misc(x = value, tcolumn = date, unit = "years", n = 5, func = length)

ggplot(df) +
  geom_point(aes(date, value)) +
  geom_errorbarh(aes(xmin = min_date, xmax = max_date, 
                     y = value, color = results)) +
  scale_color_distiller(type = "seq", palette = "OrRd", 
                        direction = 1) +
  guides(color = guide_colorbar(title = "Number of samples")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Sample Date", y = "Sample Value",
       title = "Window length and n used by tbrf",
       caption = "Lines depict time window used in the function\nColors indicate number of samples in the time window")
```

<img src="man/figures/README-tbr_misc-1.png" width="672" />

A more general use case, visualize the 5 year geometric mean:

``` r
data("Dissolved_Oxygen")

df <- Dissolved_Oxygen %>%
  tbr_mean(x = Average_DO, tcolumn = Date, unit = "years", n = 5)

ggplot(df) +
  geom_point(aes(Date, Average_DO)) +
  geom_line(aes(Date, mean)) +
  geom_ribbon(aes(Date, ymin = lwr.ci, ymax = upr.ci), alpha = 0.5) +
  theme_minimal() +
   labs(x = "Sample Date", y = "Concentration (mg/L)",
       title = "5-yr rolling mean",
       caption = "Line depicts the 5-yr mean\nShaded area indicates the 95% CI")
```

<img src="man/figures/README-dissolved_oxygen-1.png" width="672" />

## Contributing

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

tbrf code is released under GPL-3 | LICENSE.md

`binconf()` is licensed under GPL (\>=2) by Frank Harrell’s
[`Hmisc`](https://github.com/harrelfe/Hmisc) package.

`Gmean()`, `MeanCI()`, `MedianCI()`,`SignTest()`, and `Winsorize()` are
licensed under GPL (\>= 2) by Andri Signorell’s
[DescTools](https://cran.r-project.org/package=DescTools) package.

## Test Results

``` r
library(tbrf)

date()
## [1] "Wed Jul 25 15:47:48 2018"

devtools::test()
## v | OK F W S | Context
## 
/ |  0       | core functions return expected structures
- |  1       | core functions return expected structures
\ |  2       | core functions return expected structures
| |  3       | core functions return expected structures
/ |  4       | core functions return expected structures
- |  5       | core functions return expected structures
\ |  6       | core functions return expected structures
| |  7       | core functions return expected structures
v |  7       | core functions return expected structures [3.7 s]
## 
/ |  0       | core functions return expected errors and messages
- |  1       | core functions return expected errors and messages
\ |  2       | core functions return expected errors and messages
| |  3       | core functions return expected errors and messages
/ |  4       | core functions return expected errors and messages
- |  5       | core functions return expected errors and messages
\ |  6       | core functions return expected errors and messages
| |  7       | core functions return expected errors and messages
v |  7       | core functions return expected errors and messages
## 
## == Results ==========================================================================================
## Duration: 3.8 s
## 
## OK:       14
## Failed:   0
## Warnings: 0
## Skipped:  0
```
