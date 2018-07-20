
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tbrf

The goal of tbrf is to provide time-window based rolling statistical
functions. The package differs from other rolling statistic packages
because the intended use is for irregular measured data.

An example use case is water quality data that is measured at irregular
time intervals. Regulatory compliance is often based on a statistical
average measure or exceedance probability applied to all samples
collected in the previous 7-years.

tbrf identifies the previous n measurements within the specified time
window, applies the function, and outputs a varaible with the result of
the rolling statistical measure.

## Installation

tbrf is still under development and can be installed from github with:

``` r
devtools::install.github("mps9506/tbrf")
```

## Examples

### Rolling Geometric Mean

``` r
library(tbrf)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)

## Generate sample data
set.seed(100)
df <- data.frame(date = sample(seq(as.Date('2000-01-01'), 
                                   as.Date('2017-12-30'), by = "day"), 68),
                 value = rexp(68, 1/100))


head(df)
##         date     value
## 1 2005-07-16 161.34566
## 2 2004-08-20  14.51295
## 3 2009-12-08  43.30075
## 4 2001-01-05  54.95578
## 5 2008-06-05  66.80542
## 6 2008-09-12 254.36060

## apply the geomean to each row
df <- tbr_gmean(df, x = value, tcolumn = date,
                unit = "years", n = 5, conf.level = 0.95)
df
## # A tibble: 68 x 5
##    date       value  mean lwr.ci upr.ci
##    <date>     <dbl> <dbl>  <dbl>  <dbl>
##  1 2001-01-05  55.0  NA     NA     NA  
##  2 2002-03-15  58.6  56.8   55.0   58.6
##  3 2002-05-01 198.   86.1   37.4  135. 
##  4 2003-01-22 113.   92.0   56.8  149. 
##  5 2003-01-27  68.0  86.6   58.9  128. 
##  6 2003-03-25 153.   95.3   64.4  135. 
##  7 2003-07-19 169.  103.    70.4  150. 
##  8 2003-09-03 114.  105.    75.6  145. 
##  9 2003-09-17  63.0  98.9   74.2  134. 
## 10 2003-10-09  21.2  84.8   57.4  126. 
## # ... with 58 more rows


## tidy pipeline sample. sample data with breaks
set.seed(100)

df <- data.frame(date = sample(seq(as.Date('2000-01-01'), 
                                   as.Date('2007-12-30'), by = "day"), 25),
                 value = rexp(25, 1/100)) %>%
  bind_rows(data.frame(date = sample(seq(as.Date('2009-01-01'), 
                                         as.Date('2011-12-30'), by = "day"), 5),
                       value = rexp(5, 1/1000)))

df <- df %>%
  tbr_gmean(x = value, tcolumn = date, unit = "years",
            n = 5, conf.level = 0.95)
df
## # A tibble: 30 x 5
##    date       value  mean lwr.ci upr.ci
##    <date>     <dbl> <dbl>  <dbl>  <dbl>
##  1 2000-06-13  93.5  NA     NA     NA  
##  2 2001-05-10  20.7  44.0   20.7   93.5
##  3 2001-08-17 223.   75.5   25.6  276. 
##  4 2002-01-22  54.1  69.4   30.9  144. 
##  5 2002-03-26 102.   74.9   39.8  166. 
##  6 2002-06-17 176.   86.4   47.1  176. 
##  7 2002-11-04 114.   89.9   55.0  164. 
##  8 2002-11-09  35.3  80.0   47.7  135. 
##  9 2002-12-15  55.5  76.8   48.6  122. 
## 10 2003-03-04 198.   84.4   52.4  135. 
## # ... with 20 more rows

ggplot(df) +
  geom_point(aes(date, value)) +
  geom_line(aes(date, mean)) +
  geom_ribbon(aes(x = date, ymin = lwr.ci, ymax = upr.ci), alpha = 0.4) +
  scale_y_log10()
## Warning: Removed 1 rows containing missing values (geom_path).
```

<img src="man/figures/README-example-1.png" width="672" />

### Test Results

``` r
library(tbrf)

date()
## [1] "Fri Jul 20 16:47:24 2018"

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
v |  6       | core functions return expected structures [3.7 s]
## 
/ |  0       | core functions return expected errors and messages
- |  1       | core functions return expected errors and messages
\ |  2       | core functions return expected errors and messages
| |  3       | core functions return expected errors and messages
/ |  4       | core functions return expected errors and messages
- |  5       | core functions return expected errors and messages
\ |  6       | core functions return expected errors and messages
v |  6       | core functions return expected errors and messages
## 
## == Results =========================================================================================
## Duration: 3.7 s
## 
## OK:       12
## Failed:   0
## Warnings: 0
## Skipped:  0
```
