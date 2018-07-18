## example
library(tibble)
library(tbrf)
library(dplyr)
library(ggplot2)
library(ggalt)


## binomial test
df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
  value = rbinom(100, 1, 0.25)
)

df_new <- tbrf::roll_binom(df,
                           x = value,
                           tcolumn = date,
                           unit = "years",
                           n = 5,
                           alpha = 0.1)


## geometric mean
df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
  value = rexp(100, 0.2)
)

df_new <- tbr_gmean(.tbl = df,
                           x = value,
                           tcolumn = date,
                           unit = "years",
                           n = 5,
                           conf.level = 0.95)

## sum
df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
  value = rnorm(100, 0.2)
)
df
df <- df %>%
  tbr_sum(x = value, tcolumn = date, unit = "weeks", n = 54)
df

df <- data.frame(date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
                 value = rnorm(100, 0.2))
str(df)
df <- df %>%
  tbr_sum(x = value, tcolumn = date, unit = "weeks", n = 54)
df

df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
  value = rnorm(100, 0.2)
)

df_new <- tbrf::tbr_sum(df,
                         x = value,
                         tcolumn = date,
                         unit = "weeks",
                         n = 54)



str(df_new)

##regression

df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2015/12/30'), by = "day"), 100),
  x = rnorm(100, 0.2),
  y = rexp(100, 0.25)
)


library(dplyr)
df_new <- tbr_lm(data = df, tcolumn = date, unit = "years", n = 5)
