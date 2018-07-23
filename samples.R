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
                           n = 5)

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



df <- tibble::data_frame(
  date = sample(seq(as.Date('2000-01-01'), as.Date('2010/12/30'), by = "day"), 10),
  value = rexp(10, 1/100)
)
df_new <- tbr_gmean(.tbl = df,
                    x = value,
                    tcolumn = date,
                    unit = "years",
                    n = 5,
                    conf.level = 0.95)

df_new <- tbr_gmean(.tbl = df,
                    x = value,
                    tcolumn = date,
                    unit = "years",
                    n = 5,
                    conf.level = NA)
df_new <- tbr_gmean(.tbl = df,
                    x = value,
                    tcolumn = date,
                    unit = "years",
                    n = 5,
                    method = "classic",
                    conf.level = NA)
df_new <- tbr_mean(.tbl = df,
                    x = value,
                    tcolumn = date,
                    unit = "years",
                    n = 5,
                    conf.level = 0.95)
df_new <- tbr_sd(.tbl = df,
                 x = value,
                 tcolumn = date,
                 unit = "years",
                 n = 5)



df <- data_frame(date = sample(seq(as.Date('2000-01-01'),
                                   as.Date('2005-12-30'), by = "day"), 25)) %>%
  bind_rows(data.frame(date = sample(seq(as.Date('2009-01-01'),
                                         as.Date('2011-12-30'), by = "day"), 25))) %>%
  arrange(date) %>%
  mutate(value = 1:50)


df <- df %>%
  tbr_misc(x = value, tcolumn = date, unit = "years", n = 5, func = sum) %>%
  tbr_misc(x = value, tcolumn = date, unit = "years", n = 5, func = length)

library(ggplot2)
ggplot(df) +
  geom_point(aes(date, value)) +
  geom_errorbarh(aes(xmin = min_date, xmax = max_date, y = value, color = results1)) +
  scale_color_distiller(type = "seq", palette = "OrRd", direction = 1) +
  theme_minimal() +
  guides(color = guide_colorbar(title = "Number of samples"))

Dissolved_Oxygen
df <- Dissolved_Oxygen %>%
  tbr_misc(x = Average_DO, tcolumn = Date, unit = "years", n = 5, func = range)


