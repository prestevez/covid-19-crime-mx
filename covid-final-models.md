---
title: "Crime and COVID-19: Effects of changes in routine activities in Mexico City"
subitle: "Replication materials, v0.01"
author: "Patricio R Estévez-Soto"
date: "2020-07-24"
output: html_document
---




```r
require(tidyverse)
```

```
## Loading required package: tidyverse
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✔ ggplot2 3.3.2     ✔ purrr   0.3.4
## ✔ tibble  3.0.3     ✔ dplyr   1.0.0
## ✔ tidyr   1.1.0     ✔ stringr 1.4.0
## ✔ readr   1.3.1     ✔ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```
##
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
##
##     date, intersect, setdiff, union
```

```r
require(magrittr)
```

```
## Loading required package: magrittr
```

```
##
## Attaching package: 'magrittr'
```

```
## The following object is masked from 'package:purrr':
##
##     set_names
```

```
## The following object is masked from 'package:tidyr':
##
##     extract
```

```r
require(broom)
```

```
## Loading required package: broom
```

```r
require(ggthemes)
```

```
## Loading required package: ggthemes
```

```r
kable <- knitr::kable

adf.test <- tseries::adf.test
```

```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```r
white.test <- tseries::white.test

Sys.setenv("TZ"="Europe/London")
require(fable)
```

```
## Loading required package: fable
```

```
## Loading required package: fabletools
```

```r
require(tsibble)
```

```
## Loading required package: tsibble
```

```
##
## Attaching package: 'tsibble'
```

```
## The following object is masked from 'package:lubridate':
##
##     interval
```

```r
require(feasts)
```

```
## Loading required package: feasts
```

```r
## multiprocessing
require(future)
```

```
## Loading required package: future
```

```r
plan(multiprocess, workers = 6)


## requires future.apply to be installed

# install.packages("future.apply")

## geom_hex requires hexbin package

# install.packages("hexbin")

# install.packages("cowplot")
require(cowplot)
```

```
## Loading required package: cowplot
```

```
##
## ********************************************************
```

```
## Note: As of version 1.0.0, cowplot does not change the
```

```
##   default ggplot2 theme anymore. To recover the previous
```

```
##   behavior, execute:
##   theme_set(theme_cowplot())
```

```
## ********************************************************
```

```
##
## Attaching package: 'cowplot'
```

```
## The following object is masked from 'package:ggthemes':
##
##     theme_map
```

```
## The following object is masked from 'package:lubridate':
##
##     stamp
```

```r
# load("tmp.Rdata")

require(scales)
```

```
## Loading required package: scales
```

```
##
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
##
##     discard
```

```
## The following object is masked from 'package:readr':
##
##     col_factor
```



# Introduction

This document contains the replication materials for the study "Crime and COVID-19: Effects of changes in routine activities in Mexico City". The study is available as a pre-print here: ADDRESS.

This is version 0.01 of the replication materials.


# Data

Crime data come from Mexico City's Open Data portal and represent criminal investigations undertaken by the City's criminal prosecutor's office. Violence against women (VAW) calls come from a dedicated phone service. Passenger numbers in public transport come from the same open data portal and count Bus Rapid Transit and Metro passengers (as well as a measure combining total passengers of both services). Only total passenger counts were used in the analysis.


```r
daily_crime_mobility <- read_csv("https://raw.githubusercontent.com/prestevez/covid-19-crime-mx/master/data/daily_crime_mobility-2020-07-13.csv") %>% as_tsibble()
```

```
## Parsed with column specification:
## cols(
##   fecha = col_date(format = ""),
##   all_crimes = col_double(),
##   año = col_double(),
##   mes = col_character(),
##   diasem = col_character(),
##   violent_robbery = col_double(),
##   non_violent_robbery = col_double(),
##   burglary = col_double(),
##   violent_crime = col_double(),
##   sex_crime = col_double(),
##   domestic_violence = col_double(),
##   calls_vaw = col_double(),
##   metrobus = col_double(),
##   metro = col_double(),
##   pas_total = col_double()
## )
```

```
## Using `fecha` as index variable.
```

```r
covid_date <- ymd("2020-02-29")

covid_lockdown <- ymd("2020-03-23")
```


# Data preparation

Observations for the last week of May were discarded to control for the lag in reporting that may affect the most recent figures.

Data were further split into two periods. The "training" data set used to estimate models before the impact of COVID-19 restrictions represent all observations from Jan. 1, 2017 to Feb 28, 2020, the day before the first COVID-19 cases were reported in the country.


```r
daily_crime_mobility %>%
  filter(fecha < "2020-05-25") %>%
  select(-c(año, mes, diasem)) %>%
  select(date = fecha,
         "All crimes" = all_crimes,
         "Violent robbery" = violent_robbery,
         "Non-violent robbery" = non_violent_robbery,
         "Robbery against residence" = burglary,
         "Serious violent crime (non-sexual)" = violent_crime,
         "Sexual violence" = sex_crime,
         "Domestic violence" = domestic_violence,
         "VAW helpline calls" = calls_vaw,
         "BRT + SCT passengers" = pas_total) %>%
  mutate("BRT + SCT passengers (in millions)" = `BRT + SCT passengers`/1e6) -> daily_crime_mobility

# The names of columns were changed for aesthetic purposes

# split data into periods

daily_crime_mobility_pre_covid <- daily_crime_mobility %>%
  filter(date < covid_date)

daily_crime_mobility_post_covid <-daily_crime_mobility %>%
  filter(date >= covid_date)

daily_crime_mobility_2020 <- daily_crime_mobility %>%
  filter(date >= "2020-01-01")
```


Data summaries for the three data sets are presented next:


```r
range_as_text <- function(x){
  rangex <- label_number()(range(x, na.rm = TRUE))
  paste0("\n[", rangex[1], "-", rangex[2], "]")
}

mean_sd <- function(x){
  meanx <- label_number(accuracy = 0.1)(mean(x, na.rm = TRUE))
  sdx <- label_number(accuracy = 0.1)(sd(x, na.rm = TRUE))

  paste0(meanx, " (", sdx, ")")
}

# order for variables

var_names <- c("All crimes",
  "Violent robbery",
  "Non-violent robbery",
  "Robbery against residence",
  "Serious violent crime (non-sexual)",
  "Sexual violence",
  "Domestic violence",
  "VAW helpline calls",
  "BRT + SCT passengers",
  "BRT + SCT passengers (in millions)")

daily_crime_mobility %>%
  as_tibble() %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise("Mean (sd)" = mean_sd(value),
            Range = range_as_text(value)) %>%
  mutate(Variable = factor(Variable, levels = var_names[-9])) %>%
  arrange(Variable) %>%
  kable(digits = 2,
        align = c("l", "r", "r", "r"),
        caption = "Summary table for entire period.")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```



Table: Summary table for entire period.

|Variable                           |     Mean (sd)|     Range|
|:----------------------------------|-------------:|---------:|
|All crimes                         | 638.9 (111.2)| [259-942]|
|Violent robbery                    |  190.3 (47.4)|  [50-309]|
|Non-violent robbery                |   91.1 (19.3)|  [32-140]|
|Robbery against residence          |    18.3 (5.6)|    [1-36]|
|Serious violent crime (non-sexual) |    31.4 (8.0)|    [9-70]|
|Sexual violence                    |    12.7 (6.7)|    [1-42]|
|Domestic violence                  |   59.9 (16.0)|  [22-149]|
|VAW helpline calls                 |   46.7 (16.6)|   [8-151]|
|BRT + SCT passengers (in millions) |     5.0 (1.3)| [0.8-6.6]|

```r
daily_crime_mobility_pre_covid %>%
  as_tibble() %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise("Mean (sd)" = mean_sd(value),
            Range = range_as_text(value)) %>%
  mutate(Variable = factor(Variable, levels = var_names[-9])) %>%
  arrange(Variable) %>%
  kable(digits = 2,
        align = c("l", "r", "r", "r"),
        caption = "Summary table for pre-COVID-19 period.")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```



Table: Summary table for pre-COVID-19 period.

|Variable                           |    Mean (sd)|     Range|
|:----------------------------------|------------:|---------:|
|All crimes                         | 654.3 (91.9)| [332-942]|
|Violent robbery                    | 195.9 (42.8)|  [50-309]|
|Non-violent robbery                |  93.4 (17.1)|  [32-140]|
|Robbery against residence          |   19.0 (5.0)|    [5-36]|
|Serious violent crime (non-sexual) |   32.0 (7.6)|   [13-70]|
|Sexual violence                    |   12.6 (6.6)|    [1-42]|
|Domestic violence                  |  59.8 (15.5)|  [22-122]|
|VAW helpline calls                 |  44.1 (13.0)|   [8-131]|
|BRT + SCT passengers (in millions) |    5.2 (1.1)| [1.8-6.6]|

```r
daily_crime_mobility_post_covid %>%
  as_tibble() %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise("Mean (sd)" = mean_sd(value),
            Range = range_as_text(value)) %>%
  mutate(Variable = factor(Variable, levels = var_names[-9])) %>%
  arrange(Variable) %>%
  kable(digits = 2,
        align = c("l", "r", "r", "r"),
        caption = "Summary table for post-COVID-19 period.")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```



Table: Summary table for post-COVID-19 period.

|Variable                           |     Mean (sd)|     Range|
|:----------------------------------|-------------:|---------:|
|All crimes                         | 433.0 (139.7)| [259-748]|
|Violent robbery                    |  113.8 (39.7)|  [54-218]|
|Non-violent robbery                |   60.4 (20.3)|  [33-120]|
|Robbery against residence          |     9.0 (4.9)|    [1-22]|
|Serious violent crime (non-sexual) |    22.7 (8.3)|    [9-46]|
|Sexual violence                    |    13.4 (7.8)|    [3-32]|
|Domestic violence                  |   62.4 (21.3)|  [33-149]|
|VAW helpline calls                 |   81.5 (20.4)|  [39-151]|
|BRT + SCT passengers (in millions) |     2.4 (1.5)| [0.8-6.1]|

```r
daily_crime_mobility_pre_covid %>%
  as_tibble() %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise("Pre-COVID-19 mean (sd)" = mean_sd(value)) %>%
  mutate(Variable = factor(Variable, levels = var_names[-9])) %>%
  arrange(Variable) -> pre_summary
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
daily_crime_mobility_post_covid %>%
  as_tibble() %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise("Post-COVID-19 mean (sd)" = mean_sd(value)) %>%
  mutate(Variable = factor(Variable, levels = var_names[-9])) %>%
  arrange(Variable) -> post_summary
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
pre_summary %>%
  left_join(post_summary) %>%
  kable(align = c("l", "r", "r", "r"),
        caption = "Summary table for pre- and post-COVID-19 periods.")
```

```
## Joining, by = "Variable"
```



Table: Summary table for pre- and post-COVID-19 periods.

|Variable                           | Pre-COVID-19 mean (sd)| Post-COVID-19 mean (sd)|
|:----------------------------------|----------------------:|-----------------------:|
|All crimes                         |           654.3 (91.9)|           433.0 (139.7)|
|Violent robbery                    |           195.9 (42.8)|            113.8 (39.7)|
|Non-violent robbery                |            93.4 (17.1)|             60.4 (20.3)|
|Robbery against residence          |             19.0 (5.0)|               9.0 (4.9)|
|Serious violent crime (non-sexual) |             32.0 (7.6)|              22.7 (8.3)|
|Sexual violence                    |             12.6 (6.6)|              13.4 (7.8)|
|Domestic violence                  |            59.8 (15.5)|             62.4 (21.3)|
|VAW helpline calls                 |            44.1 (13.0)|             81.5 (20.4)|
|BRT + SCT passengers (in millions) |              5.2 (1.1)|               2.4 (1.5)|

Next we plot the period from Jan 01 to May 24 for every year/variable to highlight how crimes have changed during the pandemic.



```r
num_days <- daily_crime_mobility %>%
  filter(date >= "2020-01-01" &
           date < "2020-05-25") %>% nrow


daily_crime_mobility %>%
  as_tibble %>%
  select(date) %>%
  mutate(year = year(date),
         t = yday(date),
         mth = month(date, label = TRUE)) %>%
  filter(year == 2020, t <= num_days) %>%
  group_by(mth) %>%
  summarise(t_ind = first(t)) -> mth_t
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
daily_crime_mobility %>%
  as_tibble %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  mutate(Variable = factor(Variable, levels = var_names),
         year = year(date),
         t = yday(date)) %>%
  filter(date == covid_date | date == covid_lockdown) %$% t %>% unique
```

```
## [1] 60 83
```

```r
tibble(Variable = factor("All crimes"),
       t = c(49, 77.5),
       value = 900,
       labels = c("First cases in Mexico", "Lockdown")) -> covid_annotations

daily_crime_mobility %>%
  as_tibble %>%
  select(-`BRT + SCT passengers`) %>%
  pivot_longer(-date, names_to = "Variable") %>%
  mutate(Variable = factor(Variable, levels = var_names),
         year = year(date),
         t = yday(date)) %>%
  filter(t <= num_days) %>%
  select(-date) %>%
  pivot_wider(names_from = year) %>%
  pivot_longer(-c(Variable, t, `2020`),
               names_to = "Year") %>%
  ggplot(aes(t, value)) +
  geom_line(aes(colour = Year),
            alpha = 0.5, size = 0.25) +
  geom_line(aes(y = `2020`, colour = "2020")) +
  facet_wrap(~ Variable,
             ncol = 1,
             scale = "free_y") +
  # geom_smooth(se = FALSE, alpha = 0.5, linetype = 2) +
  geom_vline(xintercept = 60, linetype = 2, size = 0.3) +
  geom_vline(xintercept = 83, linetype = 2, size = 0.3) +
  geom_text(data = covid_annotations,
             aes(label = labels,
                 fontface = 3),
             size = 2.5,
             nudge_y = -20) +
  theme_clean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  scale_color_colorblind(NULL) +
  scale_x_continuous(NULL,
                     breaks = mth_t$t_ind,
                     labels = mth_t$mth) +
  ylab(NULL) -> temporal_eda_comparison

ggsave(filename = "plots/temporal_eda_comparison.pdf",
       plot = temporal_eda_comparison,
       device = cairo_pdf,
       width = 8,
       height = 11)
```

![](plots/temporal_eda_comparison.pdf)

# ARIMA Forecasting

This section contains the comparison of the observed counts with those expected based on ARIMA forecasting. Most series used simple ARIMA models (i.e. with stochastic trends). However, sex crimes, domestic violence and VAW helpline calls required the use of a deterministic trend. We do the analysis per crime type first, and then join results to construct the final tables and plots.

- **All crimes**



```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`All crimes`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`All crimes`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_all_crimes

arima_all_crimes %>%
  select(stochastic) %>%
  report
```

```
## Series: All crimes
## Model: ARIMA(3,0,0)(0,1,1)[7]
## Transformation: log(.x)
##
## Coefficients:
##          ar1     ar2     ar3     sma1
##       0.3590  0.1761  0.0819  -0.9293
## s.e.  0.0298  0.0313  0.0298   0.0145
##
## sigma^2 estimated as 0.00804:  log likelihood=1133.62
## AIC=-2257.24   AICc=-2257.19   BIC=-2232.02
```

```r
arima_all_crimes %>%
  select(deterministic) %>%
  report
```

```
## Series: All crimes
## Model: LM w/ ARIMA(1,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1    sar2  trend()  intercept
##       0.6797  -0.3887  0.4126  0.3392    1e-04     6.4325
## s.e.  0.0609   0.0777  0.0279  0.0282    1e-04     0.0425
##
## sigma^2 estimated as 0.0101:  log likelihood=1014.28
## AIC=-2014.56   AICc=-2014.46   BIC=-1979.2
```

```r
(arima_all_crimes %>%
  accuracy -> arima_all_crimes_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE    MPE  MAPE  MASE    ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>
## 1 stochastic    Training  3.75  55.5  39.8 -0.142  6.30 0.716 -0.0468
## 2 deterministic Training  3.36  62.3  46.4 -0.459  7.37 0.835 -0.0243
```

```r
arima_all_crimes %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_all_crimes_forecast

(arima_all_crimes_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_all_crimes_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -250.  286.  254. -72.0  72.6   NaN 0.916
## 2 stochastic    Test  -191.  237.  207. -56.7  59.1   NaN 0.891
```

```r
# Stochastic model is better

arima_all_crimes_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "All crimes",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_all_crimes_final_forecast

arima_all_crimes %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "All crimes",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_all_crimes_training_forecast
```


- **Violent robbery**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Violent robbery`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Violent robbery`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_violent_robbery

arima_violent_robbery %>%
  select(stochastic) %>%
  report
```

```
## Series: Violent robbery
## Model: ARIMA(1,0,1)(2,1,1)[7]
## Transformation: log(.x)
##
## Coefficients:
##          ar1     ma1    sar1     sar2     sma1
##       0.7902  -0.525  0.0227  -0.0649  -0.8822
## s.e.  0.0750   0.101  0.0344   0.0335   0.0228
##
## sigma^2 estimated as 0.01618:  log likelihood=734.62
## AIC=-1457.24   AICc=-1457.16   BIC=-1426.97
```

```r
arima_violent_robbery %>%
  select(deterministic) %>%
  report
```

```
## Series: Violent robbery
## Model: LM w/ ARIMA(3,0,0)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1     ar2     ar3    sar1    sar2  trend()  intercept
##       0.2655  0.0291  0.0668  0.4677  0.3581    0e+00     5.2142
## s.e.  0.0296  0.0305  0.0295  0.0277  0.0279    1e-04     0.0691
##
## sigma^2 estimated as 0.02077:  log likelihood=597.79
## AIC=-1179.58   AICc=-1179.45   BIC=-1139.17
```

```r
(arima_violent_robbery %>%
  accuracy -> arima_violent_robbery_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE    MPE  MAPE  MASE     ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>
## 1 stochastic    Training  1.42  22.3  16.5 -0.653  9.11 0.751  0.00627
## 2 deterministic Training  2.12  25.5  19.1 -0.909 10.5  0.873 -0.0388
```

```r
arima_violent_robbery %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_violent_robbery_forecast

(arima_violent_robbery_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_violent_robbery_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -73.1  84.2  74.6 -81.7  82.4   NaN 0.878
## 2 stochastic    Test  -51.2  64.5  55.6 -58.1  60.4   NaN 0.777
```

```r
# Stochastic model is better

arima_violent_robbery_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Violent robbery",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_violent_robbery_final_forecast

arima_violent_robbery %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Violent robbery",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_violent_robbery_training_forecast
```


- **Non-violent robbery**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Non-violent robbery`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Non-violent robbery`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_non_violent_robbery

arima_non_violent_robbery %>%
  select(stochastic) %>%
  report
```

```
## Series: Non-violent robbery
## Model: ARIMA(1,1,1)(2,0,0)[7]
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1    sar2
##       0.2151  -0.9868  0.3410  0.2001
## s.e.  0.0295   0.0043  0.0296  0.0295
##
## sigma^2 estimated as 0.02515:  log likelihood=487.29
## AIC=-964.58   AICc=-964.53   BIC=-939.33
```

```r
arima_non_violent_robbery %>%
  select(deterministic) %>%
  report
```

```
## Series: Non-violent robbery
## Model: LM w/ ARIMA(1,0,0)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1    sar1    sar2  trend()  intercept
##       0.2394  0.3671  0.2272    0e+00     4.5259
## s.e.  0.0293  0.0293  0.0291    1e-04     0.0295
##
## sigma^2 estimated as 0.02544:  log likelihood=482.27
## AIC=-952.53   AICc=-952.46   BIC=-922.23
```

```r
(arima_non_violent_robbery %>%
  accuracy -> arima_non_violent_robbery_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE    ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
## 1 stochastic    Training  1.18  13.8  10.7 -1.11  12.2 0.822 -0.0279
## 2 deterministic Training  1.18  13.8  10.8 -1.26  12.3 0.830 -0.0452
```

```r
arima_non_violent_robbery %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_non_violent_robbery_forecast

(arima_non_violent_robbery_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_non_violent_robbery_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -32.3  38.2  34.5 -69.5  71.6   NaN 0.748
## 2 stochastic    Test  -27.0  33.3  29.7 -59.5  62.0   NaN 0.742
```

```r
# Stochastic model is better

arima_non_violent_robbery_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Non-violent robbery",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_non_violent_robbery_final_forecast

arima_non_violent_robbery %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Non-violent robbery",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_non_violent_robbery_training_forecast
```


- **Robbery against residence**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Robbery against residence`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Robbery against residence`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_robbery_against_residence

arima_robbery_against_residence %>%
  select(stochastic) %>%
  report
```

```
## Series: Robbery against residence
## Model: ARIMA(0,1,1)(2,0,0)[7]
## Transformation: log(.x)
##
## Coefficients:
##           ma1    sar1    sar2
##       -0.9777  0.0479  0.0776
## s.e.   0.0057  0.0299  0.0300
##
## sigma^2 estimated as 0.0725:  log likelihood=-123.18
## AIC=254.37   AICc=254.4   BIC=274.57
```

```r
arima_robbery_against_residence %>%
  select(deterministic) %>%
  report
```

```
## Series: Robbery against residence
## Model: LM w/ ARIMA(4,0,0)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1     ar2     ar3     ar4    sar1    sar2  trend()  intercept
##       0.0743  0.0361  0.0232  0.0737  0.0974  0.1276        0     2.9262
## s.e.  0.0297  0.0296  0.0297  0.0296  0.0299  0.0300        0     0.0260
##
## sigma^2 estimated as 0.07506:  log likelihood=-139.51
## AIC=297.02   AICc=297.18   BIC=342.48
```

```r
(arima_robbery_against_residence %>%
  accuracy -> arima_robbery_against_residence_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE       ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl>
## 1 stochastic    Training 0.555  4.81  3.83 -4.31  22.3 0.746  0.0202
## 2 deterministic Training 0.664  4.90  3.88 -4.04  22.6 0.756 -0.0000107
```

```r
arima_robbery_against_residence %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_robbery_against_residence_forecast

(arima_robbery_against_residence_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_robbery_against_residence_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -9.61 10.8   9.85 -194.  195.   NaN 0.642
## 2 stochastic    Test  -7.56  9.00  8.09 -161.  163.   NaN 0.628
```

```r
# Stochastic model is better

arima_robbery_against_residence_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Robbery against residence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_robbery_against_residence_final_forecast

arima_robbery_against_residence %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Robbery against residence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_robbery_against_residence_training_forecast
```


- **Serious violent crime (non-sexual)**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Serious violent crime (non-sexual)`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Serious violent crime (non-sexual)`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_serious_violent_crime

arima_serious_violent_crime %>%
  select(stochastic) %>%
  report
```

```
## Series: Serious violent crime (non-sexual)
## Model: ARIMA(2,1,1)(2,0,0)[7]
## Transformation: log(.x)
##
## Coefficients:
##          ar1     ar2      ma1    sar1    sar2
##       0.1528  0.0832  -0.9856  0.1270  0.1445
## s.e.  0.0304  0.0305   0.0072  0.0303  0.0299
##
## sigma^2 estimated as 0.0516:  log likelihood=73.87
## AIC=-135.74   AICc=-135.66   BIC=-105.43
```

```r
arima_serious_violent_crime %>%
  select(deterministic) %>%
  report
```

```
## Series: Serious violent crime (non-sexual)
## Model: LM w/ ARIMA(3,0,3) errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2     ar3      ma1     ma2      ma3  trend()  intercept
##       2.1966  -2.1520  0.9385  -2.0660  1.9557  -0.8088    1e-04     3.3586
## s.e.  0.0200   0.0339  0.0211   0.0341  0.0544   0.0342    1e-04     0.0586
##
## sigma^2 estimated as 0.04986:  log likelihood=96.24
## AIC=-174.48   AICc=-174.33   BIC=-129.03
```

```r
(arima_serious_violent_crime %>%
  accuracy -> arima_serious_violent_crime_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE     ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
## 1 stochastic    Training 0.847  7.10  5.54 -2.37  18.3 0.759 -0.00697
## 2 deterministic Training 0.755  6.94  5.43 -2.58  18.0 0.744  0.0159
```

```r
arima_serious_violent_crime %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_serious_violent_crime_forecast

(arima_serious_violent_crime_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_serious_violent_crime_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type     ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -11.5   14.4  12.8 -70.8  74.1   NaN 0.656
## 2 stochastic    Test   -8.44  11.7  10.5 -54.9  60.1   NaN 0.631
```

```r
# Stochastic model is better

arima_serious_violent_crime_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Serious violent crime (non-sexual)",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_serious_violent_crime_final_forecast

arima_serious_violent_crime %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Serious violent crime (non-sexual)",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_serious_violent_crime_training_forecast
```


- **Sexual violence**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Sexual violence`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Sexual violence`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_sexual_violence

arima_sexual_violence %>%
  select(stochastic) %>%
  report
```

```
## Series: Sexual violence
## Model: ARIMA(0,1,1) w/ drift
## Transformation: log(.x)
##
## Coefficients:
##           ma1  constant
##       -0.9187    0.0016
## s.e.   0.0129    0.0010
##
## sigma^2 estimated as 0.1765:  log likelihood=-636.22
## AIC=1278.45   AICc=1278.47   BIC=1293.6
```

```r
arima_sexual_violence %>%
  select(deterministic) %>%
  report
```

```
## Series: Sexual violence
## Model: LM w/ ARIMA(1,0,1) errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1  trend()  intercept
##       0.9775  -0.8967   0.0013     1.6129
## s.e.  0.0093   0.0196   0.0002     0.1059
##
## sigma^2 estimated as 0.1752:  log likelihood=-630.75
## AIC=1271.5   AICc=1271.55   BIC=1296.76
```

```r
(arima_sexual_violence %>%
  accuracy -> arima_sexual_violence_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE   ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 stochastic    Training 0.726  4.52  3.38 -11.5  36.3 0.759 0.0457
## 2 deterministic Training 0.818  4.52  3.37 -10.9  35.8 0.757 0.0294
```

```r
arima_sexual_violence %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_sexual_violence_forecast

(arima_sexual_violence_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_sexual_violence_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -15.3  17.1  15.5 -195.  196.   NaN 0.793
## 2 stochastic    Test  -18.5  20.5  18.6 -234.  235.   NaN 0.844
```

```r
# Deterministic model is better

arima_sexual_violence_forecast %>%
  filter(.model == "deterministic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Sexual violence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_sexual_violence_final_forecast


arima_sexual_violence %>%
  select(deterministic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Sexual violence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_sexual_violence_training_forecast
```


- **Domestic violence**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`Domestic violence`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`Domestic violence`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_domestic_violence

arima_domestic_violence %>%
  select(stochastic) %>%
  report
```

```
## Series: Domestic violence
## Model: ARIMA(1,1,1)(2,0,0)[7]
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1    sar2
##       0.2167  -0.9640  0.1907  0.2058
## s.e.  0.0306   0.0089  0.0297  0.0295
##
## sigma^2 estimated as 0.03401:  log likelihood=313.93
## AIC=-617.85   AICc=-617.8   BIC=-592.6
```

```r
arima_domestic_violence %>%
  select(deterministic) %>%
  report
```

```
## Series: Domestic violence
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2  trend()  intercept
##       1.1979  -0.2106  -0.9538  0.1907  0.2073    5e-04     3.8031
## s.e.  0.0332   0.0308   0.0149  0.0297  0.0295    1e-04     0.0599
##
## sigma^2 estimated as 0.03383:  log likelihood=319.26
## AIC=-622.52   AICc=-622.4   BIC=-582.11
```

```r
(arima_domestic_violence %>%
  accuracy -> arima_domestic_violence_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE    ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
## 1 stochastic    Training  1.37  11.0  8.60 -1.01  14.7 0.779 0.00325
## 2 deterministic Training  1.01  10.9  8.56 -1.69  14.8 0.775 0.00428
```

```r
arima_domestic_violence %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_domestic_violence_forecast

(arima_domestic_violence_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_domestic_violence_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -21.9  29.0  25.3 -48.0  51.0   NaN 0.698
## 2 stochastic    Test  -24.3  31.4  27.5 -52.8  55.5   NaN 0.723
```

```r
# Deterministic model is better

arima_domestic_violence_forecast %>%
  filter(.model == "deterministic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "Domestic violence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_domestic_violence_final_forecast

arima_domestic_violence %>%
  select(deterministic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "Domestic violence",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_domestic_violence_training_forecast
```


- **VAW helpline calls**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`VAW helpline calls`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`VAW helpline calls`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_vaw_calls

arima_vaw_calls %>%
  select(stochastic) %>%
  report
```

```
## Series: VAW helpline calls
## Model: ARIMA(2,1,2)(2,0,0)[7]
## Transformation: log(.x)
##
## Coefficients:
##           ar1     ar2      ma1      ma2    sar1    sar2
##       -0.6137  0.2093  -0.1343  -0.7765  0.2338  0.1626
## s.e.   0.0959  0.0344   0.0930   0.0864  0.0314  0.0304
##
## sigma^2 estimated as 0.05257:  log likelihood=60.29
## AIC=-106.59   AICc=-106.49   BIC=-71.24
```

```r
arima_vaw_calls %>%
  select(deterministic) %>%
  report
```

```
## Series: VAW helpline calls
## Model: LM w/ ARIMA(0,0,2)(1,0,1)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ma1     ma2    sar1     sma1  trend()  intercept
##       0.2526  0.1347  0.9294  -0.7399    5e-04     3.4733
## s.e.  0.0304  0.0275  0.0215   0.0412    1e-04     0.0595
##
## sigma^2 estimated as 0.05153:  log likelihood=71.34
## AIC=-128.69   AICc=-128.59   BIC=-93.33
```

```r
(arima_vaw_calls %>%
  accuracy -> arima_vaw_calls_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type       ME  RMSE   MAE   MPE  MAPE  MASE   ACF1
##   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1 stochastic    Training  1.38  9.61  7.46 -1.86  18.3 0.799 0.0399
## 2 deterministic Training  1.12  9.59  7.32 -2.64  18.1 0.785 0.0669
```

```r
arima_vaw_calls %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_vaw_calls_forecast

(arima_vaw_calls_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_vaw_calls_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type     ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  17.6    26.4  21.1 16.5   24.4   NaN 0.391
## 2 stochastic    Test   0.158  19.7  15.0 -6.71  21.0   NaN 0.393
```

```r
# Deterministic model is better

arima_vaw_calls_forecast %>%
  filter(.model == "deterministic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "VAW helpline calls",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_vaw_calls_final_forecast

arima_vaw_calls %>%
  select(deterministic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "VAW helpline calls",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_vaw_calls_training_forecast
```


- **BRT + SCT passengers**


```r
daily_crime_mobility_pre_covid %>%
  model(stochastic = ARIMA(log(`BRT + SCT passengers (in millions)`),
                      stepwise = FALSE,
                      approximation = FALSE),
        deterministic = ARIMA(log(`BRT + SCT passengers (in millions)`) ~ trend(),
                      stepwise = FALSE,
                      approximation = FALSE)) -> arima_passengers

arima_passengers %>%
  select(stochastic) %>%
  report
```

```
## Series: BRT + SCT passengers (in millions)
## Model: ARIMA(4,0,0)(2,0,0)[7] w/ mean
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2     ar3      ar4    sar1    sar2  constant
##       0.2182  -0.1411  0.0975  -0.1221  0.3927  0.1561    0.6907
## s.e.  0.0296   0.0303  0.0305   0.0300  0.0300  0.0300    0.0061
##
## sigma^2 estimated as 0.04377:  log likelihood=164.4
## AIC=-312.8   AICc=-312.68   BIC=-272.4
```

```r
arima_passengers %>%
  select(deterministic) %>%
  report
```

```
## Series: BRT + SCT passengers (in millions)
## Model: LM w/ ARIMA(4,0,0)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2     ar3      ar4    sar1    sar2  trend()  intercept
##       0.2179  -0.1414  0.0972  -0.1224  0.3923  0.1557    0e+00     1.6262
## s.e.  0.0296   0.0303  0.0305   0.0300  0.0300  0.0300    1e-04     0.0282
##
## sigma^2 estimated as 0.0438:  log likelihood=164.5
## AIC=-311   AICc=-310.84   BIC=-265.54
```

```r
(arima_passengers %>%
  accuracy -> arima_passengers_accuracy)
```

```
## # A tibble: 2 x 9
##   .model        .type        ME  RMSE   MAE   MPE  MAPE  MASE    ACF1
##   <chr>         <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
## 1 stochastic    Training 0.0981 0.903 0.713 -2.35  16.4  1.03 -0.0355
## 2 deterministic Training 0.0984 0.903 0.713 -2.34  16.4  1.03 -0.0357
```

```r
arima_passengers %>%
  forecast(new_data = daily_crime_mobility_post_covid) -> arima_passengers_forecast

(arima_passengers_forecast %>%
  accuracy(data = daily_crime_mobility_post_covid) -> arima_passengers_forecast_errors)
```

```
## # A tibble: 2 x 9
##   .model        .type    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
##   <chr>         <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 deterministic Test  -2.72  3.08  2.79 -188.  189.   NaN 0.905
## 2 stochastic    Test  -2.77  3.13  2.83 -191.  192.   NaN 0.906
```

```r
# Stochastic model is better

arima_passengers_forecast %>%
  filter(.model == "stochastic") %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  transmute(date = date,
         value = .mean,
         Variable = "BRT + SCT passengers (in millions)",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast") -> arima_passengers_final_forecast

arima_passengers %>%
  select(stochastic) %>%
  forecast(new_data = daily_crime_mobility_pre_covid) %>%
  hilo(level = 95) %>%
  unpack_hilo(`95%`) %>%
  filter(date >= "2020-01-01") %>%
  transmute(date = date,
         value = .mean,
         Variable = "BRT + SCT passengers (in millions)",
         ci_low = `95%_lower`,
         ci_high = `95%_upper`,
         type = "Forecast",
         type2 = "Training") -> arima_passengers_training_forecast
```

Now we combine ARIMA forecasting results into tables and plots.

Observed vs forecast values.


```r
## Plot first

arima_all_crimes_final_forecast %>%
  as_tibble %>%
  bind_rows(arima_violent_robbery_final_forecast,
            arima_non_violent_robbery_final_forecast,
            arima_robbery_against_residence_final_forecast,
            arima_serious_violent_crime_final_forecast,
            arima_sexual_violence_final_forecast,
            arima_domestic_violence_final_forecast,
            arima_vaw_calls_final_forecast,
            arima_passengers_final_forecast) -> arima_all_final_forecasts

covid_annotations %>%
  mutate(date = c(covid_date, covid_lockdown)) -> covid_annotations_date

daily_crime_mobility_2020 %>%
  as_tibble %>%
  select(-(`BRT + SCT passengers`)) %>%
  pivot_longer(-date,
               names_to = "Variable") %>%
  mutate(type = "Observed") %>%
  bind_rows(arima_all_final_forecasts) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names),
         type = fct_inorder(type)) %>%
  ggplot(aes(date, value,
             colour = type,
             fill = type)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high,
                  colour = NULL),
              alpha = 0.25) +
  facet_wrap(~ Variable,
             ncol = 1,
             scale = "free_y") +
  geom_vline(xintercept = covid_date, linetype = 2, size = 0.3) +
  geom_vline(xintercept = covid_lockdown, linetype = 2, size = 0.3) +
  geom_text(data = covid_annotations_date,
             aes(x = date, y = value,
                 label = labels,
                 fontface = 3),
             size = 2.5,
             nudge_y = -20,
             nudge_x = -1,
             hjust = "right",
            inherit.aes = FALSE) +
  theme_clean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  scale_color_colorblind(NULL) +
  scale_fill_colorblind(NULL) +
  labs(caption = "Shaded area represents 95% confidence interval.") +
  ylab(NULL) +
  xlab(NULL) -> arima_forecasts_plot

ggsave(filename = "plots/arima_forecasts_plot.pdf",
       plot = arima_forecasts_plot,
       device = cairo_pdf,
       width = 8,
       height = 11)
```

```
## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf

## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
## -Inf
```

![](plots/arima_forecasts_plot.pdf)

Forecast errors plots.


```r
arima_all_crimes_training_forecast %>%
  as_tibble %>%
  bind_rows(arima_violent_robbery_training_forecast,
            arima_non_violent_robbery_training_forecast,
            arima_robbery_against_residence_training_forecast,
            arima_serious_violent_crime_training_forecast,
            arima_sexual_violence_training_forecast,
            arima_domestic_violence_training_forecast,
            arima_vaw_calls_training_forecast,
            arima_passengers_training_forecast) -> arima_all_training_forecasts

arima_all_training_forecasts %>%
  bind_rows(mutate(arima_all_final_forecasts,
                   type2 = "Testing")) -> arima_all_train_test_forecasts

daily_crime_mobility_2020 %>%
  as_tibble %>%
  select(-(`BRT + SCT passengers`)) %>%
  pivot_longer(-date,
               names_to = "Variable",
               values_to = "Observed") %>%
  left_join(arima_all_train_test_forecasts) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names),
         type = fct_inorder(type),
         forecast_error = Observed - value,
         forecast_error_high = Observed - ci_low,
         forecast_error_low = Observed - ci_high) %>%
  # filter(date >= covid_date) %>%
  ggplot(aes(date, forecast_error)) +
  geom_area() +
  facet_wrap(~ Variable,
             ncol = 1,
             scale = "free_y") +
  geom_vline(xintercept = covid_date, linetype = 2, size = 0.3) +
  geom_vline(xintercept = covid_lockdown, linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_text(data = mutate(covid_annotations_date,
                          value = c(300, 300)),
             aes(x = date, y = value,
                 label = labels,
                 fontface = 3),
             size = 2.5,
             nudge_y = -20,
             nudge_x = -1,
             hjust = "right",
            inherit.aes = FALSE) +
  theme_clean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  scale_color_colorblind(NULL) +
  scale_fill_colorblind(NULL) +
  # labs(caption = "Shaded area represents 95% confidence interval.") +
  ylab("Forecast errors") +
  xlab(NULL) -> arima_forecasts_errors_plot
```

```
## Joining, by = c("date", "Variable")
```

```r
ggsave(filename = "plots/arima_forecasts_errors_plot.pdf",
       plot = arima_forecasts_errors_plot,
       device = cairo_pdf,
       width = 8,
       height = 11)
```

![](plots/arima_forecasts_errors_plot.pdf)

Lastly we generate tables summarising the ARIMA models.


```r
list(arima_all_crimes,
          arima_violent_robbery,
          arima_non_violent_robbery,
          arima_robbery_against_residence,
          arima_serious_violent_crime,
          arima_sexual_violence,
          arima_domestic_violence,
          arima_vaw_calls,
          arima_passengers) %>%
  map_df(~ .x %>% pivot_longer(c(stochastic, deterministic),
                               names_to = "trend"),
         .id = "Variable") %>%
  mutate(Variable = factor(Variable,
                           labels = var_names[-9]),
         final_model = case_when(Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")  &
           trend == "deterministic" ~ 1,
           !Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")  &
           trend == "stochastic" ~ 1,
           TRUE ~ 0)) %>%
  filter(final_model > 0) %>%
  select(-final_model) -> arima_final_models

arima_final_models %>%
  group_by(Variable) %>%
  group_map(~ .x %>%
              select(value) %>%
              mable(model = value) %$%
              value[[1]] %>%
              model_sum() %>%
              as_tibble() %>%
              mutate(Variable = .y$Variable)) %>%
  bind_rows() %>%
  select(Variable,
         Specification = value) %>%
  mutate(Specification = str_remove(Specification, " errors"),
         Specification = str_remove(Specification, "LM w/ "),
         Specification = ifelse(Variable %in% c("Sexual violence",
                                                "Domestic violence",
                                                "VAW helpline calls"),
                                paste0(Specification, " w/ linear trend"),
                                Specification)) %>%
  select(Variable,
         Specification) -> arima_specs



list(arima_all_crimes_accuracy,
          arima_violent_robbery_accuracy,
          arima_non_violent_robbery_accuracy,
          arima_robbery_against_residence_accuracy,
          arima_serious_violent_crime_accuracy,
          arima_sexual_violence_accuracy,
          arima_domestic_violence_accuracy,
          arima_vaw_calls_accuracy,
          arima_passengers_accuracy) %>%
  bind_rows(.id = "Variable") %>%
  mutate(Variable = factor(Variable, labels = var_names[-9])) -> arima_all_train_accuracy


list(arima_all_crimes_forecast_errors,
          arima_violent_robbery_forecast_errors,
          arima_non_violent_robbery_forecast_errors,
          arima_robbery_against_residence_forecast_errors,
          arima_serious_violent_crime_forecast_errors,
          arima_sexual_violence_forecast_errors,
          arima_domestic_violence_forecast_errors,
          arima_vaw_calls_forecast_errors,
          arima_passengers_forecast_errors) %>%
  bind_rows(.id = "Variable") %>%
  mutate(Variable = factor(Variable, labels = var_names[-9])) -> arima_all_forecast_errors


arima_all_train_accuracy %>%
  bind_rows(arima_all_forecast_errors) %>%
    mutate(final_model = case_when(Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")  &
           .model == "deterministic" ~ 1,
           !Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")  &
           .model == "stochastic" ~ 1,
           TRUE ~ 0)) %>%
  filter(final_model > 0) %>%
  select(Variable, ME, MAPE, type = .type) %>%
  mutate(type = factor(type,
                       levels = c("Training",
                                  "Test"),
                       labels = c("Pre-COVID-19",
                                  "Post-COVID-19"))) %>%
  pivot_wider(names_from = type,
              values_from = c(ME, MAPE),
              names_glue = "{type} {.value}") %>%
  select(1,2,4,3,5) -> arima_all_accuracy

arima_specs %>%
  left_join(arima_all_accuracy) %>%
  kable(digits = 2,
        align = c("l", "c", "r", "r", "r", "r", "c"),
        caption = "Arima models specifications and accuracy measures.")
```

```
## Joining, by = "Variable"
```



Table: Arima models specifications and accuracy measures.

|Variable                           |             Specification              | Pre-COVID-19 ME| Pre-COVID-19 MAPE| Post-COVID-19 ME| Post-COVID-19 MAPE|
|:----------------------------------|:--------------------------------------:|---------------:|-----------------:|----------------:|------------------:|
|All crimes                         |         ARIMA(3,0,0)(0,1,1)[7]         |            3.75|              6.30|          -191.27|              59.08|
|Violent robbery                    |         ARIMA(1,0,1)(2,1,1)[7]         |            1.42|              9.11|           -51.25|              60.35|
|Non-violent robbery                |         ARIMA(1,1,1)(2,0,0)[7]         |            1.18|             12.19|           -26.96|              62.04|
|Robbery against residence          |         ARIMA(0,1,1)(2,0,0)[7]         |            0.56|             22.33|            -7.56|             163.38|
|Serious violent crime (non-sexual) |         ARIMA(2,1,1)(2,0,0)[7]         |            0.85|             18.30|            -8.44|              60.15|
|Sexual violence                    |      ARIMA(1,0,1) w/ linear trend      |            0.82|             35.83|           -15.29|             196.12|
|Domestic violence                  | ARIMA(2,0,1)(2,0,0)[7] w/ linear trend |            1.01|             14.77|           -21.90|              51.03|
|VAW helpline calls                 | ARIMA(0,0,2)(1,0,1)[7] w/ linear trend |            1.12|             18.08|            17.60|              24.36|
|BRT + SCT passengers (in millions) |     ARIMA(4,0,0)(2,0,0)[7] w/ mean     |            0.10|             16.39|            -2.77|             191.91|

```r
# Calculate total forecast error
daily_crime_mobility_2020 %>%
  as_tibble %>%
  select(-(`BRT + SCT passengers`)) %>%
  pivot_longer(-date,
               names_to = "Variable",
               values_to = "Observed") %>%
  left_join(arima_all_train_test_forecasts) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names),
         type = fct_inorder(type),
         forecast_error = Observed - value,
         forecast_error_high = Observed - ci_low,
         forecast_error_low = Observed - ci_high) %>%
  filter(date >= covid_date) %>%
  group_by(Variable) %>%
  summarise(Observed = sum(Observed),
            Expected = sum(value),
            Expected_low = sum(ci_low),
            Expected_high = sum(ci_high)) %>%
  transmute(Variable = Variable,
            total_error = (Observed/Expected - 1),
            total_error_low = (Observed/Expected_low - 1),
            total_error_high = (Observed/Expected_high - 1),
            Period = "Post COVID-19") -> arima_total_prediction_error
```

```
## Joining, by = c("date", "Variable")
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Calculate total post-lockdown forecast error
daily_crime_mobility_2020 %>%
  as_tibble %>%
  select(-(`BRT + SCT passengers`)) %>%
  pivot_longer(-date,
               names_to = "Variable",
               values_to = "Observed") %>%
  left_join(arima_all_train_test_forecasts) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names),
         type = fct_inorder(type),
         forecast_error = Observed - value,
         forecast_error_high = Observed - ci_low,
         forecast_error_low = Observed - ci_high) %>%
  filter(date >= covid_lockdown) %>%
  group_by(Variable) %>%
  summarise(Observed = sum(Observed),
            Expected = sum(value),
            Expected_low = sum(ci_low),
            Expected_high = sum(ci_high)) %>%
  transmute(Variable = Variable,
            total_error = (Observed/Expected - 1),
            total_error_low = (Observed/Expected_low - 1),
            total_error_high = (Observed/Expected_high - 1),
            Period = "Post lockdown") -> arima_total_prediction_error_post_lockdown
```

```
## Joining, by = c("date", "Variable")
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
arima_total_prediction_error %>%
  bind_rows(arima_total_prediction_error_post_lockdown) %>%
  transmute(Variable = Variable,
            "% change" = paste0(label_percent(accuracy = 0.1)(total_error),
                                " [",
                                label_percent(accuracy = 0.1)(total_error_high),
                                ", ",
                                label_percent(accuracy = 0.1)(total_error_low),
                                "]"),
            Period = Period) %>%
  pivot_wider(names_from = Period,
              values_from = `% change`,
              names_glue = "{Period} [95% CI]") %>%
  kable(align = c("l", "r", "r"),
        caption = "Total percentage change vs ARIMA forecast.")
```



Table: Total percentage change vs ARIMA forecast.

|Variable                           |  Post COVID-19 [95% CI]|  Post lockdown [95% CI]|
|:----------------------------------|-----------------------:|-----------------------:|
|All crimes                         | -30.6% [-43.4%, -14.1%]| -42.7% [-53.3%, -28.8%]|
|Violent robbery                    |  -31.0% [-47.6%, -7.4%]| -42.7% [-56.6%, -22.7%]|
|Non-violent robbery                |   -30.9% [-51.2%, 1.4%]| -42.2% [-59.6%, -14.4%]|
|Robbery against residence          |  -45.6% [-67.1%, -3.4%]| -58.6% [-75.0%, -26.2%]|
|Serious violent crime (non-sexual) |  -27.1% [-53.2%, 20.3%]|   -39.0% [-61.0%, 1.1%]|
|Sexual violence                    |  -53.3% [-78.4%, 21.5%]| -66.5% [-84.6%, -11.9%]|
|Domestic violence                  |  -26.0% [-49.6%, 13.4%]|  -36.5% [-57.1%, -1.9%]|
|VAW helpline calls                 |  27.6% [-19.5%, 115.1%]|  18.1% [-26.0%, 101.0%]|
|BRT + SCT passengers (in millions) | -53.3% [-70.1%, -22.7%]| -68.1% [-79.7%, -46.7%]|



Forest plot with post COVID-19 effect


```r
arima_total_prediction_error %>%
  bind_rows(arima_total_prediction_error_post_lockdown) %>%
  mutate(Variable = fct_rev(Variable)) %>%
  arrange(Variable) %>%
  ggplot(aes(total_error, Variable, colour = Period)) +
  geom_point(position = position_dodge(width = -0.9))  +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_errorbarh(aes(xmin = total_error_low,
                     xmax = total_error_high), height = .3,
                 position = position_dodge(width = -0.9)) +
  geom_text(aes(label = label_percent(accuracy = 0.1)(total_error),
                colour = NULL),
            # position = position_dodge(width = 1),
            nudge_y = c(0.33, -0.1),
            size = 2.5,
            vjust = "bottom",
            show.legend = FALSE) +
  theme_clean() +
  # scale_color_colorblind() +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_x_continuous("Percentage change vs. ARIMA forecast",
                     labels = scales::percent) +
  ylab(NULL) +
  theme(legend.position = "bottom") +
  labs(caption = "Error bars represent 95% confidence interval") -> arima_change_plot1

ggsave(filename = "plots/arima_change_plot1",
       plot = arima_change_plot1,
       device = cairo_pdf,
       width = 7,
       height = 6)
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

```r
arima_total_prediction_error %>%
  bind_rows(arima_total_prediction_error_post_lockdown) %>%
  mutate(Variable = fct_rev(Variable)) %>%
  arrange(Variable) %>%
  ggplot(aes(total_error, Variable, colour = Period)) +
  geom_point(position = position_dodge(width = -0.9))  +
  geom_vline(xintercept = 0, colour = "grey75") +
  geom_errorbarh(aes(xmin = total_error_low,
                     xmax = total_error_high), height = .3,
                 position = position_dodge(width = -0.9)) +
  geom_text(aes(label = paste0(label_percent(accuracy = 0.1)(total_error),
                               " [",
                               label_percent(accuracy = 0.1)(total_error_high),
                               ", ",
                               label_percent(accuracy = 0.1)(total_error_low),
                               "]"),
                colour = NULL),
            nudge_y = c(0.33, -0.1),
            nudge_x = -.05,
            size = 2.5,
            vjust = "bottom",
            hjust = "left",
            show.legend = FALSE) +
  theme_clean() +
  # scale_color_colorblind() +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_x_continuous("Percentage change vs. ARIMA forecast",
                     labels = scales::percent) +
  ylab(NULL) +
  theme(legend.position = "bottom") +
  labs(caption = "Error bars represent 95% confidence interval") -> arima_change_plot2

ggsave(filename = "plots/arima_change_plot2",
       plot = arima_change_plot2,
       device = cairo_pdf,
       width = 7,
       height = 6)
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

```r
arima_total_prediction_error %>%
  bind_rows(arima_total_prediction_error_post_lockdown) %>%
  mutate(Variable = fct_rev(Variable)) %>%
  arrange(Variable) %>%
  ggplot(aes(total_error, Variable, colour = Period)) +
  geom_point(position = position_dodge(width = -0.9))  +
  geom_vline(xintercept = 0, colour = "grey75") +
  geom_errorbarh(aes(xmin = total_error_low,
                     xmax = total_error_high), height = .3,
                 position = position_dodge(width = -0.9)) +
  theme_clean() +
  # scale_color_colorblind() +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_x_continuous("Percentage change vs. ARIMA forecast",
                     labels = scales::percent) +
  ylab(NULL) +
  theme(legend.position = "bottom") +
  labs(caption = "Error bars represent 95% confidence interval") -> arima_change_plot3

ggsave(filename = "plots/arima_change_plot3",
       plot = arima_change_plot3,
       device = cairo_pdf,
       width = 7,
       height = 6)
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

# Crime and mobility error correction models

This section contains the error correction models for crime and mobility. The ARIMA errors' specifications were recalculated (as they correspond to the errors conditional on mobility). Deterministic trends were used only for sexual violence and domestic violence, and for the VAW helpline calls. The main models were estimated using the entire observation period, though additional models using the pre- and post-COVID-19 periods were estimated as robustness checks.

The results are summarised using tables with estimated parameters and model specifications, and with a forest plot of the observed effect conditional on the observed decline in mobility.

Steps followed:

- Fit LM with ARIMA errors
- Plot residuals vs fitted, density residuals and residuals over time (add white noise test)
- Fit robustness checks models
- Summarise results in table
- Summarise results in plot


```r
daily_crime_mobility %>%
  select(-c(`BRT + SCT passengers`)) %>%
  pivot_longer(-c(date, `BRT + SCT passengers (in millions)`),
               names_to = "Variable") %>%
  filter(!Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  as_tsibble(key = Variable) %>%
  model(lm = ARIMA(log(value) ~ log(`BRT + SCT passengers (in millions)`),
                   stepwise = FALSE,
                   approximation = FALSE)) -> lm_arima_stochastic

daily_crime_mobility %>%
  select(-c(`BRT + SCT passengers`)) %>%
  pivot_longer(-c(date, `BRT + SCT passengers (in millions)`),
               names_to = "Variable") %>%
  filter(Variable %in% c("Sexual violence",
                           "Domestic violence",
                           "VAW helpline calls")) %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  as_tsibble(key = Variable) %>%
  model(lm = ARIMA(log(value) ~ log(`BRT + SCT passengers (in millions)`) + trend(),
                   stepwise = FALSE,
                   approximation = FALSE)) -> lm_arima_deterministic


lapply(lm_arima_stochastic$Variable, function(x){
  print(paste0("Report for ", x))
  lm_arima_stochastic %>%
    filter(Variable == x) %>%
    select(lm) %>%
    report
})
```

```
## [1] "Report for All crimes"
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.0986  -0.1188  -0.8811  0.2876  0.2680
## s.e.  0.0476   0.0396   0.0377  0.0314  0.0331
##       log(`BRT + SCT passengers (in millions)`)  intercept
##                                          0.2130     6.0987
## s.e.                                     0.0172     0.0442
##
## sigma^2 estimated as 0.008612:  log likelihood=1177.55
## AIC=-2339.09   AICc=-2338.98   BIC=-2298.11
## [1] "Report for Violent robbery"
## Series: value
## Model: LM w/ ARIMA(1,1,2)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##           ar1      ma1      ma2    sar1    sar2
##       -0.3445  -0.4952  -0.4722  0.3336  0.3127
## s.e.   0.1422   0.1311   0.1275  0.0298  0.0287
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.3382
## s.e.                                     0.0210
##
## sigma^2 estimated as 0.01727:  log likelihood=747.15
## AIC=-1480.29   AICc=-1480.2   BIC=-1444.44
## [1] "Report for Non-violent robbery"
## Series: value
## Model: LM w/ ARIMA(0,1,2)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##           ma1      ma2    sar1    sar2
##       -0.8956  -0.0621  0.1419  0.0675
## s.e.   0.0302   0.0299  0.0316  0.0309
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.3732
## s.e.                                     0.0201
##
## sigma^2 estimated as 0.02079:  log likelihood=634.45
## AIC=-1256.9   AICc=-1256.83   BIC=-1226.17
## [1] "Report for Robbery against residence"
## Series: value
## Model: LM w/ ARIMA(0,1,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##           ma1    sar1    sar2  log(`BRT + SCT passengers (in millions)`)
##       -0.9120  0.0887  0.0703                                    -0.0404
## s.e.   0.0136  0.0301  0.0297                                     0.0368
##
## sigma^2 estimated as 0.08285:  log likelihood=-216.79
## AIC=443.57   AICc=443.62   BIC=469.18
## [1] "Report for Serious violent crime (non-sexual)"
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1066  -0.1161  -0.9023  0.0788  0.1164
## s.e.  0.0359   0.0333   0.0214  0.0310  0.0306
##       log(`BRT + SCT passengers (in millions)`)  intercept
##                                         -0.0736     3.4783
## s.e.                                     0.0321     0.0942
##
## sigma^2 estimated as 0.05242:  log likelihood=66.57
## AIC=-117.14   AICc=-117.03   BIC=-76.16
```

```
## [[1]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                      lm
##   <fct>                                    <model>
## 1 All crimes <LM w/ ARIMA(2,0,1)(2,0,0)[7] errors>
##
## [[2]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                           lm
##   <fct>                                         <model>
## 1 Violent robbery <LM w/ ARIMA(1,1,2)(2,0,0)[7] errors>
##
## [[3]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                               lm
##   <fct>                                             <model>
## 1 Non-violent robbery <LM w/ ARIMA(0,1,2)(2,0,0)[7] errors>
##
## [[4]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                                     lm
##   <fct>                                                   <model>
## 1 Robbery against residence <LM w/ ARIMA(0,1,1)(2,0,0)[7] errors>
##
## [[5]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                                              lm
##   <fct>                                                            <model>
## 1 Serious violent crime (non-sexual) <LM w/ ARIMA(2,0,1)(2,0,0)[7] errors>
```

```r
lapply(lm_arima_deterministic$Variable, function(x){
  print(paste0("Report for ", x))
  lm_arima_deterministic %>%
    filter(Variable == x) %>%
    select(lm) %>%
    report
})
```

```
## [1] "Report for Sexual violence"
## Series: value
## Model: LM w/ ARIMA(1,0,1) errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1  log(`BRT + SCT passengers (in millions)`)  trend()
##       0.9842  -0.8884                                     0.2220    1e-03
## s.e.  0.0073   0.0178                                     0.0471    2e-04
##       intercept
##          1.3723
## s.e.     0.1728
##
## sigma^2 estimated as 0.1718:  log likelihood=-665.18
## AIC=1342.36   AICc=1342.43   BIC=1373.1
## [1] "Report for Domestic violence"
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1802  -0.1933  -0.9007  0.1342  0.1602
## s.e.  0.0355   0.0329   0.0201  0.0321  0.0314
##       log(`BRT + SCT passengers (in millions)`)  trend()  intercept
##                                         -0.1345    2e-04     4.1090
## s.e.                                     0.0283    1e-04     0.1122
##
## sigma^2 estimated as 0.03297:  log likelihood=352.48
## AIC=-686.97   AICc=-686.82   BIC=-640.86
## [1] "Report for VAW helpline calls"
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1349  -0.1483  -0.9105  0.1711  0.1319
## s.e.  0.0378   0.0343   0.0230  0.0301  0.0295
##       log(`BRT + SCT passengers (in millions)`)  trend()  intercept
##                                          0.1881    7e-04     3.0942
## s.e.                                     0.0297    2e-04     0.1252
##
## sigma^2 estimated as 0.04926:  log likelihood=101.55
## AIC=-185.1   AICc=-184.95   BIC=-138.99
```

```
## [[1]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                 lm
##   <fct>                               <model>
## 1 Sexual violence <LM w/ ARIMA(1,0,1) errors>
##
## [[2]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                             lm
##   <fct>                                           <model>
## 1 Domestic violence <LM w/ ARIMA(2,0,1)(2,0,0)[7] errors>
##
## [[3]]
## # A mable: 1 x 2
## # Key:     Variable [1]
##   Variable                                              lm
##   <fct>                                            <model>
## 1 VAW helpline calls <LM w/ ARIMA(2,0,1)(2,0,0)[7] errors>
```

```r
## Accuracy

(lm_arima_stochastic %>%
  accuracy() %>%
    bind_rows(accuracy(lm_arima_deterministic)) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names[-c(9,10)])) -> lm_arima_accuracy)
```

```
## # A tibble: 8 x 10
##   Variable          .model .type     ME  RMSE   MAE     MPE  MAPE  MASE     ACF1
##   <fct>             <chr>  <chr>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>
## 1 All crimes        lm     Train… 3.21  57.4  43.6   -0.422  7.04 0.794 -0.0264
## 2 Violent robbery   lm     Train… 1.60  23.0  18.0   -0.913 10.1  0.837 -0.0262
## 3 Non-violent robb… lm     Train… 0.832 12.6   9.76  -1.17  11.3  0.760 -0.0139
## 4 Robbery against … lm     Train… 0.484  4.77  3.78  -5.51  23.9  0.757 -0.00926
## 5 Serious violent … lm     Train… 0.769  7.00  5.45  -2.74  18.5  0.761 -0.0187
## 6 Sexual violence   lm     Train… 0.831  4.48  3.36 -10.6   35.6  0.752  0.0228
## 7 Domestic violence lm     Train… 0.997 10.9   8.46  -1.64  14.5  0.767 -0.00182
## 8 VAW helpline cal… lm     Train… 1.13  10.3   7.71  -2.49  17.8  0.772  0.0453
```


Now we check the residuals of the LM w/ ARIMA errors models.



```r
## Residual checks

lm_arima_stochastic %>%
  augment() %>%
  bind_rows(augment(lm_arima_deterministic)) %>%
  mutate(Variable = factor(Variable,
                           levels = var_names[-c(9,10)])) -> lm_arima_augment

lm_arima_augment %>%
  ggplot(aes(.resid)) +
  geom_density() +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  xlab("Residuals") +
  ylab("Density") +
  theme_clean()  -> lm_arima_residual_plot

## Residual vs fitted

lm_arima_augment %>%
  ggplot(aes(.fitted, .resid)) +
  geom_hex() +
  scale_fill_continuous_tableau(palette = "Orange") +
  geom_smooth(se = TRUE,
              method = "lm") +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  ylab("Residuals") +
  xlab("Fitted") +
  theme_clean() +
  theme(legend.position = "none") -> lm_arima_residual_plot2


lm_arima_augment %>%
  ggplot(aes(date, .resid)) +
  geom_line(size = 0.01, alpha = 0.5) +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  ylab("Residuals") +
  xlab("Date") +
  theme_clean() +
  theme(legend.position = "none") -> lm_arima_residual_plot3

lm_arima_augment %>%
  ACF(.resid, lag_max = 14) %>%
  autoplot() +
  # ggplot(aes(date, .resid)) +
  # geom_line(size = 0.05) +
  facet_wrap(~ Variable,
             # scales = "free",
             ncol = 4) +
  ylab("ACF") +
  xlab("Lags") +
  theme_clean() -> lm_arima_residual_plot4


plot_grid(lm_arima_residual_plot3,
          lm_arima_residual_plot4,
          lm_arima_residual_plot,
          lm_arima_residual_plot2,
          labels = "AUTO",
          scale = 0.98,
          hjust = -0.9,
          vjust = 1.7) -> arima_all_residual_plots
```

```
## Warning: Removed 70 rows containing non-finite values (stat_density).
```

```
## Warning: Removed 70 rows containing non-finite values (stat_binhex).
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 70 rows containing non-finite values (stat_smooth).
```

```r
ggsave(filename = "plots/arima_all_residual_plots.pdf",
       plot = arima_all_residual_plots,
       device = cairo_pdf,
       width = 20,
       height = 12)
```

![](plots/arima_all_residual_plots.pdf)

Some residual ACF plots suggest the presence of serial correlation. We test whether the residuals are white noise using the Ljung-Box test.


```r
lb.test <- function(mdl, lags = 14){

  get_df <- function(mdl){
  mdl[[1]]$fit$spec[1:7] %>% sum + 1
}

  mdls <- names(mdl)
  lapply(mdls, function(x){
    dfs <- get_df(mdl[[x]])
    mdl %>%
      select(x) %>%
      augment %$%
      Box.test(.resid,
               lag = lags,
               type = "Ljung-Box",
               fitdf = dfs) %$%
      tibble(.model = x,
             LB = statistic,
             df = parameter,
             pvalue = p.value,
             sig = victim::add_stars(pvalue))
  }) %>%
    bind_rows
  }

## Stochastic models

lm_arima_stochastic %>%
  group_by(Variable) %>%
  group_map(~ .x %>%
              as_mable %>%
              lb.test(lags = 21) %>%
              mutate(Variable = .y$Variable) %>%
              select(last_col(), everything())) %>%
  bind_rows() -> lm_arima_stochastic_lb
```

```
## Note: Using an external vector in selections is ambiguous.
## ℹ Use `all_of(x)` instead of `x` to silence this message.
## ℹ See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Deterministic models

lm_arima_deterministic %>%
  group_by(Variable) %>%
  group_map(~ .x %>%
              as_mable %>%
              lb.test(lags = 21) %>%
              mutate(Variable = .y$Variable) %>%
              select(last_col(), everything())) %>%
  bind_rows() -> lm_arima_deterministic_lb


(lm_arima_stochastic_lb %>%
  bind_rows(lm_arima_deterministic_lb) -> lm_arima_lb)
```

```
## # A tibble: 8 x 6
##   Variable                           .model    LB    df   pvalue sig
##   <fct>                              <chr>  <dbl> <dbl>    <dbl> <chr>
## 1 All crimes                         lm      67.9    14 4.58e- 9 "***"
## 2 Violent robbery                    lm      78.0    14 6.58e-11 "***"
## 3 Non-violent robbery                lm      18.3    15 2.47e- 1 ""
## 4 Robbery against residence          lm      15.0    16 5.22e- 1 ""
## 5 Serious violent crime (non-sexual) lm      43.3    14 7.76e- 5 "***"
## 6 Sexual violence                    lm      21.8    17 1.93e- 1 ""
## 7 Domestic violence                  lm      68.8    14 3.17e- 9 "***"
## 8 VAW helpline calls                 lm      36.5    14 8.91e- 4 "***"
```

```r
# Also test for stationarity

(list(lm_arima_stochastic,
     lm_arima_deterministic) %>%
  map_df(~ .x %>%
        augment() %>%
  features(.resid, unitroot_kpss)) %>%
    mutate(sig = victim::add_stars(kpss_pvalue)) -> lm_arima_kpss)
```

```
## # A tibble: 8 x 5
##   Variable                           .model kpss_stat kpss_pvalue sig
##   <fct>                              <chr>      <dbl>       <dbl> <chr>
## 1 All crimes                         lm        0.313       0.1    ""
## 2 Violent robbery                    lm        1.01        0.01   "*"
## 3 Non-violent robbery                lm        0.421       0.0680 ""
## 4 Robbery against residence          lm        0.287       0.1    ""
## 5 Serious violent crime (non-sexual) lm        0.294       0.1    ""
## 6 Sexual violence                    lm        0.173       0.1    ""
## 7 Domestic violence                  lm        0.0944      0.1    ""
## 8 VAW helpline calls                 lm        0.112       0.1    ""
```

```r
# check for invertibility (Box-Steffensmeier, et al., 2014)

lm_arima_stochastic %>%
  tidy %>%
  bind_rows(tidy(lm_arima_deterministic)) %>%
  filter(abs(estimate) > 1 &
           term != "intercept")
```

```
## # A tibble: 4 x 7
##   Variable                   .model term  estimate std.error statistic   p.value
##   <fct>                      <chr>  <chr>    <dbl>     <dbl>     <dbl>     <dbl>
## 1 All crimes                 lm     ar1       1.10    0.0476      23.1 3.30e- 98
## 2 Serious violent crime (no… lm     ar1       1.11    0.0359      30.8 7.29e-155
## 3 Domestic violence          lm     ar1       1.18    0.0355      33.2 2.60e-173
## 4 VAW helpline calls         lm     ar1       1.13    0.0378      30.0 1.07e-148
```


The Ljung-Box test suggest the the residuals of the following crimes are autocorrelated:

- All crimes
- Violent robbery
- Serious violent crime
- Domestic violence
- VAW calls

It is necessary to fit these models by hand to find a more suitable fit.

There was also an indication of non-stationarity for Violent Robbery. All other residuals were stationary. The fact that Ar terms for all crimes, serious violent crime, domestic violence and VAW calls  were not invertible (the were greater than 1), suggests that the residuals are non-stationary.

- **All crimes**


```r
daily_crime_mobility %>%
  model(auto = ARIMA(log(`All crimes`) ~ 1 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(2,0,0,
                            period = 7)),
        manual1 = ARIMA(log(`All crimes`) ~ 1 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,0,1) +
                        PDQ(1,0,1,
                            period = 7)),
        manual2 = ARIMA(log(`All crimes`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,0,1,
                            period = 7)),
        manual3 = ARIMA(log(`All crimes`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,1,1,
                            period = 7))) -> lm_arima_all_crimes_manual

# lm_arima_all_crimes_manual %>%
#   augment() %>%
#   ACF(.resid) %>%
#   autoplot()
#
# lm_arima_all_crimes_manual %>%
#   augment() %>%
#   ggplot(aes(date, .resid)) +
#   geom_line() +
#   facet_wrap(~ .model)
#
# lm_arima_all_crimes_manual %>%
#   augment() %>%
#   ggplot(aes(.resid)) +
#   geom_density() +
#   facet_wrap(~ .model)
#
# lm_arima_all_crimes_manual %>%
#   augment() %>%
#   ggplot(aes(.fitted, .resid)) +
#   geom_hex() +
#   geom_smooth(method = "lm") +
#   scale_fill_continuous_tableau(palette = "Orange") +
#   facet_wrap(~ .model) +
#   theme(legend.position = "none")

lm_arima_all_crimes_manual %>%
  glance
```

```
## # A tibble: 4 x 8
##   .model   sigma2 log_lik    AIC   AICc    BIC ar_roots   ma_roots
##   <chr>     <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <list>     <list>
## 1 auto    0.00861   1178. -2339. -2339. -2298. <cpl [16]> <cpl [1]>
## 2 manual1 0.00697   1302. -2589. -2589. -2554. <cpl [8]>  <cpl [8]>
## 3 manual2 0.00697   1300. -2588. -2588. -2558. <cpl [8]>  <cpl [8]>
## 4 manual3 0.00697   1291. -2570. -2570. -2539. <cpl [8]>  <cpl [8]>
```

```r
lb.test(lm_arima_all_crimes_manual, lags = 21)
```

```
## # A tibble: 4 x 5
##   .model     LB    df        pvalue sig
##   <chr>   <dbl> <dbl>         <dbl> <chr>
## 1 auto     67.9    14 0.00000000458 "***"
## 2 manual1  26.1    15 0.0375        "*"
## 3 manual2  20.3    15 0.160         ""
## 4 manual3  20.7    14 0.111         ""
```

```r
lm_arima_all_crimes_manual %>%
  augment() %>%
  features(.resid, unitroot_kpss) %>%
    mutate(sig = victim::add_stars(kpss_pvalue))
```

```
## # A tibble: 4 x 4
##   .model  kpss_stat kpss_pvalue sig
##   <chr>       <dbl>       <dbl> <chr>
## 1 auto       0.313       0.1    ""
## 2 manual1    0.597       0.0229 "*"
## 3 manual2    0.0861      0.1    ""
## 4 manual3    0.160       0.1    ""
```

```r
# Best model is manual 2

lm_arima_all_crimes_manual %>%
  select(manual2) %>%
  report
```

```
## Series: All crimes
## Model: LM w/ ARIMA(1,1,1)(1,0,1)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1     sma1
##       0.0980  -0.8258  0.9986  -0.9568
## s.e.  0.0375   0.0220  0.0010   0.0111
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.1666
## s.e.                                     0.0136
##
## sigma^2 estimated as 0.00697:  log likelihood=1300.17
## AIC=-2588.34   AICc=-2588.28   BIC=-2557.61
```

```r
lm_arima_all_crimes_manual %>%
  select(manual2) -> lm_arima_all_crimes_manual_best
```

- **Violent robbery**


```r
lm_arima_stochastic %>%
  filter(Variable == "Violent robbery") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(1,1,2)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##           ar1      ma1      ma2    sar1    sar2
##       -0.3445  -0.4952  -0.4722  0.3336  0.3127
## s.e.   0.1422   0.1311   0.1275  0.0298  0.0287
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.3382
## s.e.                                     0.0210
##
## sigma^2 estimated as 0.01727:  log likelihood=747.15
## AIC=-1480.29   AICc=-1480.2   BIC=-1444.44
```

```r
daily_crime_mobility %>%
  model(auto = ARIMA(log(`Violent robbery`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,2) +
                        PDQ(2,0,0,
                            period = 7)),
        manual1 = ARIMA(log(`Violent robbery`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,2) +
                        PDQ(1,0,1,
                            period = 7)),
        # manual2 = ARIMA(log(`Violent robbery`) ~ 0 +
        #                 log(`BRT + SCT passengers (in millions)`) +
        #                 pdq(1,1,2) +
        #                 PDQ(2,0,0,
        #                     period = 7)),
        # manual3 = ARIMA(log(`Violent robbery`) ~ 0 +
        #                 log(`BRT + SCT passengers (in millions)`) +
        #                 pdq(1,1,2) +
        #                 PDQ(2,0,0,
        #                     period = 7))
        ) -> lm_arima_violent_robbery_manual

# lm_arima_violent_robbery_manual %>%
#   augment() %>%
#   ACF(.resid) %>%
#   autoplot()
#
# lm_arima_violent_robbery_manual %>%
#   augment() %>%
#   ggplot(aes(date, .resid)) +
#   geom_line() +
#   facet_wrap(~ .model)
#
# lm_arima_violent_robbery_manual %>%
#   augment() %>%
#   ggplot(aes(.resid)) +
#   geom_density() +
#   facet_wrap(~ .model)
#
# lm_arima_violent_robbery_manual %>%
#   augment() %>%
#   ggplot(aes(.fitted, .resid)) +
#   geom_hex() +
#   geom_smooth(method = "lm") +
#   scale_fill_continuous_tableau(palette = "Orange") +
#   facet_wrap(~ .model) +
#   theme(legend.position = "none")

lm_arima_violent_robbery_manual %>%
  glance
```

```
## # A tibble: 2 x 8
##   .model  sigma2 log_lik    AIC   AICc    BIC ar_roots   ma_roots
##   <chr>    <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <list>     <list>
## 1 auto    0.0173    747. -1480. -1480. -1444. <cpl [15]> <cpl [2]>
## 2 manual1 0.0135    895. -1775. -1775. -1739. <cpl [8]>  <cpl [9]>
```

```r
lb.test(lm_arima_violent_robbery_manual, lags = 21)
```

```
## # A tibble: 2 x 5
##   .model     LB    df   pvalue sig
##   <chr>   <dbl> <dbl>    <dbl> <chr>
## 1 auto     78.0    14 6.58e-11 "***"
## 2 manual1  16.8    14 2.66e- 1 ""
```

```r
lm_arima_violent_robbery_manual %>%
  augment() %>%
  features(.resid, unitroot_kpss) %>%
    mutate(sig = victim::add_stars(kpss_pvalue))
```

```
## # A tibble: 2 x 4
##   .model  kpss_stat kpss_pvalue sig
##   <chr>       <dbl>       <dbl> <chr>
## 1 auto        1.01         0.01 "*"
## 2 manual1     0.128        0.1  ""
```

```r
# Best model is manual 1

lm_arima_violent_robbery_manual %>%
  select(manual1) %>%
  report
```

```
## Series: Violent robbery
## Model: LM w/ ARIMA(1,1,2)(1,0,1)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##           ar1      ma1      ma2    sar1     sma1
##       -0.2117  -0.5949  -0.3183  0.9990  -0.9567
## s.e.   0.3547   0.3420   0.3138  0.0008   0.0116
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.3041
## s.e.                                     0.0203
##
## sigma^2 estimated as 0.01345:  log likelihood=894.54
## AIC=-1775.08   AICc=-1774.99   BIC=-1739.23
```

```r
lm_arima_violent_robbery_manual %>%
  select(manual1) -> lm_arima_violent_robbery_manual_best
```

- **Serious violent crime**


```r
lm_arima_stochastic %>%
  filter(Variable == "Serious violent crime (non-sexual)") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1066  -0.1161  -0.9023  0.0788  0.1164
## s.e.  0.0359   0.0333   0.0214  0.0310  0.0306
##       log(`BRT + SCT passengers (in millions)`)  intercept
##                                         -0.0736     3.4783
## s.e.                                     0.0321     0.0942
##
## sigma^2 estimated as 0.05242:  log likelihood=66.57
## AIC=-117.14   AICc=-117.03   BIC=-76.16
```

```r
daily_crime_mobility %>%
  model(auto = ARIMA(log(`Serious violent crime (non-sexual)`) ~ 1 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(2,0,0,
                            period = 7)),
        manual1 = ARIMA(log(`Serious violent crime (non-sexual)`) ~ 1 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(1,0,1,
                            period = 7)),
        manual2 = ARIMA(log(`Serious violent crime (non-sexual)`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                          pdq(1,1,1) +
                          PDQ(1,0,1,
                            period = 7)),
        manual3 = ARIMA(log(`Serious violent crime (non-sexual)`) ~ 0 +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,1,1,
                            period = 7))
        ) -> lm_arima_serious_violent_crime_manual

# lm_arima_serious_violent_crime_manual %>%
#   augment() %>%
#   ACF(.resid) %>%
#   autoplot()
#
# lm_arima_serious_violent_crime_manual %>%
#   augment() %>%
#   ggplot(aes(date, .resid)) +
#   geom_line() +
#   facet_wrap(~ .model)
#
# lm_arima_serious_violent_crime_manual %>%
#   augment() %>%
#   ggplot(aes(.resid)) +
#   geom_density() +
#   facet_wrap(~ .model)
#
# lm_arima_serious_violent_crime_manual %>%
#   augment() %>%
#   ggplot(aes(.fitted, .resid)) +
#   geom_hex() +
#   geom_smooth(method = "lm") +
#   scale_fill_continuous_tableau(palette = "Orange") +
#   facet_wrap(~ .model) +
#   theme(legend.position = "none")

lm_arima_serious_violent_crime_manual %>%
  glance
```

```
## # A tibble: 4 x 8
##   .model  sigma2 log_lik   AIC  AICc    BIC ar_roots   ma_roots
##   <chr>    <dbl>   <dbl> <dbl> <dbl>  <dbl> <list>     <list>
## 1 auto    0.0524    66.6 -117. -117.  -76.2 <cpl [16]> <cpl [1]>
## 2 manual1 0.0482   114.  -212. -212. -171.  <cpl [9]>  <cpl [8]>
## 3 manual2 0.0482   111.  -211. -211. -180.  <cpl [8]>  <cpl [8]>
## 4 manual3 0.0483   104.  -197. -197. -166.  <cpl [8]>  <cpl [8]>
```

```r
lb.test(lm_arima_serious_violent_crime_manual, lags = 21)
```

```
## # A tibble: 4 x 5
##   .model     LB    df    pvalue sig
##   <chr>   <dbl> <dbl>     <dbl> <chr>
## 1 auto     43.3    14 0.0000776 "***"
## 2 manual1  21.4    14 0.0920    ""
## 3 manual2  22.0    15 0.109     ""
## 4 manual3  18.9    14 0.168     ""
```

```r
lm_arima_serious_violent_crime_manual %>%
  augment() %>%
  features(.resid, unitroot_kpss) %>%
    mutate(sig = victim::add_stars(kpss_pvalue))
```

```
## # A tibble: 4 x 4
##   .model  kpss_stat kpss_pvalue sig
##   <chr>       <dbl>       <dbl> <chr>
## 1 auto        0.294      0.1    ""
## 2 manual1     0.417      0.0699 ""
## 3 manual2     0.147      0.1    ""
## 4 manual3     0.141      0.1    ""
```

```r
# Best model is manual 2

lm_arima_stochastic %>%
  filter(Variable == "Serious violent crime (non-sexual)") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1066  -0.1161  -0.9023  0.0788  0.1164
## s.e.  0.0359   0.0333   0.0214  0.0310  0.0306
##       log(`BRT + SCT passengers (in millions)`)  intercept
##                                         -0.0736     3.4783
## s.e.                                     0.0321     0.0942
##
## sigma^2 estimated as 0.05242:  log likelihood=66.57
## AIC=-117.14   AICc=-117.03   BIC=-76.16
```

```r
lm_arima_serious_violent_crime_manual %>%
  select(manual2) %>%
  report
```

```
## Series: Serious violent crime (non-sexual)
## Model: LM w/ ARIMA(1,1,1)(1,0,1)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1     sma1
##       0.0886  -0.9107  0.9985  -0.9762
## s.e.  0.0328   0.0159  0.0012   0.0080
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.0805
## s.e.                                     0.0347
##
## sigma^2 estimated as 0.04825:  log likelihood=111.32
## AIC=-210.64   AICc=-210.57   BIC=-179.91
```

```r
lm_arima_serious_violent_crime_manual %>%
  select(manual2) -> lm_arima_serious_violent_crime_manual_best
```

- **Domestic violence**


```r
lm_arima_deterministic %>%
  filter(Variable == "Domestic violence") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1802  -0.1933  -0.9007  0.1342  0.1602
## s.e.  0.0355   0.0329   0.0201  0.0321  0.0314
##       log(`BRT + SCT passengers (in millions)`)  trend()  intercept
##                                         -0.1345    2e-04     4.1090
## s.e.                                     0.0283    1e-04     0.1122
##
## sigma^2 estimated as 0.03297:  log likelihood=352.48
## AIC=-686.97   AICc=-686.82   BIC=-640.86
```

```r
daily_crime_mobility %>%
  model(auto = ARIMA(log(`Domestic violence`) ~ 1 +
                       trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(2,0,0,
                            period = 7)),
        manual1 = ARIMA(log(`Domestic violence`) ~ 1 +
                       trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(1,0,1,
                            period = 7)),
        manual2 = ARIMA(log(`Domestic violence`) ~ 0 +
                       trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                          pdq(1,1,1) +
                          PDQ(1,0,1,
                            period = 7)),
        manual3 = ARIMA(log(`Domestic violence`) ~ 0 +
                       # trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,0,1,
                            period = 7))
        ) -> lm_arima_domestic_violence_manual
```

```
## Warning in wrap_arima(y, order = c(p, d, q), seasonal = list(order = c(P, :
## possible convergence problem: optim gave code = 1

## Warning in wrap_arima(y, order = c(p, d, q), seasonal = list(order = c(P, :
## possible convergence problem: optim gave code = 1

## Warning in wrap_arima(y, order = c(p, d, q), seasonal = list(order = c(P, :
## possible convergence problem: optim gave code = 1
```

```r
# lm_arima_domestic_violence_manual %>%
#   augment() %>%
#   ACF(.resid) %>%
#   autoplot()
#
# lm_arima_domestic_violence_manual %>%
#   augment() %>%
#   ggplot(aes(date, .resid)) +
#   geom_line() +
#   facet_wrap(~ .model)
#
# lm_arima_domestic_violence_manual %>%
#   augment() %>%
#   ggplot(aes(.resid)) +
#   geom_density() +
#   facet_wrap(~ .model)
#
# lm_arima_domestic_violence_manual %>%
#   augment() %>%
#   ggplot(aes(.fitted, .resid)) +
#   geom_hex() +
#   geom_smooth(method = "lm") +
#   scale_fill_continuous_tableau(palette = "Orange") +
#   facet_wrap(~ .model) +
#   theme(legend.position = "none")

lm_arima_domestic_violence_manual %>%
  glance
```

```
## # A tibble: 4 x 8
##   .model  sigma2 log_lik   AIC  AICc   BIC ar_roots   ma_roots
##   <chr>    <dbl>   <dbl> <dbl> <dbl> <dbl> <list>     <list>
## 1 auto    0.0330    352. -687. -687. -641. <cpl [16]> <cpl [1]>
## 2 manual1 0.0289    423. -829. -828. -782. <cpl [9]>  <cpl [8]>
## 3 manual2 0.0291    419. -825. -825. -789. <cpl [8]>  <cpl [8]>
## 4 manual3 0.0291    419. -826. -826. -795. <cpl [8]>  <cpl [8]>
```

```r
lb.test(lm_arima_domestic_violence_manual, lags = 21)
```

```
## # A tibble: 4 x 5
##   .model     LB    df        pvalue sig
##   <chr>   <dbl> <dbl>         <dbl> <chr>
## 1 auto     68.9    14 0.00000000309 "***"
## 2 manual1  23.4    14 0.0543        ""
## 3 manual2  22.2    15 0.103         ""
## 4 manual3  22.0    15 0.107         ""
```

```r
lm_arima_domestic_violence_manual %>%
  augment() %>%
  features(.resid, unitroot_kpss) %>%
    mutate(sig = victim::add_stars(kpss_pvalue))
```

```
## # A tibble: 4 x 4
##   .model  kpss_stat kpss_pvalue sig
##   <chr>       <dbl>       <dbl> <chr>
## 1 auto       0.0953         0.1 ""
## 2 manual1    0.104          0.1 ""
## 3 manual2    0.100          0.1 ""
## 4 manual3    0.0973         0.1 ""
```

```r
# Best model is manual 3

lm_arima_deterministic %>%
  filter(Variable == "Domestic violence") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1802  -0.1933  -0.9007  0.1342  0.1602
## s.e.  0.0355   0.0329   0.0201  0.0321  0.0314
##       log(`BRT + SCT passengers (in millions)`)  trend()  intercept
##                                         -0.1345    2e-04     4.1090
## s.e.                                     0.0283    1e-04     0.1122
##
## sigma^2 estimated as 0.03297:  log likelihood=352.48
## AIC=-686.97   AICc=-686.82   BIC=-640.86
```

```r
lm_arima_domestic_violence_manual %>%
  select(manual2) %>%
  report
```

```
## Series: Domestic violence
## Model: LM w/ ARIMA(1,1,1)(1,0,1)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1     sma1  trend()
##       0.1274  -0.8889  0.9999  -0.9922  -0.0005
## s.e.  0.0328   0.0152  0.0001   0.0048   0.0044
##       log(`BRT + SCT passengers (in millions)`)
##                                         -0.0004
## s.e.                                     0.0270
##
## sigma^2 estimated as 0.02905:  log likelihood=419.38
## AIC=-824.75   AICc=-824.66   BIC=-788.9
```

```r
lm_arima_domestic_violence_manual %>%
  select(manual2) -> lm_arima_domestic_violence_manual_best
```



- **VAW helpline calls**


```r
# lm_arima_deterministic %>%
#   filter(Variable == "VAW helpline calls") %>%
#   select(lm) %>%
#   report

daily_crime_mobility %>%
  model(auto = ARIMA(log(`VAW helpline calls`) ~ 1 +
                       trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(2,0,1) +
                        PDQ(2,0,0,
                            period = 7)),
        manual1 = ARIMA(log(`VAW helpline calls`) ~ 0 +
                        trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,0,3,
                            period = 7)),
        manual2 = ARIMA(log(`VAW helpline calls`) ~ 0 +
                        # trend() +
                        log(`BRT + SCT passengers (in millions)`) +
                        pdq(1,1,1) +
                        PDQ(1,0,3,
                            period = 7)),
        manual3 = ARIMA(log(`VAW helpline calls`) ~ 0 +
                       trend() +
                       log(`BRT + SCT passengers (in millions)`) +
                       pdq(2,0,4) +
                       PDQ(1,1,2,
                            period = 7)),
        manual4 = ARIMA(log(`VAW helpline calls`) ~ 0 +
                       # trend() +
                       log(`BRT + SCT passengers (in millions)`) +
                       pdq(2,0,4) +
                       PDQ(1,1,2,
                            period = 7))
        ) -> lm_arima_vaw_calls_manual

# lm_arima_vaw_calls_manual %>%
#   augment() %>%
#   ACF(.resid) %>%
#   autoplot()
#
# lm_arima_vaw_calls_manual %>%
#   augment() %>%
#   ggplot(aes(date, .resid)) +
#   geom_line() +
#   facet_wrap(~ .model)
#
# lm_arima_vaw_calls_manual %>%
#   augment() %>%
#   ggplot(aes(.resid)) +
#   geom_density() +
#   facet_wrap(~ .model)
#
# lm_arima_vaw_calls_manual %>%
#   augment() %>%
#   ggplot(aes(.fitted, .resid)) +
#   geom_hex() +
#   geom_smooth(method = "lm") +
#   scale_fill_continuous_tableau(palette = "Orange") +
#   facet_wrap(~ .model) +
#   theme(legend.position = "none")

lm_arima_vaw_calls_manual %>%
  glance
```

```
## # A tibble: 5 x 8
##   .model  sigma2 log_lik   AIC  AICc   BIC ar_roots   ma_roots
##   <chr>    <dbl>   <dbl> <dbl> <dbl> <dbl> <list>     <list>
## 1 auto    0.0493    102. -185. -185. -139. <cpl [16]> <cpl [1]>
## 2 manual1 0.0464    131. -244. -244. -198. <cpl [8]>  <cpl [22]>
## 3 manual2 0.0464    131. -246. -246. -205. <cpl [8]>  <cpl [22]>
## 4 manual3 0.0458    133. -242. -241. -180. <cpl [9]>  <cpl [18]>
## 5 manual4 0.0462    128. -234. -234. -178. <cpl [9]>  <cpl [18]>
```

```r
lb.test(lm_arima_vaw_calls_manual, lags = 21)
```

```
## # A tibble: 5 x 5
##   .model     LB    df   pvalue sig
##   <chr>   <dbl> <dbl>    <dbl> <chr>
## 1 auto     36.5    14 0.000887 "***"
## 2 manual1  22.5    13 0.0484   "*"
## 3 manual2  22.5    13 0.0479   "*"
## 4 manual3  17.8    10 0.0585   ""
## 5 manual4  21.0    10 0.0211   "*"
```

```r
lm_arima_vaw_calls_manual %>%
  augment() %>%
  features(.resid, unitroot_kpss) %>%
    mutate(sig = victim::add_stars(kpss_pvalue))
```

```
## # A tibble: 5 x 4
##   .model  kpss_stat kpss_pvalue sig
##   <chr>       <dbl>       <dbl> <chr>
## 1 auto       0.107          0.1 ""
## 2 manual1    0.0804         0.1 ""
## 3 manual2    0.0680         0.1 ""
## 4 manual3    0.0972         0.1 ""
## 5 manual4    0.0723         0.1 ""
```

```r
# Best model is manual 2

lm_arima_deterministic %>%
  filter(Variable == "VAW helpline calls") %>%
  select(lm) %>%
  report
```

```
## Series: value
## Model: LM w/ ARIMA(2,0,1)(2,0,0)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ar2      ma1    sar1    sar2
##       1.1349  -0.1483  -0.9105  0.1711  0.1319
## s.e.  0.0378   0.0343   0.0230  0.0301  0.0295
##       log(`BRT + SCT passengers (in millions)`)  trend()  intercept
##                                          0.1881    7e-04     3.0942
## s.e.                                     0.0297    2e-04     0.1252
##
## sigma^2 estimated as 0.04926:  log likelihood=101.55
## AIC=-185.1   AICc=-184.95   BIC=-138.99
```

```r
lm_arima_vaw_calls_manual %>%
  select(manual2) %>%
  report
```

```
## Series: VAW helpline calls
## Model: LM w/ ARIMA(1,1,1)(1,0,3)[7] errors
## Transformation: log(.x)
##
## Coefficients:
##          ar1      ma1    sar1     sma1     sma2     sma3
##       0.1295  -0.9024  0.9998  -0.9060  -0.0325  -0.0545
## s.e.  0.0345   0.0174  0.0003   0.0301   0.0401   0.0295
##       log(`BRT + SCT passengers (in millions)`)
##                                          0.1356
## s.e.                                     0.0328
##
## sigma^2 estimated as 0.04636:  log likelihood=130.97
## AIC=-245.95   AICc=-245.83   BIC=-204.97
```

```r
lm_arima_vaw_calls_manual %>%
  select(manual2) -> lm_arima_vaw_calls_manual_best
```

Now we recreate the residual plots of all models substituting the preferred manual fits for those crimes were problems were identified.



```r
## Residual checks

corrected_models <- c("All crimes",
                      "Violent robbery",
                      "Serious violent crime (non-sexual)",
                      "Domestic violence",
                      "VAW helpline calls")


list(lm_arima_all_crimes_manual_best,
     lm_arima_violent_robbery_manual_best,
     lm_arima_serious_violent_crime_manual_best,
     lm_arima_domestic_violence_manual_best,
     lm_arima_vaw_calls_manual_best) -> lm_manual_models_ls

names(lm_manual_models_ls) <- corrected_models

lm_manual_models_ls %>%
  map_df(~ .x %>%
           augment %>%
           mutate(.model = "lm",
                  value = .[[3]]) %>%
        select(.model,
               date,
               value,
               .fitted,
               .resid) %>%
          as_tibble(),
        .id = "Variable") %>%
  mutate(Variable = factor(Variable, levels = var_names[-c(9,20)])) %>%
  bind_rows(filter(lm_arima_augment,
                   !Variable %in% corrected_models)) -> lm_arima_augment_corrected

# residual density

lm_arima_augment_corrected %>%
  ggplot(aes(.resid)) +
  geom_density() +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  xlab("Residuals") +
  ylab("Density") +
  theme_clean()  -> lm_arima_residual_plot_corrected

## Residual vs fitted

lm_arima_augment_corrected %>%
  ggplot(aes(.fitted, .resid)) +
  geom_hex() +
  scale_fill_continuous_tableau(palette = "Orange") +
  geom_smooth(se = TRUE,
              method = "lm") +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  ylab("Residuals") +
  xlab("Fitted") +
  theme_clean() +
  theme(legend.position = "none") -> lm_arima_residual_plot2_corrected

lm_arima_augment_corrected %>%
  ggplot(aes(date, .resid)) +
  geom_line(size = 0.01, alpha = 0.5) +
  facet_wrap(~ Variable,
             scales = "free",
             ncol = 4) +
  ylab("Residuals") +
  xlab("Date") +
  theme_clean() +
  theme(legend.position = "none") -> lm_arima_residual_plot3_corrected

lm_arima_augment_corrected %>%
  as_tsibble(key = Variable)  %>%
  ACF(.resid, lag_max = 21) %>%
  autoplot() +
  # ggplot(aes(date, .resid)) +
  # geom_line(size = 0.05) +
  facet_wrap(~ Variable,
             # scales = "free",
             ncol = 4) +
  ylab("ACF") +
  xlab("Lags") +
  theme_clean() -> lm_arima_residual_plot4_corrected
```

```
## Using `date` as index variable.
```

```r
plot_grid(lm_arima_residual_plot3_corrected,
          lm_arima_residual_plot4_corrected,
          lm_arima_residual_plot_corrected,
          lm_arima_residual_plot2_corrected,
          labels = "AUTO",
          scale = 0.98,
          hjust = -0.9,
          vjust = 1.7) -> arima_all_residual_plots_corrected
```

```
## Warning: Removed 70 rows containing non-finite values (stat_density).
```

```
## Warning: Removed 70 rows containing non-finite values (stat_binhex).
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 70 rows containing non-finite values (stat_smooth).
```

```r
ggsave(filename = "plots/arima_all_residual_plots_corrected.pdf",
       plot = arima_all_residual_plots_corrected,
       device = cairo_pdf,
       width = 20,
       height = 12)
```

![](plots/arima_all_residual_plots_corrected.pdf)

Test residuals for autocorrelation and stationarity.


```r
# Ljung-Box

lm_manual_models_ls %>%
  map_df(~ .x %>%
        lb.test(lags = 21),
        .id = "Variable") %>%
  mutate(Variable = factor(Variable, levels = var_names[-c(9,10)])) %>%
  bind_rows(filter(lm_arima_lb,
                   !Variable %in% corrected_models)) %>%
  arrange(Variable) -> lm_arima_lb_corrected

# # check DF values are correct
# lm_manual_models_ls %>%
#   map(report)

# dfs:
# all crimes
21 - 6
```

```
## [1] 15
```

```r
# violent robbery
21 - 7
```

```
## [1] 14
```

```r
# serious violent crime
21 - 6
```

```
## [1] 15
```

```r
# domestic violence
21 - 7 # change to 14
```

```
## [1] 14
```

```r
# vaw calls
21 - 8
```

```
## [1] 13
```

```r
# lm_arima_deterministic %>%
#   filter(Variable == "Sexual violence") %>%
#   select(lm) %>%
#   report

# sexual violence
21 - 5 #  change to 16
```

```
## [1] 16
```

```r
(lm_arima_lb_corrected %>%
  mutate(df = ifelse(Variable %in% c("Sexual violence",
                                        "Domestic violence"),
                     df - 1,
                     df)) %>%
  mutate(pvalue = pchisq(LB, df, lower.tail = FALSE),
         sig = victim::add_stars(pvalue)) -> lm_arima_lb_corrected_2)
```

```
## # A tibble: 8 x 6
##   Variable                           .model     LB    df pvalue sig
##   <fct>                              <chr>   <dbl> <dbl>  <dbl> <chr>
## 1 All crimes                         manual2  20.3    15 0.160  ""
## 2 Violent robbery                    manual1  16.8    14 0.266  ""
## 3 Non-violent robbery                lm       18.3    15 0.247  ""
## 4 Robbery against residence          lm       15.0    16 0.522  ""
## 5 Serious violent crime (non-sexual) manual2  22.0    15 0.109  ""
## 6 Sexual violence                    lm       21.8    16 0.150  ""
## 7 Domestic violence                  manual2  22.2    14 0.0748 ""
## 8 VAW helpline calls                 manual2  22.5    13 0.0479 "*"
```

```r
## KPSS test

(lm_arima_augment_corrected %>%
  as_tsibble(key = Variable) %>%
  features(.resid, unitroot_kpss) %>%
  transmute(Variable = Variable,
            KPSS = kpss_stat,
            pvalue = kpss_pvalue,
            sig = victim::add_stars(pvalue)) -> lm_arima_corrected_kpss)
```

```
## Using `date` as index variable.
```

```
## # A tibble: 8 x 4
##   Variable                             KPSS pvalue sig
##   <fct>                               <dbl>  <dbl> <chr>
## 1 All crimes                         0.0861 0.1    ""
## 2 Violent robbery                    0.128  0.1    ""
## 3 Non-violent robbery                0.421  0.0680 ""
## 4 Robbery against residence          0.287  0.1    ""
## 5 Serious violent crime (non-sexual) 0.147  0.1    ""
## 6 Sexual violence                    0.173  0.1    ""
## 7 Domestic violence                  0.100  0.1    ""
## 8 VAW helpline calls                 0.0680 0.1    ""
```

```r
# # check for invertibility (Box-Steffensmeier, et al., 2014)

lm_arima_stochastic %>%
  tidy %>%
  bind_rows(tidy(lm_arima_deterministic)) %>%
  filter(!Variable %in% corrected_models) -> lm_arima_auto_tidy

lm_manual_models_ls %>%
  map_df(tidy,
         .id = "Variable") %>%
  mutate(Variable = factor(Variable,
                           levels = var_names[-c(9,10)])) %>%
  bind_rows(lm_arima_auto_tidy) %>%
  arrange(Variable) -> lm_arima_tidy_estimates

lm_arima_tidy_estimates %>%
  filter(abs(estimate) > 1)
```

```
## # A tibble: 1 x 7
##   Variable        .model term      estimate std.error statistic  p.value
##   <fct>           <chr>  <chr>        <dbl>     <dbl>     <dbl>    <dbl>
## 1 Sexual violence lm     intercept     1.37     0.173      7.94 4.53e-15
```

Now construct summary tables for crime-mobility models.



```r
## LM Arima specification

lm_arima_stochastic %>%
  as_tibble() %>%
  bind_rows(as_tibble(lm_arima_deterministic)) %>%
  filter(!Variable %in% corrected_models) %>%
  group_by(Variable) %>%
  group_map(~ .x %>%
              select(lm) %>%
              as_mable(model = lm)) -> lm_arima_auto_ls

names(lm_arima_auto_ls) <- c("Non-violent robbery",
                             "Robbery against residence",
                             "Sexual violence")

# Which models had linear trends
lm_arima_tidy_estimates %>%
  filter(str_detect(term, "trend")) %$%
  Variable %>% as.character() -> lm_with_trend

lm_manual_models_ls %>%
  append(lm_arima_auto_ls) -> lm_arima_corrected_models_ls

lm_arima_corrected_models_ls %>%
  map_df(~ .x[[1]][[1]] %>%
        model_sum %>%
        as_tibble() %>%
        transmute(Specification = str_remove(value,
                                    "LM w/ "),
                  Specification = str_remove(Specification,
                                    " errors")),
        .id = "Variable") %>%
  mutate(Variable = factor(Variable,
                           levels = var_names)) %>%
  arrange(Variable) %>%
  mutate(Specification = ifelse(Variable %in% lm_with_trend,
                                paste0(Specification, " w/ linear trend"),
                                Specification)) -> lm_arima_specs

# LM Arima accuracies

lm_arima_corrected_models_ls %>%
  map_df(accuracy,
         .id = "Variable") %>%
  mutate(Variable = factor(Variable,
                           levels = var_names)) %>%
  select(Variable, ME, MAPE) %>%
  arrange(Variable) -> lm_arima_accuracy_corrected

# LM ARIMA AICc

lm_arima_corrected_models_ls %>%
  map_df(glance,
         .id = "Variable") %>%
  mutate(Variable = factor(Variable,
                           levels = var_names)) %>%
  select(Variable, AICc) %>%
  arrange(Variable) -> lm_arima_aicc

# Ljung-Box

lm_arima_lb_corrected_2 %>%
  transmute(Variable = Variable,
            "Q (df)" = paste0(label_number()(LB),
                       " (",
                       df,
                       ")",
                       sig)) -> lm_arima_lb_pretty

## Pretty coefficient estimates

lm_arima_tidy_estimates %>%
  filter(str_detect(term, "BRT")) %>%
  transmute(Variable = Variable,
            "Mobility coefficient (SE)" = paste0(label_number()(estimate),
                                                 " (",
                                                 label_number(accuracy = 0.001)(std.error),
                                                 ")",
                                                 victim::add_stars(p.value))) %>%
  left_join(lm_arima_specs) %>%
  left_join(lm_arima_accuracy_corrected) %>%
  left_join(lm_arima_aicc) %>%
  left_join(lm_arima_lb_pretty) %>%
  left_join(select(lm_arima_corrected_kpss, Variable, KPSS)) -> lm_arima_summary
```

```
## Joining, by = "Variable"
## Joining, by = "Variable"
## Joining, by = "Variable"
## Joining, by = "Variable"
## Joining, by = "Variable"
```

```r
# ncol(lm_arima_summary)

lm_arima_summary %>%
  kable(digits = 2,
        align = c("l", "r", "c", "r", "r", "r", "r", "r"),
        caption = "Crime-mobility models estimates, specifications and GOF measures.")
```



Table: Crime-mobility models estimates, specifications and GOF measures.

|Variable                           | Mobility coefficient (SE)|             Specification              |   ME|  MAPE|     AICc|      Q (df)| KPSS|
|:----------------------------------|-------------------------:|:--------------------------------------:|----:|-----:|--------:|-----------:|----:|
|All crimes                         |          0.167 (0.014)***|         ARIMA(1,1,1)(1,0,1)[7]         | 0.73|  6.28| -2588.28|  20.33 (15)| 0.09|
|Violent robbery                    |          0.304 (0.020)***|         ARIMA(1,1,2)(1,0,1)[7]         | 0.18|  8.81| -1774.99|  16.81 (14)| 0.13|
|Non-violent robbery                |          0.373 (0.020)***|         ARIMA(0,1,2)(2,0,0)[7]         | 0.83| 11.26| -1256.83|  18.30 (15)| 0.42|
|Robbery against residence          |            -0.040 (0.037)|         ARIMA(0,1,1)(2,0,0)[7]         | 0.48| 23.88|   443.62|  15.04 (16)| 0.29|
|Serious violent crime (non-sexual) |            0.080 (0.035)*|         ARIMA(1,1,1)(1,0,1)[7]         | 0.50| 17.98|  -210.57|  21.96 (15)| 0.15|
|Sexual violence                    |          0.222 (0.047)***|      ARIMA(1,0,1) w/ linear trend      | 0.83| 35.60|  1342.43|  21.79 (16)| 0.17|
|Domestic violence                  |             0.000 (0.027)| ARIMA(1,1,1)(1,0,1)[7] w/ linear trend | 0.52| 13.61|  -824.66|  22.19 (14)| 0.10|
|VAW helpline calls                 |          0.136 (0.033)***|         ARIMA(1,1,1)(1,0,3)[7]         | 1.02| 17.26|  -245.83| 22.52 (13)*| 0.07|


Conduct robustness checks for crime-mobility models


```r
# Refit auto models

lm_arima_stochastic %>%
  as_tibble() %>%
  bind_rows(as_tibble(lm_arima_deterministic)) %>%
  filter(!Variable %in% corrected_models) %>%
  mable(key = Variable, model = lm) -> lm_arima_auto_mable

daily_crime_mobility_pre_covid %>%
  select(-c(`BRT + SCT passengers`)) %>%
  pivot_longer(-c(date, `BRT + SCT passengers (in millions)`),
               names_to = "Variable") %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  as_tsibble(key = Variable) %>%
  refit(lm_arima_auto_mable,
        new_data = . ,
        reestimate = TRUE) -> lm_arima_auto_mable_pre_covid


daily_crime_mobility_post_covid %>%
  select(-c(`BRT + SCT passengers`)) %>%
  pivot_longer(-c(date, `BRT + SCT passengers (in millions)`),
               names_to = "Variable") %>%
  mutate(Variable = fct_inorder(Variable)) %>%
  as_tsibble(key = Variable) %>%
  refit(lm_arima_auto_mable,
        new_data = .,
        reestimate = TRUE) -> lm_arima_auto_mable_post_covid


lm_arima_auto_mable_pre_covid %>%
  tidy %>%
  mutate(Period = "Pre-COVID-19") %>%
  bind_rows(lm_arima_auto_mable_post_covid %>%
              tidy %>%
              mutate(Period = "Post-COVID-19")) -> lm_arima_auto_pre_post_tidy

# Refit manual models

lm_manual_models_ls %>%
  map_df(~ .x %>% refit(new_data = daily_crime_mobility_pre_covid,
                     reestimate = TRUE) %>%
        tidy %>%
        mutate(Period = "Pre-COVID-19"),
        .id = "Variable") -> lm_manual_pre_tidy
```

```
## Warning in wrap_arima(y, order = c(p, d, q), seasonal = list(order = c(P, :
## possible convergence problem: optim gave code = 1
```

```r
lm_manual_models_ls %>%
  map_df(~ .x %>% refit(new_data = daily_crime_mobility_post_covid,
                     reestimate = TRUE) %>%
        tidy %>%
        mutate(Period = "Post-COVID-19"),
        .id = "Variable") -> lm_manual_post_tidy

lm_manual_pre_tidy %>%
  bind_rows(lm_manual_post_tidy) %>%
  mutate(Variable = factor(Variable, levels = var_names)) %>%
  bind_rows(lm_arima_auto_pre_post_tidy) %>%
  arrange(Variable) -> lm_arima_all_models_pre_post_tidy
```



Forest plot for mobility coefficient (move to end, include robustness checks estimates)


```r
# get CI for mobility estimates

ci_lower <- function(estimate, se, level = 0.95){
  alpha <- (1-level)/2
  critical <- abs(qnorm(alpha))

  lower <- estimate - se * critical
  # upper <- estimate + se * alpha

  lower
}

ci_upper<- function(estimate, se, level = 0.95){
  alpha <- (1-level)/2
  critical <- abs(qnorm(alpha))

  # lower <- estimate - se * alpha
  upper <- estimate + se * critical

  upper
}

mobility_drop <- 0.5

lm_arima_tidy_estimates %>%
  mutate(Period = "Pre- and post-COVID-19") %>%
  bind_rows(lm_arima_all_models_pre_post_tidy) %>%
  mutate(Period = fct_inorder(Period)) %>%
  filter(str_detect(term, "BRT")) %>%
  transmute(Variable = Variable,
            estimate = estimate,
            lower = ci_lower(estimate, std.error),
            upper = ci_upper(estimate, std.error),
            percent_estimate = (mobility_drop^estimate)-1,
            percent_lower = (mobility_drop^lower)-1,
            percent_upper = (mobility_drop^upper)-1,
            Period = Period) -> lm_arima_ci_complete

lm_arima_ci_complete %>%
  mutate(Variable = fct_rev(Variable)) %>%
  arrange(Variable) %>%
  ggplot(aes(percent_estimate, Variable, colour = Period)) +
  geom_point(position = position_dodge(width = -0.5))  +
  geom_vline(xintercept = 0, colour = "grey75") +
  geom_errorbarh(aes(xmin = percent_lower,
                     xmax = percent_upper), height = .3,
                 position = position_dodge(width = -0.5)) +
  theme_clean() +
  # scale_color_colorblind() +
  scale_color_manual(values = c("#000000", "#009E73", "#0072B2")) +
  scale_x_continuous("Estimated percentage change in crime for a 50% reduction in mobility",
                     labels = scales::percent) +
  ylab(NULL) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.box = "vertical") +
  guides(colour = guide_legend(nrow=2,byrow=TRUE)) +
  labs(caption = "Error bars represent 95% confidence interval") -> lm_arima_percent_change_plot

ggsave(filename = "plots/lm_arima_percent_change_plot.pdf",
       plot = lm_arima_percent_change_plot,
       device = cairo_pdf,
       width = 7,
       height = 6)
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

```r
lm_arima_ci_complete %>%
  mutate(change = paste0(label_percent(accuracy = 0.1)(percent_estimate),
                         " [",
                         label_percent(accuracy = 0.1)(percent_lower),
                         ", ",
                         label_percent(accuracy = 0.1)(percent_upper),
                         "]")) %>%
  select(Variable, Period, change) %>%
  pivot_wider(names_from = Period,
              values_from = change,
              names_glue = "{Period} [95% CI]") %>%
  kable(digits = 2,
        align = c("l","r","r","r"))
```



|Variable                           | Pre- and post-COVID-19 [95% CI]|   Pre-COVID-19 [95% CI]|  Post-COVID-19 [95% CI]|
|:----------------------------------|-------------------------------:|-----------------------:|-----------------------:|
|All crimes                         |          -10.9% [-9.2%, -12.5%]|  -10.4% [-8.6%, -12.0%]| -18.0% [-14.1%, -21.8%]|
|Violent robbery                    |         -19.0% [-16.7%, -21.2%]| -17.1% [-14.9%, -19.2%]| -31.2% [-29.2%, -33.1%]|
|Non-violent robbery                |         -22.8% [-20.7%, -24.9%]| -21.7% [-19.2%, -24.1%]| -21.7% [-14.1%, -28.6%]|
|Robbery against residence          |              2.8% [8.1%, -2.2%]|      6.1% [11.1%, 1.4%]|   -5.6% [29.0%, -31.0%]|
|Serious violent crime (non-sexual) |            -5.4% [-0.9%, -9.8%]|     -4.2% [0.5%, -8.7%]|     7.2% [27.5%, -9.9%]|
|Sexual violence                    |          -14.3% [-8.6%, -19.6%]|  -11.8% [-5.7%, -17.5%]|  -18.7% [-1.4%, -33.0%]|
|Domestic violence                  |              0.0% [3.8%, -3.6%]|      1.4% [5.2%, -2.3%]|     8.9% [31.3%, -9.6%]|
|VAW helpline calls                 |           -9.0% [-4.8%, -12.9%]|   -9.3% [-5.0%, -13.4%]|  -12.2% [-0.9%, -22.2%]|

![](plots/lm_arima_percent_change_plot.pdf)







```r
# save.image("tmp.Rdata")
```
