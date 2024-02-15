Assignment 2
================
John Chen
Feb 13, 2024

# Importing packages and data

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(tidyr)
```

``` r
life_exp <- data.table::fread("life-expectancy-of-women-vs-life-expectancy-of-men.csv")
alc <- data.table::fread("WHOAlcoholTotalPerCapita_2021-09-20v2.csv")
head(life_exp)
```

    ##         Entity     Code Year
    ## 1:    Abkhazia OWID_ABK 2015
    ## 2: Afghanistan      AFG 1950
    ## 3: Afghanistan      AFG 1951
    ## 4: Afghanistan      AFG 1952
    ## 5: Afghanistan      AFG 1953
    ## 6: Afghanistan      AFG 1954
    ##    Life expectancy - Sex: female - Age: at birth - Variant: estimates
    ## 1:                                                                 NA
    ## 2:                                                               28.4
    ## 3:                                                               28.6
    ## 4:                                                               29.1
    ## 5:                                                               29.6
    ## 6:                                                               29.9
    ##    Life expectancy - Sex: male - Age: at birth - Variant: estimates
    ## 1:                                                               NA
    ## 2:                                                             27.1
    ## 3:                                                             27.4
    ## 4:                                                             27.8
    ## 5:                                                             28.3
    ## 6:                                                             28.6
    ##    Population - Sex: all - Age: all - Variant: estimates Continent
    ## 1:                                                  <NA>      Asia
    ## 2:                                               7480464          
    ## 3:                                               7571542          
    ## 4:                                               7667534          
    ## 5:                                               7764549          
    ## 6:                                               7864289

``` r
head(alc)
```

    ##    WHO Region Code            WHO Region Country Code    Country Year
    ## 1:            SEAR       South-East Asia          BGD Bangladesh 2019
    ## 2:            SEAR       South-East Asia          BGD Bangladesh 2019
    ## 3:            SEAR       South-East Asia          BGD Bangladesh 2019
    ## 4:             EMR Eastern Mediterranean          KWT     Kuwait 2019
    ## 5:             EMR Eastern Mediterranean          KWT     Kuwait 2019
    ## 6:             EMR Eastern Mediterranean          KWT     Kuwait 2019
    ##           Sex Alcohol total per capita (15+) consumption in liters (numeric)
    ## 1: Both sexes                                                              0
    ## 2:     Female                                                              0
    ## 3:       Male                                                              0
    ## 4: Both sexes                                                              0
    ## 5:     Female                                                              0
    ## 6:       Male                                                              0
    ##    Alcohol total per capita (15+) consumption in liters (low estimation)
    ## 1:                                                                     0
    ## 2:                                                                     0
    ## 3:                                                                     0
    ## 4:                                                                     0
    ## 5:                                                                     0
    ## 6:                                                                     0
    ##    Alcohol total per capita (15+) consumption in liters (high estimation)
    ## 1:                                                                      0
    ## 2:                                                                      0
    ## 3:                                                                      0
    ## 4:                                                                      0
    ## 5:                                                                      0
    ## 6:                                                                      0
    ##    Alcohol total per capita (15+) consumption in liters (string)
    ## 1:                                                     0 [0 – 0]
    ## 2:                                                     0 [0 – 0]
    ## 3:                                                     0 [0 – 0]
    ## 4:                                                     0 [0 – 0]
    ## 5:                                                     0 [0 – 0]
    ## 6:                                                     0 [0 – 0]

# Data Wrangling & Merging

Some of the column names looks confusing and hard to work with. They
should be simplified.

``` r
life_exp <- life_exp %>% rename(
  "Female" = "Life expectancy - Sex: female - Age: at birth - Variant: estimates",
  "Male" = "Life expectancy - Sex: male - Age: at birth - Variant: estimates",
  "all_age" = "Population - Sex: all - Age: all - Variant: estimates"
) %>%
  pivot_longer(cols=c("Female","Male"), names_to = "Sex", values_to = "Age")
alc <- subset(alc, Sex != "Both sexes") %>%
  rename(
    "consumption" = "Alcohol total per capita (15+) consumption in liters (numeric)",
    "consumption_lowEst" = "Alcohol total per capita (15+) consumption in liters (low estimation)",
    "consumption_highEst" = "Alcohol total per capita (15+) consumption in liters (high estimation)",
    "consumption_interval" = "Alcohol total per capita (15+) consumption in liters (string)"
  )
```

Merging based on Country, Year, and Sex.

``` r
life_exp_alc <- merge(
  x= alc,
  y= life_exp,
  by.x=c("Country","Year","Sex"),
  by.y=c("Entity","Year", "Sex"),
  all.x= TRUE,
  all.y= TRUE
)
head(life_exp_alc)
```

    ##        Country Year    Sex WHO Region Code WHO Region Country Code consumption
    ## 1:    Abkhazia 2015 Female            <NA>       <NA>         <NA>          NA
    ## 2:    Abkhazia 2015   Male            <NA>       <NA>         <NA>          NA
    ## 3: Afghanistan 1950 Female            <NA>       <NA>         <NA>          NA
    ## 4: Afghanistan 1950   Male            <NA>       <NA>         <NA>          NA
    ## 5: Afghanistan 1951 Female            <NA>       <NA>         <NA>          NA
    ## 6: Afghanistan 1951   Male            <NA>       <NA>         <NA>          NA
    ##    consumption_lowEst consumption_highEst consumption_interval     Code all_age
    ## 1:                 NA                  NA                 <NA> OWID_ABK    <NA>
    ## 2:                 NA                  NA                 <NA> OWID_ABK    <NA>
    ## 3:                 NA                  NA                 <NA>      AFG 7480464
    ## 4:                 NA                  NA                 <NA>      AFG 7480464
    ## 5:                 NA                  NA                 <NA>      AFG 7571542
    ## 6:                 NA                  NA                 <NA>      AFG 7571542
    ##    Continent  Age
    ## 1:      Asia   NA
    ## 2:      Asia   NA
    ## 3:           28.4
    ## 4:           27.1
    ## 5:           28.6
    ## 6:           27.4
