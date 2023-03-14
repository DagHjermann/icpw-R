---
title: "164a1 Analyse NO3 medians 2012-2016 - all variables"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
<<<<<<< HEAD
    value: '164a1 Analyse NO3 medians 2012-2016 - all variables'
  text_line1: 
    value: 'Analysis of NO3 medians (2012-2016)'
  text_line2: 
    value: 'Dataset: NO3 medians data set incl. catchment_area + TOC'
  medians_filename:
    value: 'medians_2012-2016_no3.csv'        
  selected_vars: 
    value: 'median_no3, catchment_area, median_toc,slope_dep_vs_time, TOTN_dep, latitude, longitude, pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse'
  tree_formula:
    value: 'median_no3 ~ .'
  extra_pairwise_plots:
    value: 'TOC,NO3; slope_dep_vs_time,TOTN_dep'
=======
    value: '164x Analyse NO3 status - test run'
  text_line1: 
    value: 'Analysis of NO3 medians (2012-2016)'
  text_line2: 
    value: 'Data with NO3, TOTN_dep, slope_dep_vs_time, TOC, tmp (temperature), pre (precipitation)'
  medians_filename:
    value: 'medians_2012-2016_no3.csv'        
  selected_vars: 
    value: 'median_no3,catchment_area, median_toc,slope_dep_vs_time, TOTN_dep, latitude, longitude, pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse'
  tree_formula:
    value: 'median_no3 ~ .'
  extra_pairwise_plots:
    value: 'TOC,NO3; TOTN_dep,slope_dep_vs_time'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
  pairwise_plots_same_scale:
    value: 'FALSE'
  logistic_formula: 
    value: 'median_no3 ~ TOTN_dep + slope_dep_vs_time + TOTN_dep:slope_dep_vs_time + median_toc + TOTN_dep:median_toc + tmp + pre + decid_mixed + bare_sparse + coniferous + catchment_area + lake_water + total_shrub_herbaceous'

---




<<<<<<< HEAD

=======
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
**Analysis of NO3 medians (2012-2016)**   

**Dataset: NO3 medians data set incl. catchment_area + TOC**   
**Name of dataset: medians_2012-2016_no3.csv**   


* Response variable: 'Current NO3 level' (locations with signif. *increase* are *not* excluded)  
* Data from https://github.com/JamesSample/icpw2/tree/master/thematic_report_2020/results      
<<<<<<< HEAD
* Sen slope of NO3, TOTN, TOC/TON etc. 1992-2016
=======
* Sen slope of NO3, TON, TOC/TON etc. 1992-2016
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
* Response variable in all analyses are *whether NO3 decreases or not*     
* Predictors:
    - slope_dep_vs_time: Trend in Tot-N deposition 1992-2016    
    - NO3, TOTN_dep: Medians of NO3, TOTN_dep (Tot-N deposition) 1992-2016   
    - catchment_area (if included in data)      
    - TOC: Medians of TOC 1992-2016 (if included in data)     
    - pre, tmp: mean precipitation + temp   
    - Land cover 
  
Technical details: This html file was created was 

## 1. Libraries  

```r
# All of these packages cn be loaded at once using library(tidyverse). (I just like to be specific.)
library(dplyr)
library(tidyr)      # pivot_wider
library(purrr)      # 'map' functions  
library(lubridate)  
library(ggplot2)

# Too many packages, not all are used
# library(mapview)
library(visreg)     # visreg
library(rkt)        # Theil -Sen Regression

library(MuMIn)      

# Trees and forests
library(party)                  # ctree
library(evtree)                 # evtree
library(randomForest)
library(randomForestExplainer)  # measure_importance, plot_multi_way_importance
library(pdp)                    # partial, autoplot

library(maps)
my_map <- map_data("world")

library(effects)    # handles lme models  
library(readxl)
library(readr)
<<<<<<< HEAD
=======
library(stringr)    # str_extract
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

source("002_Functions.R")
source("160parm_functions.R")

knitr::opts_chunk$set(results = 'hold') # collect the results from a chunk  
knitr::opts_chunk$set(warning = FALSE)  

options(width = 95)
```



## 2. Data  
<<<<<<< HEAD
* The data part (part 2) is quite similar in scripts 160-164 
=======
* The data part (part 2) is quite similar in scripts 160 - 165 
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

### Available files

```
## Medians results: 'medians_2012-2016_no3.csv' ,n = 494 
## 
## Number of values per variable: 
## 
## Medians results: 'medians_2012-2016_toc_totn_no3.csv' ,n = 310 
## 
## Number of values per variable:
```

### James' trends and medians     

```r
#
# Median results 2012-2016  
#
folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/medians_2012-2016"
file <- params$medians_filename
fn <- paste0(folder, "/", file)
df_medians <- read.csv(fn, encoding = "UTF-8")
cat("Medians results:", sQuote(file), ",n =", nrow(df_medians), "\n\n")  
cat("Number of values per variable: \n")
apply(!is.na(df_medians), 2, sum)
cat("\n")

# Station metadata
# WILL BE ADDED TOGETHER WITH LAND COVER, BELOW
# file_meta <- gsub(".csv", "_stations.csv", file)
# fn <- paste0(folder, "/", file_meta)
# df_metadata <- read.csv(fn, encoding = "UTF-8")
# cat("Regression result metadata:", sQuote(file_meta), ",n =",  
#      nrow(df_metadata), "\n\n")


#
# Regression results 1992-2016
#
folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_no3"
file <- "trends_1992-2016_no3_results.csv"
fn <- paste0(folder, "/", file)
df_trends <- read.csv(fn, encoding = "UTF-8")
cat("Trends used as predictor variable:", sQuote(file), ",n =", nrow(df_trends), "\n\n")
```

```
## Medians results: 'medians_2012-2016_no3.csv' ,n = 494 
## 
## Number of values per variable: 
##   station_id NH4.N_µg.l.N NO3.N_µg.l.N   TOC_mg.C.l  TOTN_µg.l.N  TOTP_µg.l.P   TON_µg.l.N 
##          494          382          494          487          316          365          231 
##    TOTN.TOTP     NO3.TOTP      TOC.TON     TOC.TOTP 
##          235          364          227          357 
## 
## Trends used as predictor variable: 'trends_1992-2016_no3_results.csv' ,n = 3176
```

### Start 'dat'  
Using medians    
* Make one line per station   
* Also including some trends for  

```r
# table(reg_no3$variable)

# Medians 2012-2016  
df1 <- df_medians %>%
<<<<<<< HEAD
  select(station_id, `NO3.N_µg.l.N`, `TOTN_µg.l.N`, `TON_µg.l.N`, `TOC_mg.C.l`, TOC.TON) %>%
  rename(median_no3 = `NO3.N_µg.l.N`,
         median_totn = `TOTN_µg.l.N`,
=======
  select(station_id, `NO3.N_µg.l.N`, `TON_µg.l.N`, `TON_µg.l.N`, `TOC_mg.C.l`, TOC.TON) %>%
  rename(median_no3 = `NO3.N_µg.l.N`,
         median_ton = `TON_µg.l.N`,
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
         median_ton = `TON_µg.l.N`,
         median_toc = `TOC_mg.C.l`,
         median_tocton = `TOC.TON`) %>%
  mutate(log_median_no3 = log10(median_no3 + 0.1),
<<<<<<< HEAD
         log_median_totn = log10(median_totn),
=======
         log_median_ton = log10(median_ton),
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
         log_median_ton = log10(median_ton),
         log_median_toc = log10(median_toc),
         log_median_tocton = log10(median_tocton))

# Some trends
df2 <- df_trends %>% # table()
  filter(variable %in% c("NO3-N_µg/l N", "TOTN_µg/l N", "TOC_mg C/l")) %>%
  select(station_id, variable, median) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "median") %>% # str()
  rename(trend_NO3 = `NO3-N_µg/l N`,
         trend_TOTN = `TOTN_µg/l N`,
         trend_TOC = `TOC_mg C/l`)

cat("\n")
cat("df1, n =", nrow(df1), "\n")
cat("df2, n =", nrow(df2), "\n")

<<<<<<< HEAD
dat_1 <- df1 %>%
  left_join(df2, by = "station_id")

cat("dat_1, n =", nrow(dat_1), "\n")
=======
dat_1_allrows <- df1 %>%
  left_join(df2, by = "station_id")

response_var <- str_extract(params$tree_formula, "[^[[:blank:]]]+")

cat("dat_1_allrows, n =", nrow(dat_1_allrows), 
    " (may include series where", response_var,  "= NA)\n")

sel <- !is.na(dat_1_allrows[[response_var]])

dat_1 <- dat_1_allrows[sel,]

cat("dat_1, n =", nrow(dat_1), 
    " (series where", response_var,  "has values)\n")
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

```
## 
## df1, n = 494 
## df2, n = 498 
<<<<<<< HEAD
## dat_1, n = 494
```


=======
## dat_1_allrows, n = 494  (may include series where median_no3 = NA)
## dat_1, n = 494  (series where median_no3 has values)
```


```r
sum(is.na(dat_1$log_median_no3))
sum(is.na(dat_1$log_median_tocton))
```

```
## [1] 0
## [1] 267
```

>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
### Deposition trends and median 1992-2006     

```r
fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/deposition/totn_dep_trends_icpw_stns.csv"  

df_deposition <- read.csv(fn) %>% 
  filter(variable == "totn_mgNpm2")  

cat("n =", nrow(df_deposition), "\n")
```

```
## n = 556
```


### Add deposition slope and medians to data  

```r
# debugonce(left_join2)
dat_2 <- dat_1 %>% 
  left_join2(df_deposition %>% 
              select(station_id, median, sen_slp, mk_p_val) %>%
              rename(TOTN_dep = median,
                     slope_dep_vs_time = sen_slp,
                     p_dep_vs_time = mk_p_val),
             by = "station_id",
             print_vars = TRUE)
```

```
## Variables before join: 
<<<<<<< HEAD
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN'
=======
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
```

### Add medians and station metadata   


```r
# dat_2 <- dat_2 %>%
#   left_join(df_metadata, by = "station_id")

# cat("dat_2, n =", nrow(dat_2), "\n")

# Simplify names by removing units
# names(dat_2)
# names(dat_2) <- sub(".N_µg.l.N", "", names(dat_2))
# names(dat_2) <- sub("_mg.C.l", "", names(dat_2))
# names(dat_2) <- sub("_µg.l.P", "", names(dat_2))

# cat("\nVariable names: \n")
# names(dat_2)
```

### Add climate and deposition medians and slopes  

```r
fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/climate/cru_climate_trends_icpw_stns.csv"

df_climate_mean <- read_csv(fn) %>% 
  select(station_id, variable, median) %>%
  pivot_wider(names_from = "variable", values_from = "median")
```

```
## Rows: 1112 Columns: 8
```

```
<<<<<<< HEAD
## -- Column specification -----------------------------------------------------------------------
=======
## ── Column specification ───────────────────────────────────────────────────────────────────────
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
```

```
## 
<<<<<<< HEAD
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
=======
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

```r
cat("\n")
# names(df_climate_mean)

df_climate_slope <- read_csv(fn) %>%
  select(station_id, variable, sen_slp) %>%
<<<<<<< HEAD
  pivot_wider(names_from = "variable", values_from = "sen_slp", names_prefix = "Slope_")
=======
  pivot_wider(names_from = "variable", values_from = "sen_slp", names_prefix = "slope_")
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

```
## Rows: 1112 Columns: 8
```

```
<<<<<<< HEAD
## -- Column specification -----------------------------------------------------------------------
=======
## ── Column specification ───────────────────────────────────────────────────────────────────────
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
```

```
## 
<<<<<<< HEAD
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
=======
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

```r
# Add
dat_3 <- dat_2 %>%
  left_join2(df_climate_mean, by = "station_id", print_vars = TRUE) %>%
  left_join2(df_climate_slope, by = "station_id", print_vars = TRUE)
```

```
## 
## Variables before join: 
<<<<<<< HEAD
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
=======
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
<<<<<<< HEAD
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
=======
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
<<<<<<< HEAD
## 'Slope_pre', 'Slope_tmp'
=======
## 'slope_pre', 'slope_tmp'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

### Combine land cover types   
* Data including UK read using script 159   
* Note: also includes metadata (country, etc.)
* bare_sparse = bare_rock + sparsely_vegetated + glacier   
* Select: coniferous, deciduous, lake, mixed_forest, wetland, bare_sparse   


```r
# df_landcover3_OLD <- readRDS("Data/159_df_landcover3.rds")
df_landcover3 <- readRDS("Data/159_df_meta3.rds")

df_landcover3 <- df_landcover3 %>%
  mutate(bare_sparse = bare_rock + sparsely_vegetated + glacier,
         decid_mixed = deciduous + mixed_forest,
         lake_water = lake + water_ex_lake) %>%
  select(-c(bare_rock, sparsely_vegetated, glacier, deciduous, mixed_forest, lake, water_ex_lake))
```


### Add land cover columns to main data    

```r
dat_4 <- left_join2(dat_3, 
                   df_landcover3, 
                   by = "station_id", 
                   print_vars = TRUE
)
```

```
## Variables before join: 
<<<<<<< HEAD
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'Slope_pre', 'Slope_tmp'
=======
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'slope_pre', 'slope_tmp'
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'station_code', 'station_name', 'latitude', 'longitude', 'altitude', 'continent', 'country', 'region', 'group', 'catchment_area', 'urban', 'cultivated', 'total_forest', 'coniferous', 'total_shrub_herbaceous', 'grasslands', 'heathlands', 'transitional_woodland_shrub', 'wetland', 'other', 'bare_sparse', 'decid_mixed', 'lake_water'
```

<<<<<<< HEAD
### Drop locations with >10% cultivated    

```r
dat_5 <- dat_4 %>%
  filter(cultivated <= 10)

cat(nrow(dat_4) - nrow(dat_5), "stations with > 10% cultivated deleted \n")
```

```
## 31 stations with > 10% cultivated deleted
```


=======


### Drop locations with >5% cultivated and >5% urban     
- also excluding stations 23517, 38273    

```r
cultivated_threshold <- 5
urban_threshold <- 5

dat_5 <- dat_4 %>%
  filter2(!station_id %in% c(23517, 38273), text = "Deleted stations 23517, 38273") %>%
  filter2(cultivated <= cultivated_threshold, 
          text = paste("Deleted stations with >", cultivated_threshold, "% cultivated")) %>%
  filter2(urban <= urban_threshold, 
          text = paste("Deleted stations with >", urban_threshold, "% urban"))
```

```
## Removed 1 rows (Deleted stations 23517, 38273)
## Removed 40 rows (Deleted stations with > 5 % cultivated)
## Removed 6 rows (Deleted stations with > 5 % urban)
```



>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
### Data set used  

```r
dat <- dat_5
```


<<<<<<< HEAD

=======
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 3. Plot data      


```r
<<<<<<< HEAD
gg <- ggplot(dat, aes(TOTN_dep, log_median_no3)) + 
=======
gg <- ggplot(dat, aes(TOTN_dep, median_no3)) + 
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
  
gg
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5


## 4. Select data   

### a. Selection of variables    
* Select variables to use, and thereby also cases  
* Also remove PL05, which has dubious values   

```r
<<<<<<< HEAD
get_data_for_analysis <- function(data, variable_string){
=======
add_flag_variable <- function(data, variable_string){
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
  variable_string <- gsub(" ", "", variable_string)
  variables <- strsplit(variable_string, split = ",")[[1]]
  # Check if all variables are there
  found <- variables %in% names(data)
  if (sum(!found) > 0)
    stop("Not all variables found in data:", 
      paste(variables[!found], collapse = " ,"), 
      "\n")
  # Data for analyses
<<<<<<< HEAD
=======
  complete <- complete.cases(data[variables])
  data$Row_excluded <- !complete
  variables %>% 
    purrr::map_dfr(~data.frame(Var = .x, Missing = sum(is.na(data[[.x]])))) %>%
    print()
  data
}

delete_unused_variables <- function(data, variable_string){
  variable_string <- gsub(" ", "", variable_string)
  variables <- strsplit(variable_string, split = ",")[[1]]
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
  data[variables]
}

cat("-------------------------------------------------------------\n")
cat("Variables: \n")
cat(params$selected_vars)
cat("\n-------------------------------------------------------------\n")

<<<<<<< HEAD
sel <- dat$station_code %in% "PL05"
dat <- dat[!sel,]
message(sum(sel), " station removed - station PL05 (has dubious NO3 data)")  
```

```
## 1 station removed - station PL05 (has dubious NO3 data)
```

```r
# debugonce(get_data_for_analysis)
# df_analysis <- get_data_for_analysis(dat, vars)  
df_analysis <- get_data_for_analysis(dat, params$selected_vars)  

# names(dat) %>% paste(collapse = ", ")

cat("Number of missing values per variable: \n")
apply(is.na(df_analysis), 2, sum) 
cat("\n")

# What is missing? (long output)
if (FALSE){
dat %>% 
  split(.$country) %>%
  purrr::map(~apply(is.na(.), 2, mean))
}

cat("Number of complete observations: \n")
complete <- complete.cases(df_analysis)
table(complete)

cat("\n\n")
cat("Number of complete observations by country: \n")
table(dat$country, complete)

# Keep only complete cases
df_analysis <- df_analysis[complete.cases(df_analysis),]

cat("\n\n")
cat("Original data: n =", nrow(dat), "\n")
cat("Analysis: n =", nrow(df_analysis), "\n")
=======
dat <- dat %>%
  filter2(!station_code %in% "PL05", text = "station PL05 (has dubious NO3 data)")

# debugonce(add_flag_variable)
# df_analysis <- add_flag_variable(dat, vars)  
df_analysis_allrows <- add_flag_variable(dat, params$selected_vars)  

# Save to excel
fn <- paste0(substr(params$document_title, 1, 3), "_", response_var, "_data.xlsx")
writexl::write_xlsx(df_analysis_allrows, paste0("Data_analysed/", fn))
cat("\nDataset after removing urban, cultivated, PL05 saved as", sQuote(fn), "\n\n")

cat("Number of rows that will be excluded: \n")
table(df_analysis_allrows$Row_excluded)

cat("\n\n")
cat("Number of complete observations by country: \n")
xtabs(~country + Row_excluded, df_analysis_allrows)

# Keep only complete cases
df_analysis <- df_analysis_allrows %>%
  filter(!Row_excluded)

# Save to excel
fn <- paste0(
  stringr::str_extract(params$document_title, "[^[[:blank:]]]+"),
  "_data.xlsx")
writexl::write_xlsx(df_analysis, paste0("Data_analysed/", fn))

# Remove variables that will note be used
df_analysis <- delete_unused_variables(df_analysis, params$selected_vars)

cat("\n\n")
cat("Data before removing PL05: n =", nrow(dat_5), "\n")
cat("Data after removing PL05: n =", nrow(df_analysis_allrows), "\n")
cat("Data after removing missing predictors: n =", nrow(df_analysis), "\n")
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

```
## -------------------------------------------------------------
## Variables: 
## median_no3,catchment_area, median_toc,slope_dep_vs_time, TOTN_dep, latitude, longitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
<<<<<<< HEAD
## Number of missing values per variable: 
##             median_no3         catchment_area             median_toc      slope_dep_vs_time 
##                      0                     73                      4                      0 
##               TOTN_dep               latitude              longitude                    pre 
##                      0                      0                      0                      0 
##                    tmp                  urban             cultivated             coniferous 
##                      0                      0                      0                      8 
##            decid_mixed total_shrub_herbaceous                wetland             lake_water 
##                      8                      0                      0                      0 
##            bare_sparse 
##                      0 
## 
## Number of complete observations: 
## complete
## FALSE  TRUE 
##    85   377 
## 
## 
## Number of complete observations by country: 
##                 complete
##                  FALSE TRUE
##   Canada             0  113
##   Czech Republic     0    8
##   Finland            0   23
##   Germany            0    3
##   Ireland            0   10
##   Italy              0    4
##   Latvia             0    1
##   Netherlands        1    2
##   Norway             0   82
##   Poland             3    6
##   Slovakia           0   12
##   Sweden             0   91
##   Switzerland        8    0
##   United Kingdom     0   22
##   United States     73    0
## 
## 
## Original data: n = 462 
## Analysis: n = 377
=======
## Removed 1 rows (station PL05 (has dubious NO3 data))
##                       Var Missing
## 1              median_no3       0
## 2          catchment_area      73
## 3              median_toc       3
## 4       slope_dep_vs_time       0
## 5                TOTN_dep       0
## 6                latitude       0
## 7               longitude       0
## 8                     pre       0
## 9                     tmp       0
## 10                  urban       0
## 11             cultivated       0
## 12             coniferous      12
## 13            decid_mixed      12
## 14 total_shrub_herbaceous       0
## 15                wetland       0
## 16             lake_water       0
## 17            bare_sparse       0
## 
## Dataset after removing urban, cultivated, PL05 saved as '164_median_no3_data.xlsx' 
## 
## Number of rows that will be excluded: 
## 
## FALSE  TRUE 
##   358    88 
## 
## 
## Number of complete observations by country: 
##                 Row_excluded
## country          FALSE TRUE
##   Canada           103    4
##   Czech Republic     8    0
##   Finland           22    0
##   Germany            3    0
##   Ireland           10    0
##   Italy              3    0
##   Latvia             1    0
##   Netherlands        2    1
##   Norway            80    0
##   Poland             5    2
##   Slovakia          12    0
##   Sweden            87    0
##   Switzerland        0    8
##   United Kingdom    22    0
##   United States      0   73
## 
## 
## Data before removing PL05: n = 447 
## Data after removing PL05: n = 446 
## Data after removing missing predictors: n = 358
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```r
# SHOULD also workaccording to ?element_rect (update ggplot2?)
# gg + theme(plot.margin = margin(.6, .5, .6, 1.7, "cm"))
```



## 5. Tree and forest classification


### Split into training and validation data

```r
set.seed(123)

#
# NOT RELEVANT
#
# x <- runif(nrow(df_analysis))
# train <- ifelse(x < 0.9, TRUE, FALSE)
# 
# train_set <- df_analysis[train,]  %>% 
#   select(-longitude, - latitude) %>%
#   as.data.frame()
# valid_set <- df_analysis[!train,] %>% 
#   select(-longitude, - latitude) %>%
#   as.data.frame()

full_set <- df_analysis  %>% 
  select(-longitude, - latitude) %>%
  as.data.frame()

# plot(full_set)
```


### a. Tree classification using 'party'   

```r
<<<<<<< HEAD
# train_set$X <- 10^train_set$log_median_no3
=======
# train_set$X <- 10^train_set$median_no3
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
# (ct = ctree(X ~ ., data = train_set))

(ct = ctree(as.formula(params$tree_formula), 
            data = full_set))

plot(ct, main="Conditional Inference Tree")
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```
## 
## Model formula:
## median_no3 ~ catchment_area + median_toc + slope_dep_vs_time + 
##     TOTN_dep + pre + tmp + urban + cultivated + coniferous + 
##     decid_mixed + total_shrub_herbaceous + wetland + lake_water + 
##     bare_sparse
## 
## Fitted party:
## [1] root
<<<<<<< HEAD
## |   [2] TOTN_dep <= 1310.84
## |   |   [3] TOTN_dep <= 797.3
## |   |   |   [4] wetland <= 39.918
## |   |   |   |   [5] slope_dep_vs_time <= -11.51806
## |   |   |   |   |   [6] cultivated <= 1.971
## |   |   |   |   |   |   [7] bare_sparse <= 36.672: 37.018 (n = 87, err = 69675.6)
## |   |   |   |   |   |   [8] bare_sparse > 36.672: 78.500 (n = 7, err = 20911.5)
## |   |   |   |   |   [9] cultivated > 1.971: 83.143 (n = 7, err = 10704.9)
## |   |   |   |   [10] slope_dep_vs_time > -11.51806
## |   |   |   |   |   [11] slope_dep_vs_time <= -2.5575: 17.376 (n = 143, err = 14411.1)
## |   |   |   |   |   [12] slope_dep_vs_time > -2.5575: 2.974 (n = 19, err = 69.2)
## |   |   |   [13] wetland > 39.918: 222.889 (n = 9, err = 78520.9)
## |   |   [14] TOTN_dep > 797.3
## |   |   |   [15] bare_sparse <= 47.412
## |   |   |   |   [16] coniferous <= 2.15517: 167.743 (n = 23, err = 568779.7)
## |   |   |   |   [17] coniferous > 2.15517
## |   |   |   |   |   [18] pre <= 839.90002: 42.214 (n = 21, err = 12463.3)
## |   |   |   |   |   [19] pre > 839.90002: 95.671 (n = 26, err = 75543.3)
## |   |   |   [20] bare_sparse > 47.412: 275.487 (n = 13, err = 83773.4)
## |   [21] TOTN_dep > 1310.84: 352.006 (n = 22, err = 1496398.0)
## 
## Number of inner nodes:    10
## Number of terminal nodes: 11
=======
## |   [2] TOTN_dep <= 1092.63
## |   |   [3] bare_sparse <= 81.231
## |   |   |   [4] TOTN_dep <= 794.88144
## |   |   |   |   [5] wetland <= 39.918
## |   |   |   |   |   [6] slope_dep_vs_time <= -11.51806
## |   |   |   |   |   |   [7] bare_sparse <= 36.672: 38.531 (n = 89, err = 73352.5)
## |   |   |   |   |   |   [8] bare_sparse > 36.672: 78.500 (n = 7, err = 20911.5)
## |   |   |   |   |   [9] slope_dep_vs_time > -11.51806
## |   |   |   |   |   |   [10] slope_dep_vs_time <= -2.5575: 17.261 (n = 137, err = 14368.0)
## |   |   |   |   |   |   [11] slope_dep_vs_time > -2.5575: 3.300 (n = 15, err = 60.9)
## |   |   |   |   [12] wetland > 39.918: 243.750 (n = 8, err = 47187.5)
## |   |   |   [13] TOTN_dep > 794.88144
## |   |   |   |   [14] bare_sparse <= 14.805: 80.812 (n = 51, err = 249864.8)
## |   |   |   |   [15] bare_sparse > 14.805: 201.123 (n = 8, err = 23530.1)
## |   |   [16] bare_sparse > 81.231: 307.973 (n = 7, err = 44483.0)
## |   [17] TOTN_dep > 1092.63: 260.924 (n = 36, err = 2140191.2)
## 
## Number of inner nodes:    8
## Number of terminal nodes: 9
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = full_set)

plot(ev.raw)
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5


### c. Random forest  
* Model called 'model1'

```r
model1 <- randomForest(as.formula(params$tree_formula), 
                       data = full_set, 
                       mtry = 5,
                       importance = TRUE)

model1
```

```
## 
## Call:
##  randomForest(formula = as.formula(params$tree_formula), data = full_set,      mtry = 5, importance = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
<<<<<<< HEAD
##           Mean of squared residuals: 7574.631
##                     % Var explained: 50.49
=======
##           Mean of squared residuals: 7448.989
##                     % Var explained: 49.14
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```


#### c1. Predict on training data
Not relevant here  

#### c2. Importance of variables   
* Classification forests result in   
    - mean_min_depth   
    - accuracy_decrease  
    - gini_decrease  
    - no_of_nodes   
    - times_a_root      
* Regression forests result in     
    - mean_min_depth   
    - mse_increase  
    - node_purity_increase  
    - no_of_nodes   
    - times_a_root      

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 6)  
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 6)
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-21-2.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1                TOTN_dep       1.324000        5871   7042.60326           1268527.07
## 2                     tmp       1.988000        5229   3352.47554            701008.64
## 3       slope_dep_vs_time       2.390000        5983   2554.38138            520806.86
## 4              coniferous       3.466000        3977    359.69404            361586.75
## 5             decid_mixed       4.236000        3852    578.41061            183684.60
## 6              lake_water       3.362000        4930    582.81048            312435.96
## 7                 wetland       3.408000        4151   1219.62508            281920.15
## 8             bare_sparse       3.482000        2583    888.88533            243594.31
## 9              median_toc       3.014000        5309   1243.09094            369937.11
## 10 total_shrub_herbaceous       3.980000        3892    360.78477            181105.33
## 11         catchment_area       3.448000        5142    133.85218            273586.19
## 12             cultivated       6.579644        1185     19.06669             15362.58
## 13                    pre       3.948000        4530    611.58389            171337.86
## 14                  urban       6.247232        1932    157.90945             35071.54
##    no_of_trees times_a_root       p_value
## 1          500          172 3.475231e-145
## 2          500          100  3.052973e-59
## 3          500           61 9.676454e-164
## 4          500           41  9.995831e-01
## 5          500           41  1.000000e+00
## 6          500           39  9.151393e-32
## 7          500           23  6.999929e-01
## 8          500           12  1.000000e+00
## 9          500            7  5.487369e-68
## 10         500            4  9.999989e-01
## 11         500            0  2.164183e-50
## 12         471            0  1.000000e+00
## 13         500            0  2.022860e-08
## 14         488            0  1.000000e+00
```
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5



#### c3. Random forest, show partial effects  


```r
# Which variables to include:
variables_for_plot <- importance %>%
  mutate(variable = levels(variable)[as.numeric(variable)]) %>%
  arrange(desc(mse_increase)) %>%    # gini_decrease for classification trees
  pull(variable) %>%
  head(12)   # pick the first 12 variables (or less)

# Calculation

plotdata <- NULL  # will be list for storing results

max_number_of_plots <- length(variables_for_plot)/2 %>% floor()

for (i in 1:max_number_of_plots){
  varno1 <- c(1,3,5,7,9,11)[i]
  varno2 <- varno1 + 1
  plotdata[[i]] <- model1 %>%
    partial(pred.var = variables_for_plot[c(varno1, varno2)], chull = TRUE, progress = "text",
            which.class = "1", prob = TRUE)
}
```



```r
### Extra plots

plotpairs <- params$extra_pairwise_plots %>%
  gsub(" ", "", ., fixed = TRUE) %>%
  strsplit(split = ";") %>%
  .[[1]] %>%
  purrr::map(~strsplit(., split = ",")[[1]])

for (plotvar in plotpairs){
  # print(plotvar)
  if (plotvar[1] %in% names(full_set) & plotvar[2] %in% names(full_set)){
    i <- i + 1
    plotdata[[i]] <- model1 %>%
      partial(pred.var = c(plotvar[1], plotvar[2]), chull = TRUE, progress = "text",
             which.class = "1", prob = TRUE)
  }
}
```



```r
# Find range of predicted values for each graph
ranges <- plotdata %>% purrr::map_dfc(~range(.$yhat))
```

```
## New names:
## * NA -> ...1
## * NA -> ...2
## * NA -> ...3
## * NA -> ...4
## * NA -> ...5
## * ...
```

```r
# use range of all the ranges
for (i in 1:length(plotdata)){
  
  if (params$pairwise_plots_same_scale == "TRUE"){
  
    gg <- plot_pair_number(i, zrange = range(ranges))
    print(gg)
    
  } else {
    
    gg <- plot_pair_number(i)
    print(gg)

  }
  
<<<<<<< HEAD
  # Save gg obkjct for later plotting / changes
  fn <- paste0(
    "Figures/Partial_plots/gg_",
    params$document_title %>% stringr::str_extract("([^[[:blank:]]]+)"),
    "_", i, ".rds")
  saveRDS(gg, fn)

=======
  # Save gg object for later plotting / changes
  # Saved in Figures/Partial_plots' with name e.g. "gg_164a1_7.rds" for plot number 7
  fn <- paste0(
    "Figures/Partial_plots/gg_",
    stringr::str_extract(params$document_title, "([^[[:blank:]]]+)"),   # extract e.g. "164a1"
    "_", i, ".rds")
  saveRDS(gg, fn)
  
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
}
```

![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](164a1_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->


<<<<<<< HEAD
=======


>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## 6. Linear regression      

```r
fm <- lm(
  as.formula(params$logistic_formula),
  data = df_analysis, 
  na.action = "na.fail")

dredged_models <- dredge(fm)                       # only once
```

```
## Fixed term is "(Intercept)"
```

```r
# saveRDS(dredged_models, "Data/164_all_dredged_models.rds")    # save it as it takes a couple of minutes
# dredged_models <- readRDS("Data/164_all_dredged_models.rds")

# cat("\n\nR2: \n")
# mod1 <- get.models(dredged_models, 1)[[1]]  
# summary(mod1)  
```

### Best models  

```r
# subset(dredged_models, delta < 1)

subset(dredged_models, delta < 2)

# Alternative way of showing result (didn't become any better)
# df <- subset(dredged_models, delta < 2)
# select(as.data.frame(df) %>% round(6), -`(Intercept)`, -logLik, -AICc)
```

```
## Global model call: lm(formula = as.formula(params$logistic_formula), data = df_analysis, 
##     na.action = "na.fail")
## ---
## Model selection table 
<<<<<<< HEAD
##      (Int) bar_spr     cnf  dcd_mxd lak_wtr mdn_toc      pre slp_dep_vs_tim   tmp TOT_dep
## 3570 17.84   1.130                   -2.650  0.8259 -0.04233          1.710 4.406  0.2781
## 7666 27.81   1.165                   -2.845  1.3100 -0.04486          2.210 5.822  0.2607
## 3314 15.27   1.080                   -2.606  1.8420 -0.03479          1.915        0.3040
## 3574 25.83   1.072 -0.1468           -2.735  1.1090 -0.04579          1.524 4.457  0.2741
## 7674 28.81   1.125         -0.10090  -2.810  1.4340 -0.04336          2.310 5.724  0.2622
## 3578 18.12   1.104         -0.06327  -2.621  0.8871 -0.04130          1.756 4.296  0.2796
##      mdn_toc:TOT_dep slp_dep_vs_tim:TOT_dep df    logLik   AICc delta weight
## 3570       -0.009344                        10 -2207.498 4435.6  0.00  0.256
## 7666       -0.010130             -0.0004647 11 -2206.480 4435.7  0.09  0.246
## 3314       -0.009891                         9 -2208.898 4436.3  0.69  0.182
## 3574       -0.009509                        11 -2207.169 4437.1  1.46  0.123
## 7674       -0.010240             -0.0004900 12 -2206.326 4437.5  1.91  0.099
## 3578       -0.009387                        11 -2207.436 4437.6  2.00  0.094
=======
##      (Int) bar_spr    cnf dcd_mxd lak_wtr mdn_toc      pre slp_dep_vs_tim   tmp ttl_shr_hrb
## 3837 135.7         -1.205  -1.244  -3.703  0.9352 -0.03459          2.143            -1.328
## 3838 170.0 -0.4683 -1.533  -1.568  -4.050  0.4713 -0.03443          2.093            -1.653
## 4093 137.3         -1.200  -1.222  -3.740  0.3057 -0.03867          2.004 2.366      -1.318
##      TOT_dep mdn_toc:TOT_dep df    logLik   AICc delta weight
## 3837  0.3049       -0.009546 11 -2087.192 4197.1  0.00  0.466
## 3838  0.3035       -0.009463 12 -2086.579 4198.1  0.92  0.295
## 4093  0.2899       -0.009214 12 -2086.784 4198.5  1.33  0.240
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## Models ranked by AICc(x)
```


<<<<<<< HEAD



=======
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
### Plots  

```r
# Pick model with lowest AICc
mod1 <- get.models(dredged_models, 1)[[1]]  

modelvars <- get_model_variables(mod1)

# Interactions: 2D plot 
if (length(modelvars$interaction_list) > 0){
  modelvars$interaction_list %>% purrr::walk(
    ~visreg(mod1, .x[1], by = .x[2], scale = "response")
  )
}
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-26-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```r
# Additive effects: 1D plot
if (length(modelvars$additive_vars) > 0){
  par(mfrow = c(2,3), mar = c(4,5,2,1), oma = c(0,0,2,0))
  for (var in modelvars$additive_vars)
    visreg(mod1, var, scale = "response")  
}
```

<<<<<<< HEAD
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-25-2.png)<!-- -->

```
## Conditions used in construction of plot
## lake_water: 12.10006
## median_toc: 5.705
## pre: 980.1
## slope_dep_vs_time: -13.20993
## tmp: 5.425001
## TOTN_dep: 525.5851
## Conditions used in construction of plot
## bare_sparse: 0
## median_toc: 5.705
## pre: 980.1
## slope_dep_vs_time: -13.20993
## tmp: 5.425001
## TOTN_dep: 525.5851
## Conditions used in construction of plot
## bare_sparse: 0
## lake_water: 12.10006
## median_toc: 5.705
## slope_dep_vs_time: -13.20993
## tmp: 5.425001
## TOTN_dep: 525.5851
## Conditions used in construction of plot
## bare_sparse: 0
## lake_water: 12.10006
## median_toc: 5.705
## pre: 980.1
## tmp: 5.425001
## TOTN_dep: 525.5851
## Conditions used in construction of plot
## bare_sparse: 0
## lake_water: 12.10006
## median_toc: 5.705
## pre: 980.1
## slope_dep_vs_time: -13.20993
## TOTN_dep: 525.5851
=======
![](164a1_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-26-2.png)<!-- -->

```
## Conditions used in construction of plot
## decid_mixed: 9.193245
## lake_water: 11.54497
## median_toc: 5.769355
## pre: 980.1
## slope_dep_vs_time: -13.27874
## total_shrub_herbaceous: 2.991775
## TOTN_dep: 522.26
## Conditions used in construction of plot
## coniferous: 16.90036
## lake_water: 11.54497
## median_toc: 5.769355
## pre: 980.1
## slope_dep_vs_time: -13.27874
## total_shrub_herbaceous: 2.991775
## TOTN_dep: 522.26
## Conditions used in construction of plot
## coniferous: 16.90036
## decid_mixed: 9.193245
## median_toc: 5.769355
## pre: 980.1
## slope_dep_vs_time: -13.27874
## total_shrub_herbaceous: 2.991775
## TOTN_dep: 522.26
## Conditions used in construction of plot
## coniferous: 16.90036
## decid_mixed: 9.193245
## lake_water: 11.54497
## median_toc: 5.769355
## slope_dep_vs_time: -13.27874
## total_shrub_herbaceous: 2.991775
## TOTN_dep: 522.26
## Conditions used in construction of plot
## coniferous: 16.90036
## decid_mixed: 9.193245
## lake_water: 11.54497
## median_toc: 5.769355
## pre: 980.1
## total_shrub_herbaceous: 2.991775
## TOTN_dep: 522.26
## Conditions used in construction of plot
## coniferous: 16.90036
## decid_mixed: 9.193245
## lake_water: 11.54497
## median_toc: 5.769355
## pre: 980.1
## slope_dep_vs_time: -13.27874
## TOTN_dep: 522.26
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```








