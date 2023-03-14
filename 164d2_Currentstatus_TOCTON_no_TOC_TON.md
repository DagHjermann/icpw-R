---
title: "164d2 Analyse TOC/TON medians 2012-2016 - without TOC and TON"
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
**Analysis of TOC/TON medians (2012-2016)**   

**Dataset: TOC/TON medians data set, but not incudig TOC and TIN medians**   
**Name of dataset: medians_2012-2016_toc_totn_no3.csv**   


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
## Medians results: 'medians_2012-2016_toc_totn_no3.csv' ,n = 310 
## 
## Number of values per variable: 
##   station_id NH4.N_µg.l.N NO3.N_µg.l.N   TOC_mg.C.l  TOTN_µg.l.N  TOTP_µg.l.P   TON_µg.l.N 
##          310          225          310          310          310          229          310 
##    TOTN.TOTP     NO3.TOTP      TOC.TON     TOC.TOTP 
##          229          229          310          229 
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
## df1, n = 310 
## df2, n = 498 
<<<<<<< HEAD
## dat_1, n = 310
```


=======
## dat_1_allrows, n = 310  (may include series where median_tocton = NA)
## dat_1, n = 310  (series where median_tocton has values)
```


```r
sum(is.na(dat_1$log_median_no3))
sum(is.na(dat_1$log_median_tocton))
```

```
## [1] 0
## [1] 0
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
## 8 stations with > 10% cultivated deleted
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
## Removed 15 rows (Deleted stations with > 5 % cultivated)
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
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
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
## 0 station removed - station PL05 (has dubious NO3 data)
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
## median_tocton,catchment_area,slope_dep_vs_time, TOTN_dep, latitude, longitude,pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
<<<<<<< HEAD
## Number of missing values per variable: 
##          median_tocton         catchment_area      slope_dep_vs_time               TOTN_dep 
##                      0                      0                      0                      0 
##               latitude              longitude                    pre                    tmp 
##                      0                      0                      0                      0 
##                  urban             cultivated           total_forest total_shrub_herbaceous 
##                      0                      0                      0                      0 
##                wetland             lake_water            bare_sparse 
##                      0                      0                      0 
## 
## Number of complete observations: 
## complete
## TRUE 
##  302 
## 
## 
## Number of complete observations by country: 
##                 complete
##                  TRUE
##   Canada           76
##   Czech Republic    2
##   Finland          23
##   Germany           1
##   Italy             4
##   Latvia            1
##   Norway           82
##   Sweden           91
##   United Kingdom   22
## 
## 
## Original data: n = 302 
## Analysis: n = 302
=======
## Removed 0 rows (station PL05 (has dubious NO3 data))
##                       Var Missing
## 1           median_tocton       0
## 2          catchment_area       0
## 3       slope_dep_vs_time       0
## 4                TOTN_dep       0
## 5                latitude       0
## 6               longitude       0
## 7                     pre       0
## 8                     tmp       0
## 9                   urban       0
## 10             cultivated       0
## 11           total_forest       0
## 12 total_shrub_herbaceous       0
## 13                wetland       0
## 14             lake_water       0
## 15            bare_sparse       0
## 
## Dataset after removing urban, cultivated, PL05 saved as '164_median_tocton_data.xlsx' 
## 
## Number of rows that will be excluded: 
## 
## FALSE 
##   288 
## 
## 
## Number of complete observations by country: 
##                 Row_excluded
## country          FALSE
##   Canada            70
##   Czech Republic     2
##   Finland           22
##   Germany            1
##   Italy              3
##   Latvia             1
##   Norway            80
##   Sweden            87
##   United Kingdom    22
## 
## 
## Data before removing PL05: n = 288 
## Data after removing PL05: n = 288 
## Data after removing missing predictors: n = 288
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

<<<<<<< HEAD
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
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
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```
## 
## Model formula:
## median_tocton ~ catchment_area + slope_dep_vs_time + TOTN_dep + 
##     pre + tmp + urban + cultivated + total_forest + total_shrub_herbaceous + 
##     wetland + lake_water + bare_sparse
## 
## Fitted party:
## [1] root
<<<<<<< HEAD
## |   [2] bare_sparse <= 0.924
## |   |   [3] lake_water <= 10.15983
## |   |   |   [4] slope_dep_vs_time <= -27.74892: 27.370 (n = 28, err = 1698.5)
## |   |   |   [5] slope_dep_vs_time > -27.74892: 34.984 (n = 73, err = 5429.9)
## |   |   [6] lake_water > 10.15983
## |   |   |   [7] lake_water <= 20.38524
## |   |   |   |   [8] total_shrub_herbaceous <= 2.761
## |   |   |   |   |   [9] TOTN_dep <= 564.26: 31.598 (n = 41, err = 1167.5)
## |   |   |   |   |   [10] TOTN_dep > 564.26: 24.227 (n = 28, err = 598.6)
## |   |   |   |   [11] total_shrub_herbaceous > 2.761: 21.492 (n = 19, err = 758.9)
## |   |   |   [12] lake_water > 20.38524: 21.397 (n = 63, err = 2758.2)
## |   [13] bare_sparse > 0.924
## |   |   [14] bare_sparse <= 27.834
## |   |   |   [15] slope_dep_vs_time <= -24.37738: 13.754 (n = 7, err = 366.5)
## |   |   |   [16] slope_dep_vs_time > -24.37738: 21.741 (n = 20, err = 834.8)
=======
## |   [2] bare_sparse <= 4
## |   |   [3] lake_water <= 16.33853
## |   |   |   [4] wetland <= 18.585
## |   |   |   |   [5] slope_dep_vs_time <= -14.7115: 29.150 (n = 85, err = 4319.0)
## |   |   |   |   [6] slope_dep_vs_time > -14.7115: 33.625 (n = 61, err = 4516.7)
## |   |   |   [7] wetland > 18.585: 39.567 (n = 8, err = 880.0)
## |   |   [8] lake_water > 16.33853
## |   |   |   [9] total_forest <= 36.339: 16.419 (n = 13, err = 316.0)
## |   |   |   [10] total_forest > 36.339
## |   |   |   |   [11] TOTN_dep <= 597.66: 27.317 (n = 42, err = 1961.8)
## |   |   |   |   [12] TOTN_dep > 597.66
## |   |   |   |   |   [13] pre <= 879.5: 22.198 (n = 22, err = 173.1)
## |   |   |   |   |   [14] pre > 879.5: 16.287 (n = 12, err = 74.7)
## |   [15] bare_sparse > 4
## |   |   [16] bare_sparse <= 27.834: 19.316 (n = 22, err = 969.9)
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5
## |   |   [17] bare_sparse > 27.834: 11.026 (n = 23, err = 601.4)
## 
## Number of inner nodes:    8
## Number of terminal nodes: 9
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = full_set)

plot(ev.raw)
```

<<<<<<< HEAD
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
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
##           Mean of squared residuals: 41.51112
##                     % Var explained: 57.96
=======
##           Mean of squared residuals: 41.67812
##                     % Var explained: 57.9
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
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 6)
```

<<<<<<< HEAD
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-21-2.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1             bare_sparse       1.646000        2368    26.041511            4298.2749
## 2            total_forest       1.678000        4823    33.762859            4207.9506
## 3              lake_water       1.500000        5350    30.087260            4958.5382
## 4  total_shrub_herbaceous       2.368000        4516    14.853417            2245.1853
## 5                 wetland       2.842000        4018     4.776066            1922.1728
## 6                     tmp       2.894000        4096     9.153480            1602.5592
## 7                TOTN_dep       2.396000        5400    17.489391            2947.5261
## 8                     pre       3.410000        3946     4.475620            1236.7698
## 9       slope_dep_vs_time       2.738000        4743     8.508654            1854.5646
## 10                  urban       4.825304        2466     3.411936             671.4102
## 11         catchment_area       3.398000        4389     1.293058            1202.3305
## 12             cultivated       5.551296        1368     1.279224             400.5628
##    no_of_trees times_a_root       p_value
## 1          500          169  1.000000e+00
## 2          500          149  1.860375e-44
## 3          500           88 4.974602e-108
## 4          500           41  4.961191e-20
## 5          500           22  1.572286e-01
## 6          500           14  1.097264e-02
## 7          500            7 2.058381e-115
## 8          500            5  5.743026e-01
## 9          500            4  3.631074e-37
## 10         499            1  1.000000e+00
## 11         500            0  8.622659e-13
## 12         476            0  1.000000e+00
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

![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->


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
##     (Int) bar_spr  ctc_are lak_wtr       pre    tmp  ttl_frs     urb df    logLik   AICc delta
## 126 37.09 -0.2627          -0.4382 -0.003935 0.4976 -0.03111 -0.5373  8 -1043.415 2103.3  0.00
## 62  37.10 -0.2623          -0.4523 -0.003569 0.3721 -0.03271          7 -1044.975 2104.3  1.01
## 94  34.20 -0.2538          -0.4087 -0.002658 0.4599          -0.5723  7 -1045.396 2105.2  1.85
## 128 36.94 -0.2624 0.001328 -0.4349 -0.003909 0.5032 -0.03074 -0.5525  9 -1043.299 2105.2  1.89
## 46  38.13 -0.2849          -0.4582 -0.002647        -0.02973          6 -1046.497 2105.3  1.96
##     weight
## 126  0.362
## 62   0.218
## 94   0.143
## 128  0.140
## 46   0.136
=======
##     (Int) bar_spr  ctc_are lak_wtr       pre    tmp ttl_frs     urb df   logLik   AICc delta
## 126 28.93 -0.1885          -0.4028 -0.001846 0.4880 0.07800 -1.1420  8 -983.701 1983.9  0.00
## 118 27.31 -0.2017          -0.3965           0.2808 0.08621 -0.9183  7 -985.224 1984.8  0.93
## 102 28.94 -0.2127          -0.4071                  0.08534 -0.7935  6 -986.346 1985.0  1.07
## 128 28.86 -0.1884 0.001132 -0.4006 -0.001853 0.4951 0.07784 -1.1700  9 -983.612 1985.9  1.95
##     weight
## 126  0.386
## 118  0.243
## 102  0.226
## 128  0.145
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

# Additive effects: 1D plot
if (length(modelvars$additive_vars) > 0){
  par(mfrow = c(2,3), mar = c(4,5,2,1), oma = c(0,0,2,0))
  for (var in modelvars$additive_vars)
    visreg(mod1, var, scale = "response")  
}
```

<<<<<<< HEAD
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-25-1.png)<!-- -->
=======
![](164d2_Currentstatus_TOCTON_no_TOC_TON_files/figure-html/unnamed-chunk-26-1.png)<!-- -->
>>>>>>> e5a7972e37222771881a7ff3ac9997f6a8a047b5








