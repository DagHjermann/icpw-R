---
title: "162c1 Analyse TOC/TON medians 2012-2016 - all variables"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
    value: '162x Analyse NO3 status - test run'
  text_line1: 
    value: 'Analysis of NO3 medians (2012-2016)'
  text_line2: 
    value: 'Data with NO3, TOTN_dep, slope_dep_vs_time, TOC, tmp (temperature), pre (precipitation)'
  medians_filename:
    value: 'medians_2012-2016_no3.csv'        
  selected_vars: 
    value: 'log_median_no3,catchment_area, log_median_toc,slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse'
  tree_formula:
    value: 'log_median_no3 ~ .'
  extra_pairwise_plots:
    value: 'TOC,NO3; slope_dep_vs_time,TOTN_dep; altitude,decid_mixed'
  pairwise_plots_same_scale:
    value: 'FALSE'
  logistic_formula: 
    value: 'log_median_no3 ~ TOTN_dep + slope_dep_vs_time + TOTN_dep:slope_dep_vs_time + log_median_toc + TOTN_dep:log_median_toc + tmp + pre + altitude + decid_mixed + bare_sparse + coniferous + catchment_area + lake_water + total_shrub_herbaceous'

---




**Analysis of TOC/TON medians (2012-2016)**   

**Dataset: TOC/TON medians data set**   
**Name of dataset: medians_2012-2016_toc_totn_no3.csv**   


* Response variable: 'Current NO3 level' (locations with signif. *increase* are *not* excluded)  
* Data from https://github.com/JamesSample/icpw2/tree/master/thematic_report_2020/results      
* Sen slope of NO3, TON, TOC/TON etc. 1992-2016
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
library(stringr)    # str_extract

source("002_Functions.R")
source("160parm_functions.R")

knitr::opts_chunk$set(results = 'hold') # collect the results from a chunk  
knitr::opts_chunk$set(warning = FALSE)  

options(width = 95)
```



## 2. Data  
* The data part (part 2) is quite similar in scripts 160, 161 and 162 

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
  select(station_id, `NO3.N_µg.l.N`, `TON_µg.l.N`, `TON_µg.l.N`, `TOC_mg.C.l`, TOC.TON) %>%
  rename(median_no3 = `NO3.N_µg.l.N`,
         median_ton = `TON_µg.l.N`,
         median_ton = `TON_µg.l.N`,
         median_toc = `TOC_mg.C.l`,
         median_tocton = `TOC.TON`) %>%
  mutate(log_median_no3 = log10(median_no3 + 0.1),
         log_median_ton = log10(median_ton),
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

dat_1_allrows <- df1 %>%
  left_join(df2, by = "station_id")

response_var <- str_extract(params$tree_formula, "[^[[:blank:]]]+")

cat("dat_1_allrows, n =", nrow(dat_1_allrows), 
    " (may include series where", response_var,  "= NA)\n")

sel <- !is.na(dat_1_allrows[[response_var]])

dat_1 <- dat_1_allrows[sel,]

cat("dat_1, n =", nrow(dat_1), 
    " (series where", response_var,  "has values)\n")
```

```
## 
## df1, n = 310 
## df2, n = 498 
## dat_1_allrows, n = 310  (may include series where log_median_tocton = NA)
## dat_1, n = 310  (series where log_median_tocton has values)
```


```r
sum(is.na(dat_1$log_median_no3))
sum(is.na(dat_1$log_median_tocton))
```

```
## [1] 0
## [1] 0
```

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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN'
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
## ── Column specification ───────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
cat("\n")
# names(df_climate_mean)

df_climate_slope <- read_csv(fn) %>%
  select(station_id, variable, sen_slp) %>%
  pivot_wider(names_from = "variable", values_from = "sen_slp", names_prefix = "slope_")
```

```
## Rows: 1112 Columns: 8
## ── Column specification ───────────────────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'slope_pre', 'slope_tmp'
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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'slope_pre', 'slope_tmp'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'station_code', 'station_name', 'latitude', 'longitude', 'altitude', 'continent', 'country', 'region', 'group', 'catchment_area', 'urban', 'cultivated', 'total_forest', 'coniferous', 'total_shrub_herbaceous', 'grasslands', 'heathlands', 'transitional_woodland_shrub', 'wetland', 'other', 'bare_sparse', 'decid_mixed', 'lake_water'
```



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



### Data set used  

```r
dat <- dat_5
```


## 3. Plot data      


```r
gg <- ggplot(dat, aes(TOTN_dep, log_median_no3)) + 
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
  
gg
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


## 4. Select data   

### a. Selection of variables    
* Select variables to use, and thereby also cases  
* Also remove PL05, which has dubious values   

```r
# Variables that will be included in excel output (removed afterwards)
response_var_unlogged <- sub("log_", "", response_var)
vars_for_excel <- c(response_var_unlogged, 
                    "station_id", "station_code", "station_name", 
                   "country", "region", "continent")

if (response_var == "log_median_tocton")
  vars_for_excel <- c(vars_for_excel, "median_ton", "median_toc")

get_data_for_analysis <- function(data, variable_string){
  variable_string <- gsub(" ", "", variable_string)
  variables <- strsplit(variable_string, split = ",")[[1]]
  # Check if all variables are there
  found <- variables %in% names(data)
  if (sum(!found) > 0)
    stop("Not all variables found in data:", 
      paste(variables[!found], collapse = " ,"), 
      "\n")
  # Data for analyses
  data[c(vars_for_excel, variables)]
}

cat("-------------------------------------------------------------\n")
cat("Variables: \n")
cat(params$selected_vars)
cat("\n-------------------------------------------------------------\n")

dat <- dat %>%
  filter2(!station_code %in% "PL05", text = "station PL05 (has dubious NO3 data)")

# debugonce(get_data_for_analysis)
# df_analysis <- get_data_for_analysis(dat, vars)  
df_analysis_allrows <- get_data_for_analysis(dat, params$selected_vars)  

# Save to excel
fn <- paste0(substr(params$document_title, 1, 3), "_", response_var, "_data.xlsx")
writexl::write_xlsx(df_analysis_allrows, paste0("Data_analysed/", fn))
cat("\nDataset after removing urban, cultivated, PL05 saved as", sQuote(fn), "\n\n")

# names(dat) %>% paste(collapse = ", ")

cat("Number of missing values per variable: \n")
apply(is.na(df_analysis_allrows), 2, sum) 
cat("\n")

# What is missing? (long output)
if (FALSE){
  dat %>% 
    split(.$country) %>%
    purrr::map(~apply(is.na(.), 2, mean))
}

cat("Number of complete observations: \n")
complete <- complete.cases(df_analysis_allrows)
table(complete)

cat("\n\n")
cat("Number of complete observations by country: \n")
table(dat$country, complete)

# Keep only complete cases
df_analysis <- df_analysis_allrows[complete.cases(df_analysis_allrows),]

# Save to excel
fn <- paste0(
  stringr::str_extract(params$document_title, "[^[[:blank:]]]+"),
  "_data.xlsx")
writexl::write_xlsx(df_analysis, paste0("Data_analysed/", fn))

# Remove variables defined as 'vars_for_excel' in function above
sel <- names(df_analysis) %in% vars_for_excel
df_analysis <- df_analysis[!sel]

cat("\n\n")
cat("Data before removing PL05: n =", nrow(dat_5), "\n")
cat("Data after removing PL05: n =", nrow(df_analysis_allrows), "\n")
cat("Data after removing missing predictors: n =", nrow(df_analysis), "\n")
```

```
## -------------------------------------------------------------
## Variables: 
## log_median_tocton,catchment_area, log_median_ton, log_median_toc,slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
## Removed 0 rows (station PL05 (has dubious NO3 data))
## 
## Dataset after removing urban, cultivated, PL05 saved as '162_log_median_tocton_data.xlsx' 
## 
## Number of missing values per variable: 
##          median_tocton             station_id           station_code           station_name 
##                      0                      0                      0                      0 
##                country                 region              continent             median_ton 
##                      0                      0                      0                      0 
##             median_toc      log_median_tocton         catchment_area         log_median_ton 
##                      0                      0                      0                      0 
##         log_median_toc      slope_dep_vs_time               TOTN_dep               latitude 
##                      0                      0                      0                      0 
##              longitude               altitude                    pre                    tmp 
##                      0                      0                      0                      0 
##                  urban             cultivated             coniferous            decid_mixed 
##                      0                      0                      3                      3 
## total_shrub_herbaceous                wetland             lake_water            bare_sparse 
##                      0                      0                      0                      0 
## 
## Number of complete observations: 
## complete
## FALSE  TRUE 
##     3   285 
## 
## 
## Number of complete observations by country: 
##                 complete
##                  FALSE TRUE
##   Canada             3   67
##   Czech Republic     0    2
##   Finland            0   22
##   Germany            0    1
##   Italy              0    3
##   Latvia             0    1
##   Norway             0   80
##   Sweden             0   87
##   United Kingdom     0   22
## 
## 
## Data before removing PL05: n = 288 
## Data after removing PL05: n = 288 
## Data after removing missing predictors: n = 285
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

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
# train_set$X <- 10^train_set$log_median_no3
# (ct = ctree(X ~ ., data = train_set))

(ct = ctree(as.formula(params$tree_formula), 
            data = full_set))

plot(ct, main="Conditional Inference Tree")
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```
## 
## Model formula:
## log_median_tocton ~ catchment_area + log_median_ton + log_median_toc + 
##     slope_dep_vs_time + TOTN_dep + altitude + pre + tmp + urban + 
##     cultivated + coniferous + decid_mixed + total_shrub_herbaceous + 
##     wetland + lake_water + bare_sparse
## 
## Fitted party:
## [1] root
## |   [2] log_median_toc <= 0.20412
## |   |   [3] altitude <= 494: 1.102 (n = 19, err = 0.4)
## |   |   [4] altitude > 494: 0.870 (n = 16, err = 0.4)
## |   [5] log_median_toc > 0.20412
## |   |   [6] log_median_toc <= 0.87795
## |   |   |   [7] log_median_toc <= 0.34242: 1.249 (n = 18, err = 0.2)
## |   |   |   [8] log_median_toc > 0.34242
## |   |   |   |   [9] log_median_ton <= 2.04021: 1.506 (n = 9, err = 0.1)
## |   |   |   |   [10] log_median_ton > 2.04021
## |   |   |   |   |   [11] log_median_toc <= 0.71447
## |   |   |   |   |   |   [12] log_median_ton <= 2.17898: 1.406 (n = 15, err = 0.1)
## |   |   |   |   |   |   [13] log_median_ton > 2.17898
## |   |   |   |   |   |   |   [14] log_median_toc <= 0.60477: 1.255 (n = 21, err = 0.1)
## |   |   |   |   |   |   |   [15] log_median_toc > 0.60477: 1.335 (n = 17, err = 0.0)
## |   |   |   |   |   [16] log_median_toc > 0.71447
## |   |   |   |   |   |   [17] log_median_ton <= 2.39116
## |   |   |   |   |   |   |   [18] log_median_ton <= 2.26986: 1.520 (n = 7, err = 0.0)
## |   |   |   |   |   |   |   [19] log_median_ton > 2.26986: 1.467 (n = 17, err = 0.0)
## |   |   |   |   |   |   [20] log_median_ton > 2.39116
## |   |   |   |   |   |   |   [21] log_median_toc <= 0.80618: 1.339 (n = 15, err = 0.0)
## |   |   |   |   |   |   |   [22] log_median_toc > 0.80618: 1.397 (n = 16, err = 0.0)
## |   |   [23] log_median_toc > 0.87795
## |   |   |   [24] urban <= 2.10865
## |   |   |   |   [25] log_median_toc <= 1.05308
## |   |   |   |   |   [26] log_median_ton <= 2.5172
## |   |   |   |   |   |   [27] log_median_toc <= 0.94448: 1.512 (n = 13, err = 0.0)
## |   |   |   |   |   |   [28] log_median_toc > 0.94448: 1.569 (n = 26, err = 0.1)
## |   |   |   |   |   [29] log_median_ton > 2.5172: 1.437 (n = 13, err = 0.0)
## |   |   |   |   [30] log_median_toc > 1.05308: 1.586 (n = 37, err = 0.2)
## |   |   |   [31] urban > 2.10865
## |   |   |   |   [32] coniferous <= 60.122: 1.342 (n = 8, err = 0.0)
## |   |   |   |   [33] coniferous > 60.122: 1.457 (n = 18, err = 0.1)
## 
## Number of inner nodes:    16
## Number of terminal nodes: 17
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = full_set)

plot(ev.raw)
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


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
##           Mean of squared residuals: 0.007829451
##                     % Var explained: 80.48
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

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 6)
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1          log_median_toc       1.152000        5428 0.0365326288           4.24017596
## 2             bare_sparse       3.577356        1418 0.0034915638           1.09741307
## 3          log_median_ton       2.572000        3711 0.0048694671           1.05535049
## 4              coniferous       3.345828        2745 0.0039557769           0.82168450
## 5                altitude       2.968000        3433 0.0017258920           0.73922443
## 6  total_shrub_herbaceous       3.354000        2899 0.0024716897           0.39114646
## 7              lake_water       2.541828        3907 0.0029603738           0.75239673
## 8                 wetland       4.301656        2674 0.0002939778           0.19201765
## 9             decid_mixed       4.085828        2856 0.0009597183           0.25587826
## 10               TOTN_dep       2.866000        3502 0.0025267985           0.51819044
## 11                    pre       4.287828        2658 0.0007100785           0.18541066
## 12      slope_dep_vs_time       3.149828        3391 0.0020925378           0.43480601
## 13                    tmp       4.063828        2694 0.0009091902           0.24197607
## 14                  urban       5.386624        1807 0.0006007350           0.10872683
## 15         catchment_area       4.039828        3089 0.0002217692           0.19544006
## 16             cultivated       5.810604        1069 0.0002125682           0.08774258
##    no_of_trees times_a_root      p_value
## 1          500          170 0.000000e+00
## 2          473           89 1.000000e+00
## 3          500           75 7.427363e-44
## 4          499           69 9.999737e-01
## 5          500           43 4.003312e-19
## 6          500           13 8.588298e-01
## 7          499           12 3.382140e-67
## 8          498            9 1.000000e+00
## 9          499            7 9.712085e-01
## 10         500            5 2.568600e-24
## 11         499            3 1.000000e+00
## 12         499            2 2.818164e-16
## 13         499            2 9.999998e-01
## 14         492            1 1.000000e+00
## 15         499            0 5.860185e-03
## 16         457            0 1.000000e+00
```



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
## * `` -> ...1
## * `` -> ...2
## * `` -> ...3
## * `` -> ...4
## * `` -> ...5
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
  
  # Save gg object for later plotting / changes
  # Saved in Figures/Partial_plots' with name e.g. "gg_164a1_7.rds" for plot number 7
  fn <- paste0(
    "Figures/Partial_plots/gg_",
    stringr::str_extract(params$document_title, "([^[[:blank:]]]+)"),   # extract e.g. "164a1"
    "_", i, ".rds")
  saveRDS(gg, fn)
  
}
```

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-8.png)<!-- -->![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/5c3_plot_partial_effects2-9.png)<!-- -->




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
# saveRDS(dredged_models, "Data/162_all_dredged_models.rds")    # save it as it takes a couple of minutes
# dredged_models <- readRDS("Data/162_all_dredged_models.rds")

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
##      (Int)        alt    bar_spr   ctc_are       cnf    dcd_mxd    lak_wtr log_mdn_toc
## 979  2.857            -0.0004621                     -0.0001390                 0.9717
## 963  2.862            -0.0003956                                                0.9727
## 1987 2.849            -0.0003773                                                0.9711
## 2003 2.847            -0.0004390                     -0.0001232                 0.9705
## 3027 2.829            -0.0003507                     -0.0001458                 1.0110
## 980  2.855 -8.351e-06 -0.0004038                     -0.0001484                 0.9695
## 3009 2.815                                                                      1.0470
## 995  2.861            -0.0004360                                -0.0002018      0.9679
## 3011 2.838            -0.0002965                                                1.0070
## 983  2.856            -0.0004676 7.884e-06           -0.0001453                 0.9711
## 971  2.861            -0.0004032           6.259e-05                            0.9709
## 3025 2.805                                           -0.0001222                 1.0570
## 1988 2.846 -9.231e-06 -0.0003056                                                0.9685
## 1011 2.857            -0.0004828                     -0.0001291 -0.0001272      0.9688
## 2004 2.843 -1.070e-05 -0.0003606                     -0.0001327                 0.9674
## 964  2.861 -6.312e-06 -0.0003481                                                0.9711
## 4033 2.807                                                                      1.0400
## 2019 2.848            -0.0004168                                -0.0001964      0.9664
##      log_mdn_ton       pre       tmp       urb log_mdn_toc:log_mdn_ton df  logLik    AICc
## 979      -0.9269 1.544e-05 -0.003600                                    8 550.316 -1084.1
## 963      -0.9298 1.290e-05 -0.003426                                    7 549.164 -1083.9
## 1987     -0.9228 1.134e-05 -0.003115 -0.003063                          8 550.090 -1083.7
## 2003     -0.9213 1.383e-05 -0.003315 -0.002614                          9 550.980 -1083.3
## 3027     -0.9140 1.485e-05 -0.003485                          -0.01713  9 550.748 -1082.8
## 980      -0.9246 1.606e-05 -0.003749                                    9 550.650 -1082.6
## 3009     -0.9110 1.083e-05 -0.003046                          -0.02962  7 548.524 -1082.6
## 995      -0.9263 1.226e-05 -0.003402                                    8 549.492 -1082.5
## 3011     -0.9187 1.228e-05 -0.003319                          -0.01481  8 549.486 -1082.5
## 983      -0.9262 1.566e-05 -0.003593                                    9 550.510 -1082.4
## 971      -0.9304 1.446e-05 -0.003506                                    8 549.434 -1082.3
## 3025     -0.9059 1.277e-05 -0.003144                          -0.03383  8 549.426 -1082.3
## 1988     -0.9195 1.164e-05 -0.003228 -0.003447                          9 550.489 -1082.3
## 1011     -0.9249 1.486e-05 -0.003573                                    9 550.441 -1082.2
## 2004     -0.9174 1.436e-05 -0.003461 -0.003024                         10 551.514 -1082.2
## 964      -0.9281 1.324e-05 -0.003530                                    8 549.356 -1082.2
## 4033     -0.9063 9.466e-06 -0.002770 -0.002910                -0.02707  8 549.346 -1082.2
## 2019     -0.9194 1.074e-05 -0.003095 -0.003035                          9 550.402 -1082.2
##      delta weight
## 979   0.00  0.107
## 963   0.19  0.098
## 1987  0.45  0.086
## 2003  0.80  0.072
## 3027  1.27  0.057
## 980   1.46  0.052
## 3009  1.47  0.052
## 995   1.65  0.047
## 3011  1.66  0.047
## 983   1.74  0.045
## 971   1.76  0.044
## 3025  1.78  0.044
## 1988  1.79  0.044
## 1011  1.88  0.042
## 2004  1.88  0.042
## 964   1.92  0.041
## 4033  1.94  0.041
## 2019  1.96  0.040
## Models ranked by AICc(x)
```


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

![](162c1_Currentstatus_TOCTON_allvars_files/figure-html/unnamed-chunk-26-1.png)<!-- -->








