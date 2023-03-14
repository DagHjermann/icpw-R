---
title: "164ab1x Analyse NO3 medians 2012-2016 - incl. TOC trend"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
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
  pairwise_plots_same_scale:
    value: 'FALSE'
  logistic_formula: 
    value: 'median_no3 ~ TOTN_dep + slope_dep_vs_time + TOTN_dep:slope_dep_vs_time + median_toc + TOTN_dep:median_toc + tmp + pre + decid_mixed + bare_sparse + coniferous + catchment_area + lake_water + total_shrub_herbaceous'

---




**Analysis of NO3 medians (2012-2016)**   

**Dataset: NO3 medians data set incl. TOC + TOC trend**   
**Name of dataset: medians_2012-2016_no3.csv**   


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
* The data part (part 2) is quite similar in scripts 160 - 165 

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
         slope_toc_vs_time = `TOC_mg C/l`)

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
## df1, n = 494 
## df2, n = 498 
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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'slope_toc_vs_time', 'trend_TOTN'
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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'slope_toc_vs_time', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'slope_toc_vs_time', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
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
## 'station_id', 'median_no3', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'slope_toc_vs_time', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'slope_pre', 'slope_tmp'
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
## Removed 40 rows (Deleted stations with > 5 % cultivated)
## Removed 6 rows (Deleted stations with > 5 % urban)
```



### Data set used  

```r
dat <- dat_5
```


## 3. Plot data      


```r
gg <- ggplot(dat, aes(TOTN_dep, median_no3)) + 
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
  
gg
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


## 4. Select data   

### a. Selection of variables    
* Select variables to use, and thereby also cases  
* Also remove PL05, which has dubious values   

```r
add_flag_variable <- function(data, variable_string){
  variable_string <- gsub(" ", "", variable_string)
  variables <- strsplit(variable_string, split = ",")[[1]]
  # Check if all variables are there
  found <- variables %in% names(data)
  if (sum(!found) > 0)
    stop("Not all variables found in data:", 
      paste(variables[!found], collapse = " ,"), 
      "\n")
  # Data for analyses
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
  data[variables]
}

cat("-------------------------------------------------------------\n")
cat("Variables: \n")
cat(params$selected_vars)
cat("\n-------------------------------------------------------------\n")

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
```

```
## -------------------------------------------------------------
## Variables: 
## median_no3,median_toc, slope_toc_vs_time,slope_dep_vs_time, TOTN_dep, latitude, longitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
## Removed 1 rows (station PL05 (has dubious NO3 data))
##                       Var Missing
## 1              median_no3       0
## 2              median_toc       3
## 3       slope_toc_vs_time      34
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
##   409    37 
## 
## 
## Number of complete observations by country: 
##                 Row_excluded
## country          FALSE TRUE
##   Canada           103    4
##   Czech Republic     7    1
##   Finland           22    0
##   Germany            3    0
##   Ireland            0   10
##   Italy              0    3
##   Latvia             0    1
##   Netherlands        2    1
##   Norway            80    0
##   Poland             5    2
##   Slovakia          12    0
##   Sweden            81    6
##   Switzerland        0    8
##   United Kingdom    21    1
##   United States     73    0
## 
## 
## Data before removing PL05: n = 447 
## Data after removing PL05: n = 446 
## Data after removing missing predictors: n = 409
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

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
# train_set$X <- 10^train_set$median_no3
# (ct = ctree(X ~ ., data = train_set))

(ct = ctree(as.formula(params$tree_formula), 
            data = full_set))

plot(ct, main="Conditional Inference Tree")
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```
## 
## Model formula:
## median_no3 ~ median_toc + slope_toc_vs_time + slope_dep_vs_time + 
##     TOTN_dep + pre + tmp + urban + cultivated + coniferous + 
##     decid_mixed + total_shrub_herbaceous + wetland + lake_water + 
##     bare_sparse
## 
## Fitted party:
## [1] root
## |   [2] TOTN_dep <= 743.05
## |   |   [3] slope_dep_vs_time <= -16.79619
## |   |   |   [4] bare_sparse <= 0.071: 39.582 (n = 46, err = 27751.6)
## |   |   |   [5] bare_sparse > 0.071: 60.000 (n = 10, err = 33476.0)
## |   |   [6] slope_dep_vs_time > -16.79619
## |   |   |   [7] slope_dep_vs_time <= -10.44623
## |   |   |   |   [8] bare_sparse <= 0: 12.272 (n = 34, err = 5482.2)
## |   |   |   |   [9] bare_sparse > 0: 46.050 (n = 20, err = 11248.5)
## |   |   |   [10] slope_dep_vs_time > -10.44623
## |   |   |   |   [11] tmp <= 1.60833: 7.383 (n = 30, err = 1452.8)
## |   |   |   |   [12] tmp > 1.60833: 16.307 (n = 112, err = 9951.0)
## |   [13] TOTN_dep > 743.05
## |   |   [14] slope_toc_vs_time <= 4.53
## |   |   |   [15] coniferous <= 72.492
## |   |   |   |   [16] bare_sparse <= 72.675: 153.357 (n = 71, err = 923712.7)
## |   |   |   |   [17] bare_sparse > 72.675: 313.677 (n = 7, err = 40500.4)
## |   |   |   [18] coniferous > 72.492: 532.020 (n = 7, err = 522107.7)
## |   |   [19] slope_toc_vs_time > 4.53: 88.478 (n = 72, err = 696147.6)
## 
## Number of inner nodes:     9
## Number of terminal nodes: 10
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = full_set)

plot(ev.raw)
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


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
##           Mean of squared residuals: 6653.54
##                     % Var explained: 48.73
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

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 6)
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1                TOTN_dep       1.334000        6757   7854.94581           1309846.07
## 2       slope_dep_vs_time       1.984000        7016   2945.04446            620745.74
## 3                     tmp       2.716000        5428   1324.55909            499172.42
## 4              coniferous       2.706000        5157    561.26192            543002.32
## 5              lake_water       3.266000        5697    387.98129            313413.81
## 6             bare_sparse       3.991496        2260    314.34219            145873.74
## 7             decid_mixed       4.166000        4962    270.02974            156158.75
## 8                 wetland       3.976000        4884    449.07099            179517.63
## 9  total_shrub_herbaceous       4.153496        4502    158.52252            191117.69
## 10      slope_toc_vs_time       2.732000        5747   2295.74781            451535.24
## 11             median_toc       3.136000        5618   1273.19326            325987.22
## 12             cultivated       5.576440        1550     22.02838             16036.02
## 13                    pre       3.630000        5442   1232.70352            234862.50
## 14                  urban       5.255464        2090    149.82079             28506.47
##    no_of_trees times_a_root       p_value
## 1          500          152 8.123000e-171
## 2          500           89 1.897123e-215
## 3          500           89  5.780337e-21
## 4          500           64  3.713481e-08
## 5          500           34  6.349120e-40
## 6          499           20  1.000000e+00
## 7          500           19  6.109015e-03
## 8          500           17  8.912183e-02
## 9          499            8  9.999950e-01
## 10         500            6  4.086108e-44
## 11         500            2  1.006117e-33
## 12         485            0  1.000000e+00
## 13         500            0  8.448998e-22
## 14         491            0  1.000000e+00
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

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->




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
##       (Int) bar_spr     cnf  dcd_mxd lak_wtr mdn_toc      pre slp_dep_vs_tim slp_toc_vs_tim
## 3324  31.61  0.9722  0.2915           -2.366   15.55 -0.04086          2.209         -17.84
## 7420  27.41  0.9590  0.2693           -2.238   15.34 -0.04187          1.755         -18.70
## 7676  22.44  0.9202  0.2640           -2.158   16.44 -0.03560          1.682         -19.13
## 7418  36.93  0.8818                   -2.309   15.45 -0.04737          1.399         -18.04
## 3322  42.55  0.8895                   -2.460   15.70 -0.04675          1.880         -17.00
## 7674  31.64  0.8435                   -2.226   16.58 -0.04082          1.332         -18.49
## 3326  48.92  0.7571         -0.22120  -2.482   15.89 -0.04456          2.078         -17.35
## 7422  43.25  0.7555         -0.21160  -2.336   15.64 -0.04525          1.609         -18.34
## 7678  37.97  0.7151         -0.21430  -2.252   16.78 -0.03859          1.543         -18.79
## 3838  61.23  0.6650         -0.34630  -2.665   15.76 -0.03994          2.282         -18.23
## 3580  29.24  0.9498  0.2924           -2.340   16.30 -0.03662          2.253         -17.94
## 3839 122.90         -0.6340 -0.98410  -3.301   16.15 -0.03963          2.320         -18.96
## 7934  54.88  0.6722         -0.32570  -2.515   15.54 -0.04101          1.833         -19.05
## 7935 117.20         -0.6408 -0.97040  -3.157   15.94 -0.04069          1.872         -19.79
## 3328  36.32  0.9038  0.2318 -0.08594  -2.394   15.65 -0.04122          2.219         -17.80
## 8190  48.94  0.6417         -0.31830  -2.420   16.62 -0.03515          1.752         -19.41
##         tmp ttl_shr_hrb TOT_dep mdn_toc:TOT_dep slp_dep_vs_tim:TOT_dep df    logLik   AICc
## 3324                     0.2867       -0.008898                        11 -2378.884 4780.4
## 7420                     0.2929       -0.007480              0.0004749 12 -2377.986 4780.8
## 7676 -4.073              0.3172       -0.007710              0.0006219 13 -2377.046 4781.0
## 7418                     0.2894       -0.007760              0.0005308 11 -2379.269 4781.2
## 3322                     0.2821       -0.009386                        10 -2380.394 4781.3
## 7674 -4.183              0.3145       -0.007991              0.0006806 12 -2378.284 4781.4
## 3326                     0.2870       -0.009409                        11 -2379.360 4781.4
## 7422                     0.2939       -0.007847              0.0005096 12 -2378.320 4781.4
## 7678 -4.235              0.3193       -0.008081              0.0006609 13 -2377.306 4781.5
## 3838            -0.3062  0.2881       -0.008861                        12 -2378.413 4781.6
## 3580 -2.623              0.3011       -0.009329                        12 -2378.460 4781.7
## 3839            -0.9234  0.2882       -0.008791                        12 -2378.490 4781.8
## 7934            -0.2775  0.2943       -0.007482              0.0004665 13 -2377.547 4782.0
## 7935            -0.9013  0.2943       -0.007414              0.0004658 13 -2377.626 4782.2
## 3328                     0.2877       -0.009007                        12 -2378.790 4782.4
## 8190 -3.960     -0.2534  0.3180       -0.007733              0.0006118 14 -2376.662 4782.4
##      delta weight
## 3324  0.00  0.105
## 7420  0.33  0.089
## 7676  0.58  0.078
## 7418  0.77  0.071
## 3322  0.91  0.066
## 7674  0.92  0.066
## 3326  0.95  0.065
## 7422  1.00  0.063
## 7678  1.10  0.060
## 3838  1.18  0.058
## 3580  1.28  0.055
## 3839  1.34  0.054
## 7934  1.58  0.047
## 7935  1.74  0.044
## 3328  1.94  0.040
## 8190  1.96  0.039
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
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
# Additive effects: 1D plot
if (length(modelvars$additive_vars) > 0){
  par(mfrow = c(2,3), mar = c(4,5,2,1), oma = c(0,0,2,0))
  for (var in modelvars$additive_vars)
    visreg(mod1, var, scale = "response")  
}
```

![](164ab1x_Currentstatus_NO3_allvars_files/figure-html/unnamed-chunk-26-2.png)<!-- -->

```
## Conditions used in construction of plot
## coniferous: 18.099
## lake_water: 12.272
## median_toc: 5.35
## pre: 1105
## slope_dep_vs_time: -15.41778
## slope_toc_vs_time: 4.928
## TOTN_dep: 597.4969
## Conditions used in construction of plot
## bare_sparse: 0
## lake_water: 12.272
## median_toc: 5.35
## pre: 1105
## slope_dep_vs_time: -15.41778
## slope_toc_vs_time: 4.928
## TOTN_dep: 597.4969
## Conditions used in construction of plot
## bare_sparse: 0
## coniferous: 18.099
## median_toc: 5.35
## pre: 1105
## slope_dep_vs_time: -15.41778
## slope_toc_vs_time: 4.928
## TOTN_dep: 597.4969
## Conditions used in construction of plot
## bare_sparse: 0
## coniferous: 18.099
## lake_water: 12.272
## median_toc: 5.35
## slope_dep_vs_time: -15.41778
## slope_toc_vs_time: 4.928
## TOTN_dep: 597.4969
## Conditions used in construction of plot
## bare_sparse: 0
## coniferous: 18.099
## lake_water: 12.272
## median_toc: 5.35
## pre: 1105
## slope_toc_vs_time: 4.928
## TOTN_dep: 597.4969
## Conditions used in construction of plot
## bare_sparse: 0
## coniferous: 18.099
## lake_water: 12.272
## median_toc: 5.35
## pre: 1105
## slope_dep_vs_time: -15.41778
## TOTN_dep: 597.4969
```








