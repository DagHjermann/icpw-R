---
title: "164b1 Analyse NO3 medians 2012-2016 - without TOC + catchment area"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
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
  pairwise_plots_same_scale:
    value: 'FALSE'
  logistic_formula: 
    value: 'median_no3 ~ TOTN_dep + slope_dep_vs_time + TOTN_dep:slope_dep_vs_time + median_toc + TOTN_dep:median_toc + tmp + pre + decid_mixed + bare_sparse + coniferous + catchment_area + lake_water + total_shrub_herbaceous'

---





**Analysis of NO3 medians (2012-2016)**   

**Dataset: NO3 medians data set excl. TOC and catchment_area**   
**Name of dataset: medians_2012-2016_no3.csv**   


* Response variable: 'Current NO3 level' (locations with signif. *increase* are *not* excluded)  
* Data from https://github.com/JamesSample/icpw2/tree/master/thematic_report_2020/results      
* Sen slope of NO3, TOTN, TOC/TON etc. 1992-2016
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

source("002_Functions.R")
source("160parm_functions.R")

knitr::opts_chunk$set(results = 'hold') # collect the results from a chunk  
knitr::opts_chunk$set(warning = FALSE)  

options(width = 95)
```



## 2. Data  
* The data part (part 2) is quite similar in scripts 160-164 

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
  select(station_id, `NO3.N_µg.l.N`, `TOTN_µg.l.N`, `TON_µg.l.N`, `TOC_mg.C.l`, TOC.TON) %>%
  rename(median_no3 = `NO3.N_µg.l.N`,
         median_totn = `TOTN_µg.l.N`,
         median_ton = `TON_µg.l.N`,
         median_toc = `TOC_mg.C.l`,
         median_tocton = `TOC.TON`) %>%
  mutate(log_median_no3 = log10(median_no3 + 0.1),
         log_median_totn = log10(median_totn),
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

dat_1 <- df1 %>%
  left_join(df2, by = "station_id")

cat("dat_1, n =", nrow(dat_1), "\n")
```

```
## 
## df1, n = 494 
## df2, n = 498 
## dat_1, n = 494
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
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN'
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
## -- Column specification -----------------------------------------------------------------------
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
cat("\n")
# names(df_climate_mean)

df_climate_slope <- read_csv(fn) %>%
  select(station_id, variable, sen_slp) %>%
  pivot_wider(names_from = "variable", values_from = "sen_slp", names_prefix = "Slope_")
```

```
## Rows: 1112 Columns: 8
```

```
## -- Column specification -----------------------------------------------------------------------
## Delimiter: ","
## chr (3): variable, mk_trend, sen_trend
## dbl (5): station_id, median, mk_p_val, sen_slp, sen_incpt
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'Slope_pre', 'Slope_tmp'
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
## 'station_id', 'median_no3', 'median_totn', 'median_ton', 'median_toc', 'median_tocton', 'log_median_no3', 'log_median_totn', 'log_median_ton', 'log_median_toc', 'log_median_tocton', 'trend_NO3', 'trend_TOC', 'trend_TOTN', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'Slope_pre', 'Slope_tmp'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'station_code', 'station_name', 'latitude', 'longitude', 'altitude', 'continent', 'country', 'region', 'group', 'catchment_area', 'urban', 'cultivated', 'total_forest', 'coniferous', 'total_shrub_herbaceous', 'grasslands', 'heathlands', 'transitional_woodland_shrub', 'wetland', 'other', 'bare_sparse', 'decid_mixed', 'lake_water'
```

### Drop locations with >10% cultivated    

```r
dat_5 <- dat_4 %>%
  filter(cultivated <= 10)

cat(nrow(dat_4) - nrow(dat_5), "stations with > 10% cultivated deleted \n")
```

```
## 31 stations with > 10% cultivated deleted
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

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


## 4. Select data   

### a. Selection of variables    
* Select variables to use, and thereby also cases  
* Also remove PL05, which has dubious values   

```r
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
  data[variables]
}

cat("-------------------------------------------------------------\n")
cat("Variables: \n")
cat(params$selected_vars)
cat("\n-------------------------------------------------------------\n")

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
```

```
## -------------------------------------------------------------
## Variables: 
## median_no3,slope_dep_vs_time, TOTN_dep, latitude, longitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
## Number of missing values per variable: 
##             median_no3      slope_dep_vs_time               TOTN_dep               latitude 
##                      0                      0                      0                      0 
##              longitude                    pre                    tmp                  urban 
##                      0                      0                      0                      0 
##             cultivated             coniferous            decid_mixed total_shrub_herbaceous 
##                      0                      8                      8                      0 
##                wetland             lake_water            bare_sparse 
##                      0                      0                      0 
## 
## Number of complete observations: 
## complete
## FALSE  TRUE 
##     8   454 
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
##   Netherlands        0    3
##   Norway             0   82
##   Poland             0    9
##   Slovakia           0   12
##   Sweden             0   91
##   Switzerland        8    0
##   United Kingdom     0   22
##   United States      0   73
## 
## 
## Original data: n = 462 
## Analysis: n = 454
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

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

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```
## 
## Model formula:
## median_no3 ~ slope_dep_vs_time + TOTN_dep + pre + tmp + urban + 
##     cultivated + coniferous + decid_mixed + total_shrub_herbaceous + 
##     wetland + lake_water + bare_sparse
## 
## Fitted party:
## [1] root
## |   [2] TOTN_dep <= 743.05
## |   |   [3] wetland <= 39.918
## |   |   |   [4] slope_dep_vs_time <= -16.79619
## |   |   |   |   [5] bare_sparse <= 0.071: 40.367 (n = 47, err = 29085.6)
## |   |   |   |   [6] bare_sparse > 0.071: 66.417 (n = 12, err = 44526.9)
## |   |   |   [7] slope_dep_vs_time > -16.79619
## |   |   |   |   [8] slope_dep_vs_time <= -7.80595
## |   |   |   |   |   [9] bare_sparse <= 0.003
## |   |   |   |   |   |   [10] slope_dep_vs_time <= -11.43238: 7.139 (n = 27, err = 1535.4)
## |   |   |   |   |   |   [11] slope_dep_vs_time > -11.43238: 18.545 (n = 66, err = 3864.1)
## |   |   |   |   |   [12] bare_sparse > 0.003: 39.821 (n = 28, err = 16031.6)
## |   |   |   |   [13] slope_dep_vs_time > -7.80595
## |   |   |   |   |   [14] pre <= 1138.20007: 8.358 (n = 51, err = 3247.0)
## |   |   |   |   |   [15] pre > 1138.20007
## |   |   |   |   |   |   [16] lake_water <= 8.504: 7.214 (n = 7, err = 85.9)
## |   |   |   |   |   |   [17] lake_water > 8.504: 18.398 (n = 33, err = 3434.7)
## |   |   [18] wetland > 39.918: 243.750 (n = 8, err = 47187.5)
## |   [19] TOTN_dep > 743.05
## |   |   [20] TOTN_dep <= 1387.79
## |   |   |   [21] bare_sparse <= 47.412: 115.118 (n = 141, err = 1505806.3)
## |   |   |   [22] bare_sparse > 47.412: 275.487 (n = 13, err = 83773.4)
## |   |   [23] TOTN_dep > 1387.79: 343.061 (n = 21, err = 1600251.1)
## 
## Number of inner nodes:    11
## Number of terminal nodes: 12
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = full_set)

plot(ev.raw)
```

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


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
##           Mean of squared residuals: 7750.005
##                     % Var explained: 46.79
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

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 6)
```

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-21-2.png)<!-- -->



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
  
  # Save gg obkjct for later plotting / changes
  fn <- paste0(
    "Figures/Partial_plots/gg_",
    params$document_title %>% stringr::str_extract("([^[[:blank:]]]+)"),
    "_", i, ".rds")
  saveRDS(gg, fn)

}
```

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->


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
##          (Int) bar_spr      cnf dcd_mxd lak_wtr      pre slp_dep_vs_tim    tmp TOT_dep
## 298  -4.704000   1.413                   -2.533                  1.7260         0.2120
## 810 -18.330000   1.425                   -2.341                  1.2080         0.2251
## 314   7.604000   1.460                   -2.569 -0.01150         1.8000         0.2151
## 826  -5.962000   1.474                   -2.375 -0.01174         1.2770         0.2285
## 828   8.464000   1.359 -0.24050          -2.494 -0.01810         0.8981         0.2241
## 832  24.650000   1.126 -0.41860 -0.2915  -2.582 -0.01878         0.8310         0.2245
## 874 -12.970000   1.376                   -2.359                  1.2020 -2.675  0.2418
## 302  -1.164000   1.362          -0.1144  -2.534                  1.8080         0.2142
## 812 -14.570000   1.357 -0.11730          -2.390                  1.0050         0.2221
## 362  -0.232400   1.386                   -2.566                  1.7870 -1.382  0.2190
## 814 -14.820000   1.378          -0.1050  -2.345                  1.2940         0.2268
## 316  20.330000   1.374 -0.17760          -2.687 -0.01617         1.6000         0.2099
## 300  -1.559000   1.370 -0.07216          -2.575                  1.6320         0.2094
## 816  -0.009744   1.135 -0.28210 -0.2768  -2.470                  0.9453         0.2224
##     slp_dep_vs_tim:TOT_dep df    logLik   AICc delta weight
## 298                         6 -2704.149 5420.5  0.00  0.125
## 810              0.0004194  7 -2703.170 5420.6  0.11  0.118
## 314                         7 -2703.490 5421.2  0.75  0.086
## 826              0.0004252  8 -2702.482 5421.3  0.80  0.084
## 828              0.0005132  9 -2701.557 5421.5  1.03  0.074
## 832              0.0005539 10 -2700.522 5421.5  1.06  0.074
## 874              0.0005208  8 -2702.618 5421.6  1.07  0.073
## 302                         7 -2703.901 5422.1  1.57  0.057
## 812              0.0004608  8 -2702.903 5422.1  1.64  0.055
## 362                         7 -2703.987 5422.2  1.74  0.052
## 814              0.0004113  8 -2702.961 5422.2  1.76  0.052
## 316                         8 -2702.965 5422.3  1.77  0.052
## 300                         7 -2704.045 5422.3  1.85  0.049
## 816              0.0004976  9 -2701.974 5422.4  1.87  0.049
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

![](164b1_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-25-1.png)<!-- -->








