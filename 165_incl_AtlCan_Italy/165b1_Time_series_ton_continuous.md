---
title: "165b1 Analyse TON change"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
    value: '165x Analyse TON change - test run'
  text_dataset: 
    value: 'Data with slope_ton_vs_time, catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, latitude, longitude, tmp, land cover'
  selected_vars: 
    value: 'slope_ton_vs_time, catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, latitude, longitude, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse'
  extra_pairwise_plots:
    value: 'TON,slope_pre'
  pairwise_plots_same_scale:
    value: 'FALSE'
  response_variable: 
    value: 'slope_ton_vs_time'
  regression_formula: 
    value: 'slope_ton_vs_time ~ TON'

---




**Analysis of NO3 decrease (categorical, "decrease or not"), based on James' trend results**  
  
**Dataset: Including TOC, not including slope_toc_vs_time**   

* Response variable: 'Significant /NO3 decline' (locations with signif. *increase* are *not* excluded)  
* Data from https://github.com/JamesSample/icpw2        
* Sen slope of NO3, TOTN, TOC/TON etc. 1992-2016
* Predictors:
    - catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, latitude, longitude, tmp, land cover (merk at altitude er tatt ut). 
    - I tillegg en variant med TOC, en med TOC+slope_toc_vs_time og en uten begge
    - slope_dep_vs_time: Trend in Tot-N deposition 1992-2016    
    - NO3, TOTN_dep: Medians of NO3, TOTN_dep (Tot-N deposition) 1992-2016   
    - catchment_area (if included in data)      
    - TOC: Medians of TOC       
    - pre, tmp: mean precipitation + temp   
    - Land cover 
  
Technical details: This html file was rendered with `165parm_run_markdown.R` which runs the script `165parm_Time_series_results_James.Rmd` with different inputs, resulting in html files 165a, 165b and 165c.    

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

### James' trends and medians     

```r
if (FALSE){
  #
  # Regression results 1
  #
  folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_no3"
  file <- "trends_1992-2016_no3_results.csv"
  fn <- paste0(folder, "/", file)
  
  dat_start <- read.csv(fn, encoding = "UTF-8")
  cat("Data source:", sQuote(file), ",n =", nrow(dat_start), "\n\n")
  
  cat("Number of TON records:", sum(dat_start$variable == "TON_µg/l N"), "\n")
  # 302
  
} else {
  #
  # Regression results 2
  #
  folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_toc_totn_no3_relax_italy"
  file <- "trends_1992-2016_toc_totn_no3_relax_italy_results.csv"
  fn <- paste0(folder, "/", file)
  
  dat_start <- read.csv(fn, encoding = "UTF-8")
  cat("Data source:", sQuote(file), ",n =", nrow(dat_start), "\n\n")
  
  sum(dat_start$variable == "TON_µg/l N")
  # 293
  
}
```

```
## Data source: 'trends_1992-2016_toc_totn_no3_relax_italy_results.csv' ,n = 2418 
## 
## [1] 293
```

### Start 'dat'  

With slope regression data  
* Make one line per station    



```r
# table(dat_start$variable)

# Slope 
df1 <- dat_start %>%
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l")) %>%
  select(station_id, variable, sen_slp) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "sen_slp") %>%
  rename(slope_ton_vs_time = `TON_µg/l N`, 
         slope_toc_vs_time = `TOC_mg C/l`)
  
# Slope p-value
df2 <- dat_start %>%
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l")) %>%
  select(station_id, variable, mk_p_val) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "mk_p_val") %>%
  rename(p_ton_vs_time = `TON_µg/l N`, 
         p_toc_vs_time = `TOC_mg C/l`)

# Medians
df3 <- dat_start %>%
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l")) %>%
  select(station_id, variable, median) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "median") %>%
  rename(TON = `TON_µg/l N`, 
         TOC = `TOC_mg C/l`)

cat("\n")
cat("df1, n =", nrow(df1), "\n")
cat("df2, n =", nrow(df2), "\n")
cat("df3, n =", nrow(df3), "\n")

dat_1_all <- df1 %>%
  left_join2(df2, by = "station_id") %>%
  left_join2(df3, by = "station_id")

cat("dat_1_all, n =", nrow(dat_1_all), " \n")

dat_1 <- dat_1_all %>%
  filter(!is.na(slope_ton_vs_time) & !is.na(slope_toc_vs_time))

cat("dat_1, n =", nrow(dat_1), " \n")
```

```
## 
## df1, n = 293 
## df2, n = 293 
## df3, n = 293 
## dat_1_all, n = 293  
## dat_1, n = 287
```


```r
# dat_1
# 
# str <- "slope_no3_vs_time ~ p_no3_vs_time"
# plot(as.formula(str), data = dat_1)
# lm(as.formula(str), data = dat_1)
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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'TON', 'TOC'
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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'TON', 'TOC', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'TON', 'TOC', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'TON', 'TOC', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'slope_pre', 'slope_tmp'
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
## Removed 12 rows (Deleted stations with > 5 % cultivated)
## Removed 5 rows (Deleted stations with > 5 % urban)
```


### Data set used  

```r
dat <- dat_5
```


## 3. Plot slopes    


```r
ggplot(dat, aes(slope_dep_vs_time, slope_ton_vs_time)) + 
  geom_point(data = dat %>% filter(p_ton_vs_time < 0.05), size = rel(2)) +
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


## 4. Select data   

### a. Selection of variables  
* Select variables to use, and thereby also cases  
* Saves data both before and after rows with removing missing predictors are removed

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
fn <- paste0(substr(params$document_title, 1, 3), "_", params$response_variable, "_data.xlsx")
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
## slope_ton_vs_time,catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, pre, tmp,urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse, TOC
## -------------------------------------------------------------
## Removed 0 rows (station PL05 (has dubious NO3 data))
##                       Var Missing
## 1       slope_ton_vs_time       0
## 2          catchment_area       0
## 3                     TON       0
## 4               slope_pre       0
## 5               slope_tmp       0
## 6       slope_dep_vs_time       0
## 7                TOTN_dep       0
## 8                     pre       0
## 9                     tmp       0
## 10                  urban       0
## 11             cultivated       0
## 12             coniferous       3
## 13            decid_mixed       3
## 14 total_shrub_herbaceous       0
## 15                wetland       0
## 16             lake_water       0
## 17            bare_sparse       0
## 18                    TOC       0
## 
## Dataset after removing urban, cultivated, PL05 saved as '165_slope_ton_vs_time_data.xlsx' 
## 
## Number of rows that will be excluded: 
## 
## FALSE  TRUE 
##   266     3 
## 
## 
## Number of complete observations by country: 
##                 Row_excluded
## country          FALSE TRUE
##   Canada            59    3
##   Finland           24    0
##   Germany            1    0
##   Norway            80    0
##   Sweden            81    0
##   United Kingdom    21    0
## 
## 
## Data before removing PL05: n = 269 
## Data after removing PL05: n = 269 
## Data after removing missing predictors: n = 266
```


### b. Correlations   

```r
gg <- GGally::ggcorr(
  df_analysis, 
  method = c("complete.obs", "kendall"), 
  label = TRUE,
  hjust = 0.9, angle = -30) # +                    # slanted labels
gg + coord_cartesian(x = c(-2, 20), y = c(-2,22))  # fix margins
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



## 5. Tree and forest classification


### Split into training and validation data

```r
set.seed(123)

x <- runif(nrow(df_analysis))
train <- ifelse(x < 0.9, TRUE, FALSE)

train_set <- df_analysis[train,]  %>% 
  # mutate(no3_decline_f = factor(no3_decline)) %>% select(-no3_decline, -longitude, - latitude) %>%
  as.data.frame()
valid_set <- df_analysis[!train,] %>% 
  # mutate(no3_decline_f = factor(no3_decline)) %>% select(-no3_decline, -longitude, - latitude) %>%
  as.data.frame()
```


### a. Tree classification using 'party'   

```r
# Formula for trees and forests
tree_formula <- paste0(params$response_variable, " ~ .") 

(ct = ctree(as.formula(tree_formula), data = train_set))

plot(ct, main="Conditional Inference Tree")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
#
# Only for classification (when response variable is a classification)
#
if (FALSE){
  cat("\n\n")
  cat("Table of prediction errors \n")
  table(predict(ct), train_set[[params$response_variable]])
  cat("\n\n")
  cat("Classification of training set \n")
  
  # Validation set
  tr.pred = predict(ct, newdata = valid_set, type="prob")
  colnames(tr.pred) <- c("P0", "P1")
  # tr.pred <- tr.pred %>% map_dfr(~data.frame(P0 = .[1], P1 = .[2]))
  table(tr.pred[,"P1"] > 0.5, valid_set[[params$response_variable]])
  
}

plot(valid_set[[params$response_variable]],  predict(ct, newdata = valid_set))
abline(0, 1, lty = "dashed")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## 
## Model formula:
## slope_ton_vs_time ~ catchment_area + TON + slope_pre + slope_tmp + 
##     slope_dep_vs_time + TOTN_dep + pre + tmp + urban + cultivated + 
##     coniferous + decid_mixed + total_shrub_herbaceous + wetland + 
##     lake_water + bare_sparse + TOC
## 
## Fitted party:
## [1] root
## |   [2] decid_mixed <= 53.75923
## |   |   [3] slope_tmp <= 0.05278
## |   |   |   [4] TOC <= 16.25
## |   |   |   |   [5] slope_pre <= 5.14375: -0.636 (n = 100, err = 712.4)
## |   |   |   |   [6] slope_pre > 5.14375: 1.515 (n = 42, err = 268.9)
## |   |   |   [7] TOC > 16.25: 7.353 (n = 7, err = 169.8)
## |   |   [8] slope_tmp > 0.05278
## |   |   |   [9] tmp <= 4.55
## |   |   |   |   [10] tmp <= 3.05
## |   |   |   |   |   [11] catchment_area <= 10.736: 0.731 (n = 21, err = 18.2)
## |   |   |   |   |   [12] catchment_area > 10.736: -1.394 (n = 7, err = 48.2)
## |   |   |   |   [13] tmp > 3.05: 3.697 (n = 10, err = 62.0)
## |   |   |   [14] tmp > 4.55: 7.753 (n = 17, err = 205.0)
## |   [15] decid_mixed > 53.75923
## |   |   [16] TOC <= 5.9: 4.390 (n = 17, err = 209.0)
## |   |   [17] TOC > 5.9: 11.889 (n = 19, err = 188.0)
## 
## Number of inner nodes:    8
## Number of terminal nodes: 9
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(tree_formula, data = train_set)
plot(ev.raw)
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# For classification:
# cat("Predicted in training data: \n")
# table(predict(ev.raw), train_set[[params$response_variable]])

# For continuous response:
plot(predict(ev.raw), train_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in training set")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
plot(predict(ev.raw, newdata = valid_set), 
     valid_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in validation set")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

```r
# cat("\n\nPrediction errors in training data: \n")
# 1-mean(predict(ev.raw) == train_set$no3_decline_f)
```


### c. Random forest  
* *For results/interpretation, see separate document '160_randomforest_James_data.html'*  


#### c1a. Predict on training data

```r
model1 <- randomForest(as.formula(tree_formula), 
                       data = train_set, 
                       mtry = 5,
                       importance = TRUE)

# Predicting in training set
df_plot <- data.frame(
  Predicted = predict(model1, train_set),
  Observed = train_set[[params$response_variable]])
ggplot(df_plot, aes(Predicted, Observed)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  labs(title = "Predictions in training set")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
cat("R-square, predicting in training set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq

# Predicting in validation set
df_plot <- data.frame(
  Predicted = predict(model1, valid_set),
  Observed = valid_set[[params$response_variable]])
ggplot(df_plot, aes(Predicted, Observed)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  labs(title = "Predictions in validation set")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
cat("R-square, predicting in validation set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in training set: 
## [1] 0.9646196
## R-square, predicting in validation set: 
## [1] 0.5444458
```

#### c1b. Model for all data    

```r
full_set <- df_analysis %>% 
  # mutate(no3_decline_f = factor(no3_decline)) %>% 
  # select(-no3_decline, -longitude, - latitude) %>%
  as.data.frame()

model1 <- randomForest(as.formula(tree_formula), 
                       data = full_set, 
                       mtry = 5,
                       importance = TRUE)

# Prediction
df_plot <- data.frame(
  Predicted = predict(model1, full_set),
  Observed = full_set[[params$response_variable]])
ggplot(df_plot, aes(Predicted, Observed)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  labs(title = "Predictions in entire data set")
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
cat("R-square, predicting in entire data set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in entire data set: 
## [1] 0.9621518
```

#### c2. Importance of variables

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 6)  
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
if (FALSE){
  # For classification (discrete response variable)
  plot_multi_way_importance(importance, 
                            x_measure = "accuracy_decrease", 
                            y_measure = "gini_decrease", 
                            size_measure = "p_value", no_of_labels = 6)
} else {
  
  # For regression (continuous response variable)
  plot_multi_way_importance(importance, 
                            x_measure = "mse_increase", 
                            y_measure = "node_purity_increase",
                            size_measure = "p_value", no_of_labels = 6)
}
```

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1             bare_sparse        5.63376        1333    1.4175161             164.6402
## 2          catchment_area        4.09200        2762    0.3570003             133.7022
## 3              coniferous        4.35032        2370    0.9406809             137.6327
## 4              cultivated        6.46784        1015    0.1247724              43.4780
## 5             decid_mixed        3.05728        2764    3.5242278             586.9380
## 6              lake_water        3.61376        3109    0.9191167             183.0458
## 7                     pre        3.19576        2578    6.2157388             503.5527
## 8       slope_dep_vs_time        3.41952        3046    2.9983726             291.4399
## 9               slope_pre        2.31552        2689    7.6281420            1023.5769
## 10              slope_tmp        2.54552        2904    6.6545176             654.9065
## 11                    tmp        3.33080        2707    2.8205395             421.3590
## 12                    TOC        2.25200        3969    4.0177656             633.8421
## 13                    TON        3.16152        3262    1.7951786             313.4534
## 14 total_shrub_herbaceous        4.08528        2830    0.5026051             132.4765
## 15               TOTN_dep        3.93200        2844    1.2879397             215.4056
## 16                  urban        4.31912        1658    1.5843820             172.2088
## 17                wetland        4.52352        2457    0.2785327             103.5898
##    no_of_trees times_a_root       p_value
## 1          474           21  1.000000e+00
## 2          500            0  9.035633e-04
## 3          493            9  9.999993e-01
## 4          441            1  1.000000e+00
## 5          497          105  7.891718e-04
## 6          499            0  2.742971e-23
## 7          499           63  7.148313e-01
## 8          498           34  2.261518e-18
## 9          498          145  4.779432e-02
## 10         498           71  1.671599e-09
## 11         495           29  2.137297e-02
## 12         500            6 1.532198e-145
## 13         498            8  1.153574e-37
## 14         497            0  4.032125e-06
## 15         500            6  1.075151e-06
## 16         488            1  1.000000e+00
## 17         498            1  9.988075e-01
```



#### c3. Random forest, show partial effects  


```r
# Which variables to include:
variables_for_plot <- importance %>%
  mutate(variable = levels(variable)[as.numeric(variable)]) %>%
  arrange(desc(times_a_root)) %>%
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

i <- max_number_of_plots

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

![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->








## 6. Linear regression      

```r
fm <- lm(
  as.formula(params$regression_formula),
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
## Global model call: lm(formula = as.formula(params$regression_formula), data = df_analysis, 
##     na.action = "na.fail")
## ---
## Model selection table 
##          (Int)  bar_spr   ctc_are      cnf dcd_mxd lak_wtr      pre slp_dep_vs_tim  slp_pre
## 73431   -9.999          -0.001563 -0.03235 0.01835         0.001600       -0.07101         
## 73429  -10.140                    -0.03293 0.01674         0.001646       -0.07203         
## 81605   -9.023                    -0.04703                 0.001593       -0.07435         
## 81607   -8.850          -0.001474 -0.04746                 0.001548       -0.07365         
## 73687   -9.892          -0.001585 -0.03170 0.02364         0.001668       -0.07655 -0.06161
## 114373  -8.927                    -0.04846                 0.001521       -0.06840         
## 106197 -10.310                    -0.03109 0.02055         0.001587       -0.06564         
## 81861   -8.564                    -0.05083                 0.001644       -0.08056 -0.06107
## 81863   -8.391          -0.001473 -0.05126                 0.001599       -0.07986 -0.06099
## 73685  -10.040                    -0.03231 0.02189         0.001713       -0.07747 -0.06032
## 106199 -10.160          -0.001391 -0.03084 0.02142         0.001555       -0.06568         
## 73413  -10.510                    -0.03973                 0.001648       -0.07573         
## 114375  -8.789          -0.001287 -0.04863                 0.001492       -0.06866         
## 114629  -8.483                    -0.05211                 0.001573       -0.07458 -0.05922
## 73415  -10.420          -0.001373 -0.03979                 0.001608       -0.07515         
## 106453 -10.200                    -0.03055 0.02541         0.001653       -0.07107 -0.05822
## 114374  -8.110 -0.01648           -0.05182                 0.001606       -0.06667         
## 81608   -8.128 -0.01468 -0.001494 -0.05036                 0.001627       -0.07256         
## 81606   -8.324 -0.01427           -0.04983                 0.001672       -0.07330         
## 106455 -10.050          -0.001419 -0.03028 0.02643         0.001622       -0.07125 -0.05970
## 81864   -7.487 -0.01726 -0.001496 -0.05512                 0.001699       -0.07934 -0.06840
## 7895    -9.985          -0.001521 -0.03079 0.02170         0.001549       -0.06662         
## 81862   -7.684 -0.01684           -0.05459                 0.001743       -0.08006 -0.06830
## 114630  -7.484 -0.01898           -0.05647                 0.001677       -0.07342 -0.06718
## 7893   -10.120                    -0.03138 0.02007         0.001594       -0.06769         
## 81623   -9.281          -0.001552 -0.03921 0.01188         0.001570       -0.07165         
## 73461  -10.140                    -0.03242 0.01677 0.01984 0.001694       -0.07365         
## 114631  -8.344          -0.001291 -0.05229                 0.001544       -0.07487 -0.05943
## 81879   -8.878          -0.001573 -0.04115 0.01542         0.001635       -0.07824 -0.07068
## 73463  -10.010          -0.001472 -0.03199 0.01828 0.01506 0.001640       -0.07230         
## 114389  -9.380                    -0.03990 0.01243         0.001541       -0.06587         
## 81621   -9.397                    -0.04001 0.01006         0.001614       -0.07268         
## 114376  -7.970 -0.01654 -0.001290 -0.05200                 0.001576       -0.06693         
##        slp_tmp    tmp    TOC      TON ttl_shr_hrb     urb      wtl df   logLik   AICc delta
## 73431    156.5 0.3990 0.9082 -0.02089                     -0.05400 12 -645.151 1315.5  0.00
## 73429    157.7 0.3992 0.9063 -0.02078                     -0.05306 11 -646.371 1315.8  0.25
## 81605    155.5 0.4188 0.9431 -0.02156   -0.015450         -0.06349 11 -646.374 1315.8  0.25
## 81607    154.8 0.4214 0.9485 -0.02173   -0.016240         -0.06524 12 -645.281 1315.8  0.26
## 73687    153.8 0.3939 0.9135 -0.02136                     -0.05536 13 -644.300 1316.0  0.51
## 114373   156.0 0.4568 0.9332 -0.02008   -0.019110 -0.2876 -0.07519 12 -645.405 1316.0  0.51
## 106197   158.8 0.4325 0.8883 -0.01914             -0.2841 -0.06224 12 -645.422 1316.1  0.54
## 81861    152.1 0.4197 0.9597 -0.02226   -0.020350         -0.06808 12 -645.548 1316.3  0.79
## 81863    151.4 0.4223 0.9652 -0.02244   -0.021120         -0.06982 13 -644.451 1316.3  0.81
## 73685    155.0 0.3942 0.9116 -0.02124                     -0.05438 12 -645.562 1316.4  0.82
## 106199   157.6 0.4274 0.8926 -0.01948             -0.2421 -0.06172 13 -644.471 1316.4  0.85
## 73413    170.5 0.4433 0.9521 -0.02160                     -0.06128 10 -647.769 1316.4  0.87
## 114375   155.4 0.4535 0.9394 -0.02045   -0.019250 -0.2453 -0.07500 13 -644.589 1316.6  1.09
## 114629   152.7 0.4568 0.9497 -0.02081   -0.023760 -0.2802 -0.07934 13 -644.624 1316.7  1.16
## 73415    170.6 0.4469 0.9576 -0.02176                     -0.06280 11 -646.828 1316.7  1.16
## 106453   156.2 0.4267 0.8939 -0.01963             -0.2757 -0.06323 13 -644.664 1316.8  1.24
## 114374   149.1 0.4125 0.9204 -0.01974   -0.023100 -0.3127 -0.08122 13 -644.723 1316.9  1.35
## 81608    148.6 0.3789 0.9379 -0.02155   -0.019520         -0.06972 13 -644.733 1316.9  1.37
## 81606    149.4 0.3775 0.9327 -0.02138   -0.018630         -0.06782 12 -645.860 1317.0  1.42
## 106455   154.9 0.4214 0.8984 -0.01999             -0.2327 -0.06273 14 -643.669 1317.0  1.47
## 81864    143.7 0.3726 0.9547 -0.02231   -0.025570         -0.07564 14 -643.701 1317.1  1.54
## 7895     152.3 0.4207 0.8759 -0.02066                              11 -647.027 1317.1  1.56
## 81862    144.5 0.3711 0.9495 -0.02213   -0.024680         -0.07374 13 -644.840 1317.1  1.59
## 114630   144.2 0.4057 0.9371 -0.02051   -0.028980 -0.3081 -0.08684 14 -643.727 1317.1  1.59
## 7893     153.5 0.4205 0.8746 -0.02056                              10 -648.167 1317.2  1.66
## 81623    152.8 0.4018 0.9206 -0.02118   -0.008961         -0.05845 13 -644.887 1317.2  1.68
## 73461    150.3 0.3729 0.9358 -0.02133                     -0.05231 12 -646.007 1317.2  1.71
## 114631   152.0 0.4535 0.9559 -0.02118   -0.023920 -0.2378 -0.07916 14 -643.797 1317.3  1.73
## 81879    148.2 0.3971 0.9316 -0.02183   -0.012450         -0.06174 14 -643.803 1317.3  1.74
## 73463    151.0 0.3791 0.9305 -0.02130                     -0.05338 13 -644.944 1317.3  1.80
## 114389   154.0 0.4392 0.9029 -0.01938   -0.011740 -0.3103 -0.06892 13 -644.977 1317.4  1.86
## 81621    153.8 0.4021 0.9192 -0.02108   -0.009254         -0.05766 12 -646.091 1317.4  1.88
## 114376   148.4 0.4090 0.9265 -0.02011   -0.023260 -0.2704 -0.08104 14 -643.897 1317.5  1.93
##        weight
## 73431   0.053
## 73429   0.047
## 81605   0.046
## 81607   0.046
## 73687   0.041
## 114373  0.041
## 106197  0.040
## 81861   0.035
## 81863   0.035
## 73685   0.035
## 106199  0.034
## 73413   0.034
## 114375  0.031
## 114629  0.029
## 73415   0.029
## 106453  0.028
## 114374  0.027
## 81608   0.026
## 81606   0.026
## 106455  0.025
## 81864   0.024
## 7895    0.024
## 81862   0.024
## 114630  0.024
## 7893    0.023
## 81623   0.023
## 73461   0.022
## 114631  0.022
## 81879   0.022
## 73463   0.021
## 114389  0.021
## 81621   0.021
## 114376  0.020
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

![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](165b1_Time_series_ton_continuous_files/figure-html/unnamed-chunk-27-2.png)<!-- -->








