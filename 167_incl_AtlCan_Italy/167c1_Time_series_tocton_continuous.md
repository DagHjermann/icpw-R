---
title: "167c1 Analyse TOC/TON change"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
    value: '167x Analyse TOC/TON change - test run'
  text_dataset: 
    value: 'Data with slope_tocton_vs_time, catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, latitude, longitude, tmp, land cover'
  selected_vars: 
    value: 'slope_tocton_vs_time, catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse'
  extra_pairwise_plots:
    value: 'TON,slope_pre'
  pairwise_plots_same_scale:
    value: 'FALSE'
  response_variable: 
    value: 'slope_tocton_vs_time'
  regression_formula: 
    value: 'slope_tocton_vs_time ~ TON'

---




**Analysis of NO3 decrease (categorical, "decrease or not"), based on James' trend results**  
  
**Basis + TOC + TON slopes**   

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
  
Technical details: This html file was rendered with `167parm_run_markdown.R` which runs the script `167parm_Time_series_results_James.Rmd` with different inputs, resulting in html files 167a, 167b and 167c.    

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
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l", "TOC/TON")) %>%
  select(station_id, variable, sen_slp) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "sen_slp") %>%
  rename(slope_tocton_vs_time = `TOC/TON`,
         slope_ton_vs_time = `TON_µg/l N`, 
         slope_toc_vs_time = `TOC_mg C/l`)
  
# Slope p-value
df2 <- dat_start %>%
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l", "TOC/TON")) %>%
  select(station_id, variable, mk_p_val) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "mk_p_val") %>%
  rename(p_tocton_vs_time = `TOC/TON`,
         p_ton_vs_time = `TON_µg/l N`, 
         p_toc_vs_time = `TOC_mg C/l`)

# Medians
df3 <- dat_start %>%
  filter(variable %in% c("TON_µg/l N", "TOC_mg C/l", "TOC/TON")) %>%
  select(station_id, variable, median) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "median") %>%
  rename(TOCTON = `TOC/TON`,
         TON = `TON_µg/l N`, 
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
  filter(!is.na(slope_tocton_vs_time))

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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'slope_tocton_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'p_tocton_vs_time', 'TON', 'TOC', 'TOCTON'
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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'slope_tocton_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'p_tocton_vs_time', 'TON', 'TOC', 'TOCTON', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time'
## 
## Variables used to join: 
## 'station_id'
## 
## Variables added: 
## 'pre', 'tmp'
## Variables before join: 
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'slope_tocton_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'p_tocton_vs_time', 'TON', 'TOC', 'TOCTON', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp'
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
## 'station_id', 'slope_ton_vs_time', 'slope_toc_vs_time', 'slope_tocton_vs_time', 'p_ton_vs_time', 'p_toc_vs_time', 'p_tocton_vs_time', 'TON', 'TOC', 'TOCTON', 'TOTN_dep', 'slope_dep_vs_time', 'p_dep_vs_time', 'pre', 'tmp', 'slope_pre', 'slope_tmp'
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
ggplot(dat, aes(slope_dep_vs_time, slope_tocton_vs_time)) + 
  geom_point(data = dat %>% filter(p_ton_vs_time < 0.05), size = rel(2)) +
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
```

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


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
df_analysis_rowsexcluded <- df_analysis_allrows %>%
  filter(!Row_excluded)

# Save to excel
fn <- paste0(
  stringr::str_extract(params$document_title, "[^[[:blank:]]]+"),
  "_data.xlsx")
writexl::write_xlsx(df_analysis_rowsexcluded, paste0("Data_analysed/", fn))

# Remove variables that will note be used
df_analysis <- delete_unused_variables(df_analysis_rowsexcluded, params$selected_vars)

cat("\n\n")
cat("Data before removing PL05: n =", nrow(dat_5), "\n")
cat("Data after removing PL05: n =", nrow(df_analysis_allrows), "\n")
cat("Data after removing missing predictors: n =", nrow(df_analysis), "\n")
```

```
## -------------------------------------------------------------
## Variables: 
## slope_tocton_vs_time,catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp,urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse, slope_toc_vs_time, slope_ton_vs_time
## -------------------------------------------------------------
## Removed 0 rows (station PL05 (has dubious NO3 data))
##                       Var Missing
## 1    slope_tocton_vs_time       0
## 2          catchment_area       0
## 3               slope_pre       0
## 4               slope_tmp       0
## 5       slope_dep_vs_time       0
## 6                  TOCTON       0
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
## 18      slope_toc_vs_time       0
## 19      slope_ton_vs_time       0
## 
## Dataset after removing urban, cultivated, PL05 saved as '167_slope_tocton_vs_time_data.xlsx' 
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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## 
## Model formula:
## slope_tocton_vs_time ~ catchment_area + slope_pre + slope_tmp + 
##     slope_dep_vs_time + TOCTON + TOTN_dep + pre + tmp + urban + 
##     cultivated + coniferous + decid_mixed + total_shrub_herbaceous + 
##     wetland + lake_water + bare_sparse + slope_toc_vs_time + 
##     slope_ton_vs_time
## 
## Fitted party:
## [1] root
## |   [2] TOCTON <= 45.61404
## |   |   [3] slope_ton_vs_time <= 4.34962
## |   |   |   [4] slope_ton_vs_time <= -0.3
## |   |   |   |   [5] slope_tmp <= 0.03258: 1.048 (n = 9, err = 2.1)
## |   |   |   |   [6] slope_tmp > 0.03258
## |   |   |   |   |   [7] slope_toc_vs_time <= 0.12124: 0.364 (n = 39, err = 0.9)
## |   |   |   |   |   [8] slope_toc_vs_time > 0.12124: 0.590 (n = 12, err = 0.4)
## |   |   |   [9] slope_ton_vs_time > -0.3
## |   |   |   |   [10] slope_toc_vs_time <= 0.06667: -0.041 (n = 59, err = 1.7)
## |   |   |   |   [11] slope_toc_vs_time > 0.06667: 0.286 (n = 63, err = 1.4)
## |   |   [12] slope_ton_vs_time > 4.34962: -1.393 (n = 17, err = 34.5)
## |   [13] TOCTON > 45.61404
## |   |   [14] slope_ton_vs_time <= 10.5719
## |   |   |   [15] TOCTON <= 56.5: -2.315 (n = 11, err = 9.0)
## |   |   |   [16] TOCTON > 56.5: -4.060 (n = 15, err = 8.5)
## |   |   [17] slope_ton_vs_time > 10.5719: -5.108 (n = 15, err = 24.7)
## 
## Number of inner nodes:    8
## Number of terminal nodes: 9
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(tree_formula, data = train_set)
plot(ev.raw)
```

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# For classification:
# cat("Predicted in training data: \n")
# table(predict(ev.raw), train_set[[params$response_variable]])

# For continuous response:
plot(predict(ev.raw), train_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in training set")
```

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
plot(predict(ev.raw, newdata = valid_set), 
     valid_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in validation set")
```

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
cat("R-square, predicting in validation set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in training set: 
## [1] 0.9845257
## R-square, predicting in validation set: 
## [1] 0.9849407
```

#### c1b. Model for all data    

```r
full_set <- df_analysis %>% 
  # mutate(no3_decline_f = factor(no3_decline)) %>% 
  # select( -longitude, - latitude) %>%
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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
cat("R-square, predicting in entire data set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in entire data set: 
## [1] 0.9848922
```

#### c2. Importance of variables

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 6)  
```

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1                  TOCTON       1.602000        3479  1.566141904           266.039230
## 2       slope_ton_vs_time       1.694000        4181  1.205144802           179.861973
## 3               slope_pre       3.542904        2373  0.258607106            76.030886
## 4             decid_mixed       3.395808        2605  0.231050625            59.498762
## 5               slope_tmp       3.254712        2541  0.403084406            46.692539
## 6                     pre       4.326712        2219  0.260904065            30.981675
## 7       slope_dep_vs_time       3.542904        2631  0.359719787            34.924208
## 8              coniferous       3.715808        2361  0.071763178             9.913152
## 9                TOTN_dep       4.147808        2590  0.119157041            15.488959
## 10            bare_sparse       6.444544        1183  0.042854176             4.872982
## 11                    tmp       4.443616        2186  0.138100409            17.650389
## 12         catchment_area       4.481808        2651  0.007333195             5.156835
## 13             cultivated       8.104096         665  0.004730784             0.893576
## 14             lake_water       4.114904        2696  0.029099645             8.689677
## 15      slope_toc_vs_time       2.854000        3509  0.089546930            19.765222
## 16 total_shrub_herbaceous       4.151424        2472  0.038119699             7.485125
## 17                  urban       5.427504        1360  0.014785611             3.166630
## 18                wetland       4.763808        2377 -0.001578960             5.499852
##    no_of_trees times_a_root       p_value
## 1          500          153  4.304660e-91
## 2          500           93 1.713675e-237
## 3          499           70  9.443099e-01
## 4          498           63  6.671540e-04
## 5          497           43  2.883936e-02
## 6          497           26  9.999994e-01
## 7          499           26  9.276979e-05
## 8          498            8  9.674253e-01
## 9          498            8  1.852186e-03
## 10         464            4  1.000000e+00
## 11         496            4  1.000000e+00
## 12         498            2  1.705968e-05
## 13         376            0  1.000000e+00
## 14         499            0  2.159017e-07
## 15         500            0  4.585644e-96
## 16         494            0  3.178505e-01
## 17         474            0  1.000000e+00
## 18         498            0  9.342277e-01
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

![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->








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
##         (Int)  bar_spr    ctc_are     cnf     clt   dcd_mxd   lak_wtr slp_dep_vs_tim slp_pre
## 163599 0.7969          -0.0004963 0.01086 0.10710                                    0.01993
## 163087 0.4083          -0.0004918 0.01123 0.09860                                    0.02277
## 16188  1.9270 -0.01192 -0.0005124         0.08601 -0.010190 -0.012370                0.01956
## 15676  1.6430 -0.01154 -0.0005227         0.07480 -0.010490 -0.015400                0.02148
## 196367 0.8636          -0.0005091 0.01122 0.11250                                    0.02094
## 81212  1.6310 -0.01159 -0.0005065         0.09348 -0.009817 -0.014970                0.01952
## 163727 0.8607          -0.0005067 0.01140 0.11660                           0.003574 0.02158
## 15668  1.6670 -0.01194 -0.0004389                 -0.010060 -0.015630                0.02165
## 48956  2.0440 -0.01267 -0.0005308         0.08961 -0.010230 -0.013330                0.02036
## 163615 0.7360          -0.0005023 0.01201 0.10350  0.002036                          0.01880
## 81724  1.8810 -0.01192 -0.0005004         0.09996 -0.009675 -0.012410                0.01819
## 163119 0.5257          -0.0005120 0.01066 0.09545           -0.003746                0.02141
## 229135 0.7983          -0.0004890 0.01077 0.11510                                    0.01915
## 163600 0.8812 -0.00143 -0.0004936 0.01044 0.10440                                    0.01954
## 195855 0.4205          -0.0005003 0.01151 0.10160                                    0.02373
##        slp_tmp slp_toc_vs_tim slp_ton_vs_tim      tmp      TOC ttl_shr_hrb    TOT_dep      urb
## 163599  -6.143          4.562        -0.1550 -0.08021 -0.04691     0.01070                    
## 163087                  4.687        -0.1606 -0.06138 -0.04762     0.01191                    
## 16188   -6.037          4.517        -0.1554 -0.08185 -0.04748                                
## 15676                   4.515        -0.1598 -0.06126 -0.04860                                
## 196367  -6.920          4.613        -0.1542 -0.06264 -0.04821     0.01093 -0.0002021         
## 81212                   4.762        -0.1634 -0.05895 -0.04845                        -0.04651
## 163727  -7.027          4.597        -0.1535 -0.07399 -0.04800     0.01073                    
## 15668                   4.580        -0.1587 -0.05731 -0.04972                                
## 48956   -6.693          4.581        -0.1549 -0.06228 -0.04923             -0.0002246         
## 163615  -6.201          4.534        -0.1548 -0.08136 -0.04786     0.01179                    
## 81724   -5.269          4.720        -0.1589 -0.07732 -0.04750                        -0.03829
## 163119                  4.609        -0.1603 -0.06035 -0.04815     0.01100                    
## 229135  -5.927          4.651        -0.1566 -0.07802 -0.04681     0.01036            -0.02191
## 163600  -6.678          4.559        -0.1546 -0.08295 -0.04731     0.01031                    
## 195855                  4.734        -0.1605 -0.04753 -0.04859     0.01217 -0.0001403         
##            wtl df   logLik  AICc delta weight
## 163599 0.01377 13 -206.946 441.3  0.00  0.122
## 163087 0.01409 12 -208.220 441.7  0.34  0.103
## 16188          13 -207.292 442.0  0.69  0.086
## 15676          12 -208.403 442.0  0.70  0.086
## 196367 0.01446 14 -206.368 442.4  1.07  0.071
## 81212          13 -207.556 442.6  1.22  0.066
## 163727 0.01460 14 -206.530 442.7  1.40  0.061
## 15668          11 -209.905 442.8  1.51  0.057
## 48956          14 -206.607 442.9  1.55  0.056
## 163615 0.01471 14 -206.679 443.0  1.69  0.052
## 81724          14 -206.733 443.1  1.80  0.050
## 163119 0.01337 13 -207.859 443.2  1.83  0.049
## 229135 0.01308 14 -206.776 443.2  1.89  0.048
## 163600 0.01331 14 -206.814 443.3  1.96  0.046
## 195855 0.01460 13 -207.935 443.3  1.98  0.045
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

![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](167c1_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-2.png)<!-- -->








