---
title: "167b2 Analyse TOC/TON change"
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
  
**Basis + TOC + TON medians**   

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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


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
## slope_tocton_vs_time,catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp,urban, cultivated, total_forest, total_shrub_herbaceous,wetland, lake_water, bare_sparse, TOC, TON
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
## 12           total_forest       0
## 13 total_shrub_herbaceous       0
## 14                wetland       0
## 15             lake_water       0
## 16            bare_sparse       0
## 17                    TOC       0
## 18                    TON       0
## 
## Dataset after removing urban, cultivated, PL05 saved as '167_slope_tocton_vs_time_data.xlsx' 
## 
## Number of rows that will be excluded: 
## 
## FALSE 
##   269 
## 
## 
## Number of complete observations by country: 
##                 Row_excluded
## country          FALSE
##   Canada            62
##   Finland           24
##   Germany            1
##   Norway            80
##   Sweden            81
##   United Kingdom    21
## 
## 
## Data before removing PL05: n = 269 
## Data after removing PL05: n = 269 
## Data after removing missing predictors: n = 269
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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## 
## Model formula:
## slope_tocton_vs_time ~ catchment_area + slope_pre + slope_tmp + 
##     slope_dep_vs_time + TOCTON + TOTN_dep + pre + tmp + urban + 
##     cultivated + total_forest + total_shrub_herbaceous + wetland + 
##     lake_water + bare_sparse + TOC + TON
## 
## Fitted party:
## [1] root
## |   [2] TOCTON <= 45.61404
## |   |   [3] lake_water <= 25.251
## |   |   |   [4] pre <= 1409.19995
## |   |   |   |   [5] bare_sparse <= 33.792: 0.319 (n = 133, err = 12.6)
## |   |   |   |   [6] bare_sparse > 33.792: -0.110 (n = 8, err = 0.3)
## |   |   |   [7] pre > 1409.19995
## |   |   |   |   [8] slope_tmp <= 0.04494: 0.058 (n = 22, err = 2.1)
## |   |   |   |   [9] slope_tmp > 0.04494: -1.250 (n = 7, err = 16.2)
## |   |   [10] lake_water > 25.251
## |   |   |   [11] TOCTON <= 27.19239
## |   |   |   |   [12] total_shrub_herbaceous <= 14.302: 0.292 (n = 16, err = 0.6)
## |   |   |   |   [13] total_shrub_herbaceous > 14.302: -0.086 (n = 9, err = 0.1)
## |   |   |   [14] TOCTON > 27.19239: -2.402 (n = 7, err = 2.2)
## |   [15] TOCTON > 45.61404
## |   |   [16] TOCTON <= 57.76471: -2.910 (n = 15, err = 21.8)
## |   |   [17] TOCTON > 57.76471
## |   |   |   [18] TOC <= 8.2: -4.065 (n = 15, err = 8.5)
## |   |   |   [19] TOC > 8.2: -5.517 (n = 11, err = 16.0)
## 
## Number of inner nodes:     9
## Number of terminal nodes: 10
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(tree_formula, data = train_set)
plot(ev.raw)
```

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# For classification:
# cat("Predicted in training data: \n")
# table(predict(ev.raw), train_set[[params$response_variable]])

# For continuous response:
plot(predict(ev.raw), train_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in training set")
```

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
plot(predict(ev.raw, newdata = valid_set), 
     valid_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in validation set")
```

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
cat("R-square, predicting in validation set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in training set: 
## [1] 0.9824462
## R-square, predicting in validation set: 
## [1] 0.9249123
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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
cat("R-square, predicting in entire data set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in entire data set: 
## [1] 0.9836013
```

#### c2. Importance of variables

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 6)  
```

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1                  TOCTON       1.452608        3722  1.823750618           309.524109
## 2               slope_pre       2.563824        2834  0.670736140           125.008948
## 3               slope_tmp       2.830000        2969  0.643031294            72.604068
## 4       slope_dep_vs_time       2.862000        3240  0.525940015            44.840074
## 5                     pre       3.536608        2529  0.486816083            59.415322
## 6                TOTN_dep       3.583216        3033  0.216890991            29.103849
## 7                     TON       3.168608        3063  0.310492660            16.714348
## 8                     tmp       3.968432        2532  0.312031692            38.238750
## 9             bare_sparse       6.049280        1283  0.067031536             7.632301
## 10 total_shrub_herbaceous       4.104608        2705  0.057651253            10.123707
## 11         catchment_area       4.071216        3012  0.015095675             7.432097
## 12             lake_water       4.163216        2786  0.037471561             8.871729
## 13             cultivated       7.734464         827  0.005922917             1.383209
## 14                    TOC       3.102000        3055  0.226344105            29.451228
## 15           total_forest       3.569216        2921  0.112632513            18.119892
## 16                  urban       4.887296        1663  0.061271886             6.016091
## 17                wetland       4.530000        2698 -0.001917096             6.383230
##    no_of_trees times_a_root      p_value
## 1          499          155 7.405579e-94
## 2          497          113 5.875989e-05
## 3          500           83 4.558010e-11
## 4          500           49 1.156287e-31
## 5          499           40 9.874397e-01
## 6          498           22 6.075957e-15
## 7          499           18 5.670864e-17
## 8          496           10 9.853176e-01
## 9          465            4 1.000000e+00
## 10         499            3 9.654433e-02
## 11         498            2 1.329763e-13
## 12         498            1 1.825535e-03
## 13         417            0 1.000000e+00
## 14         500            0 2.033338e-16
## 15         498            0 1.425788e-08
## 16         488            0 1.000000e+00
## 17         500            0 1.226051e-01
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

![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-7.png)<!-- -->








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
##       (Int)   bar_spr    ctc_are   lak_wtr        pre slp_dep_vs_tim  slp_pre slp_tmp      tmp
## 19898 3.914 -0.011260            -0.019810 -0.0004094      -0.009783           -29.43 -0.05925
## 19866 3.923 -0.011540            -0.020120 -0.0004070                          -29.48 -0.06687
## 19642 3.907 -0.011040            -0.022760 -0.0004714      -0.011090           -26.99         
## 19900 3.944 -0.011220 -0.0003439 -0.020810 -0.0004225      -0.009957           -29.19 -0.05543
## 97458 1.570  0.011670                      -0.0004880      -0.011300           -25.85         
## 52410 3.840 -0.010920            -0.022470 -0.0004803      -0.009384           -26.32         
## 19644 3.940 -0.011010 -0.0003761 -0.023640 -0.0004813      -0.011190           -26.90         
## 52378 3.834 -0.011130            -0.023070 -0.0004873                          -25.94         
## 52634 3.857 -0.011340            -0.020390 -0.0004278                          -28.37 -0.05426
## 97713 2.499                                -0.0004012      -0.010770           -30.60 -0.06306
## 19868 3.952 -0.011510 -0.0003346 -0.021100 -0.0004197                          -29.25 -0.06328
## 3482  3.721 -0.010410            -0.018430 -0.0004060                          -28.20 -0.10680
## 52666 3.861 -0.011130            -0.020060 -0.0004263      -0.008664           -28.52 -0.04971
## 97714 1.816  0.009138                      -0.0004371      -0.010280           -27.89 -0.04693
## 85434 3.904 -0.010880            -0.019430 -0.0004210      -0.009220           -29.67 -0.05473
## 85178 3.896 -0.010620            -0.022050 -0.0004794      -0.010310           -27.49         
## 85402 3.911 -0.011090            -0.019680 -0.0004202                          -29.74 -0.06125
## 97460 1.546  0.012230 -0.0003099           -0.0004946      -0.011320           -25.88         
## 52412 3.878 -0.010910 -0.0003172 -0.023260 -0.0004873      -0.009729           -26.35         
## 97426 1.507  0.012700                      -0.0004853                          -26.13         
## 97682 1.798  0.009680                      -0.0004269                          -28.45 -0.05414
## 97681 2.522                                -0.0003882                          -31.35 -0.07164
## 52442 3.850 -0.011360            -0.023490 -0.0005089                0.013030  -25.77         
## 19610 3.915 -0.011330            -0.023560 -0.0004777                          -26.69         
## 85180 3.928 -0.010620 -0.0003581 -0.022940 -0.0004884      -0.010460           -27.37         
## 97465 2.699                      -0.010100 -0.0004835      -0.011480           -26.57         
## 19930 3.940 -0.011750            -0.020500 -0.0004245                0.011030  -29.39 -0.06706
## 52380 3.870 -0.011130 -0.0002976 -0.023830 -0.0004941                          -25.95         
## 19962 3.928 -0.011440            -0.020110 -0.0004224      -0.009020 0.008246  -29.37 -0.05999
## 52698 3.872 -0.011570            -0.020840 -0.0004499                0.012700  -28.16 -0.05340
## 23994 3.938 -0.012080            -0.019930 -0.0004229      -0.011090           -28.89 -0.05777
## 19612 3.948 -0.011300 -0.0003707 -0.024440 -0.0004876                          -26.60         
## 97721 2.696                      -0.007557 -0.0004286      -0.010340           -28.65 -0.05135
## 85146 3.902 -0.010820            -0.022680 -0.0004865                          -27.28         
## 85436 3.934 -0.010870 -0.0003309 -0.020420 -0.0004329      -0.009423           -29.42 -0.05134
## 97522 1.552  0.011580                      -0.0005116      -0.010730 0.011380  -25.54         
## 36250 3.651 -0.010160            -0.018540 -0.0004240                          -27.14 -0.09852
## 52636 3.891 -0.011340 -0.0002843 -0.021180 -0.0004358                          -28.32 -0.05287
## 3484  3.738 -0.010320 -0.0003005 -0.019220 -0.0004173                          -27.92 -0.10550
## 97457 2.414                                -0.0004620      -0.012490           -28.69         
## 28090 3.810 -0.010770            -0.018750 -0.0004190      -0.010950           -28.89 -0.05843
## 85404 3.940 -0.011080 -0.0003203 -0.020640 -0.0004317                          -29.51 -0.05811
## 52668 3.897 -0.011120 -0.0003038 -0.020900 -0.0004349      -0.009018           -28.47 -0.04804
## 69018 3.706 -0.009958            -0.017970 -0.0004186                          -28.42 -0.10220
## 52474 3.852 -0.011120            -0.022870 -0.0004980      -0.008320 0.010150  -26.15         
##           TOCT      TON   ttl_frs ttl_shr_hrb    TOT_dep      urb      wtl df   logLik  AICc
## 19898 -0.07082 0.001801                       -0.0007827                   11 -293.977 611.0
## 19866 -0.07134 0.001911                       -0.0004395                   10 -295.234 611.3
## 19642 -0.07428 0.001659                       -0.0011030                   10 -295.285 611.4
## 19900 -0.07081 0.001778                       -0.0008135                   12 -293.132 611.5
## 97458 -0.07387 0.001756  0.022130    0.023250 -0.0010650          0.029370 12 -293.144 611.5
## 52410 -0.07381 0.001959                       -0.0010220 -0.06979          11 -294.260 611.5
## 19644 -0.07402 0.001643                       -0.0011140                   11 -294.277 611.6
## 52378 -0.07462 0.002106                       -0.0007223 -0.08382          10 -295.391 611.6
## 52634 -0.07150 0.002151                       -0.0004737 -0.06580          11 -294.348 611.7
## 97713 -0.06991 0.001953  0.012820    0.014390 -0.0008106          0.019570 12 -293.339 611.9
## 19868 -0.07134 0.001891                       -0.0004635                   11 -294.441 611.9
## 3482  -0.06831 0.001813                                                     9 -296.632 612.0
## 52666 -0.07101 0.002012                       -0.0007717 -0.05436          12 -293.384 612.0
## 97714 -0.07116 0.001862  0.019840    0.020860 -0.0008171          0.026120 13 -292.340 612.1
## 85434 -0.07114 0.001731                       -0.0007713          0.007368 12 -293.460 612.1
## 85178 -0.07435 0.001589                       -0.0010610          0.008672 11 -294.565 612.2
## 85402 -0.07167 0.001825                       -0.0004490          0.008339 11 -294.572 612.2
## 97460 -0.07370 0.001743  0.022740    0.023710 -0.0010750          0.029450 13 -292.448 612.3
## 52412 -0.07366 0.001900                       -0.0010440 -0.05908          12 -293.562 612.3
## 97426 -0.07573 0.001749  0.023920    0.023700 -0.0007174          0.032100 11 -294.658 612.3
## 97682 -0.07240 0.001872  0.021100    0.020900 -0.0004674          0.028060 12 -293.583 612.4
## 97681 -0.07114 0.001969  0.013700    0.014030 -0.0004427          0.021200 11 -294.696 612.4
## 52442 -0.07522 0.002168                       -0.0007588 -0.08935          11 -294.732 612.5
## 19610 -0.07539 0.001765                       -0.0007543                    9 -296.911 612.5
## 85180 -0.07409 0.001579                       -0.0010740          0.008080 12 -293.649 612.5
## 97465 -0.07383 0.001722  0.010930    0.012220 -0.0010900          0.018940 12 -293.652 612.5
## 19930 -0.07188 0.001945                       -0.0004713                   11 -294.756 612.5
## 52380 -0.07451 0.002055                       -0.0007322 -0.07426          11 -294.782 612.6
## 19962 -0.07127 0.001835                       -0.0007797                   12 -293.716 612.7
## 52698 -0.07213 0.002211                       -0.0005132 -0.07148          12 -293.718 612.7
## 23994 -0.06996 0.001895 -0.001508             -0.0008169                   12 -293.755 612.7
## 19612 -0.07514 0.001750                       -0.0007621                   10 -295.943 612.7
## 97721 -0.07087 0.001848  0.011120    0.012270 -0.0008140          0.017890 13 -292.673 612.8
## 85146 -0.07538 0.001676                       -0.0007340          0.009949 10 -295.962 612.8
## 85436 -0.07111 0.001713                       -0.0008017          0.006902 13 -292.676 612.8
## 97522 -0.07419 0.001837  0.022030    0.023700 -0.0010750          0.030040 13 -292.676 612.8
## 36250 -0.06824 0.002014                                  -0.05709          10 -295.966 612.8
## 52636 -0.07147 0.002102                       -0.0004896 -0.05713          12 -293.788 612.8
## 3484  -0.06816 0.001789                                                    10 -295.996 612.8
## 97457 -0.07343 0.001832  0.013330    0.015280 -0.0011760          0.021710 11 -294.926 612.9
## 28090 -0.07009 0.001894              0.001239 -0.0008135                   12 -293.834 612.9
## 85404 -0.07165 0.001809                       -0.0004715          0.007908 12 -293.844 612.9
## 52668 -0.07097 0.001954                       -0.0008009 -0.04463          13 -292.741 612.9
## 69018 -0.06856 0.001728                                           0.007947 10 -296.036 612.9
## 52474 -0.07437 0.002024                       -0.0010170 -0.07569          12 -293.872 613.0
##       delta weight
## 19898  0.00  0.042
## 19866  0.34  0.035
## 19642  0.44  0.034
## 19900  0.50  0.033
## 97458  0.53  0.032
## 52410  0.57  0.032
## 19644  0.60  0.031
## 52378  0.65  0.030
## 52634  0.74  0.029
## 97713  0.92  0.026
## 19868  0.93  0.026
## 3482   0.98  0.026
## 52666  1.01  0.025
## 97714  1.13  0.024
## 85434  1.16  0.023
## 85178  1.18  0.023
## 85402  1.19  0.023
## 97460  1.34  0.021
## 52412  1.36  0.021
## 97426  1.36  0.021
## 97682  1.40  0.021
## 97681  1.44  0.020
## 52442  1.51  0.020
## 19610  1.54  0.019
## 85180  1.54  0.019
## 97465  1.54  0.019
## 19930  1.56  0.019
## 52380  1.61  0.019
## 19962  1.67  0.018
## 52698  1.67  0.018
## 23994  1.75  0.017
## 19612  1.76  0.017
## 97721  1.79  0.017
## 85146  1.80  0.017
## 85436  1.80  0.017
## 97522  1.80  0.017
## 36250  1.80  0.017
## 52636  1.81  0.017
## 3484   1.86  0.016
## 97457  1.90  0.016
## 28090  1.91  0.016
## 85404  1.92  0.016
## 52668  1.93  0.016
## 69018  1.94  0.016
## 52474  1.98  0.016
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

![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](167b2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-2.png)<!-- -->








