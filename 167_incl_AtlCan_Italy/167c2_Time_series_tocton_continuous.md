---
title: "167c2 Analyse TOC/TON change"
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
  
**Dataset: Basis + TOC + TON slopes**   

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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


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
## slope_tocton_vs_time,catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp,urban, cultivated, total_forest, total_shrub_herbaceous,wetland, lake_water, bare_sparse, slope_toc_vs_time, slope_ton_vs_time
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
## 17      slope_toc_vs_time       0
## 18      slope_ton_vs_time       0
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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## 
## Model formula:
## slope_tocton_vs_time ~ catchment_area + slope_pre + slope_tmp + 
##     slope_dep_vs_time + TOCTON + TOTN_dep + pre + tmp + urban + 
##     cultivated + total_forest + total_shrub_herbaceous + wetland + 
##     lake_water + bare_sparse + slope_toc_vs_time + slope_ton_vs_time
## 
## Fitted party:
## [1] root
## |   [2] TOCTON <= 45.61404
## |   |   [3] slope_ton_vs_time <= 4.34962
## |   |   |   [4] slope_ton_vs_time <= -0.3
## |   |   |   |   [5] slope_tmp <= 0.03258: 0.945 (n = 10, err = 2.5)
## |   |   |   |   [6] slope_tmp > 0.03258
## |   |   |   |   |   [7] slope_toc_vs_time <= 0.12375: 0.377 (n = 39, err = 0.9)
## |   |   |   |   |   [8] slope_toc_vs_time > 0.12375: 0.586 (n = 12, err = 0.4)
## |   |   |   [9] slope_ton_vs_time > -0.3
## |   |   |   |   [10] slope_toc_vs_time <= 0.06667
## |   |   |   |   |   [11] slope_ton_vs_time <= 1.73684
## |   |   |   |   |   |   [12] slope_toc_vs_time <= 0.01222: -0.061 (n = 25, err = 0.2)
## |   |   |   |   |   |   [13] slope_toc_vs_time > 0.01222: 0.057 (n = 24, err = 0.3)
## |   |   |   |   |   [14] slope_ton_vs_time > 1.73684: -0.214 (n = 13, err = 0.8)
## |   |   |   |   [15] slope_toc_vs_time > 0.06667: 0.283 (n = 60, err = 1.4)
## |   |   [16] slope_ton_vs_time > 4.34962: -1.264 (n = 19, err = 39.6)
## |   [17] TOCTON > 45.61404
## |   |   [18] slope_ton_vs_time <= 10.5719
## |   |   |   [19] pre <= 1469: -2.668 (n = 11, err = 18.2)
## |   |   |   [20] pre > 1469: -3.908 (n = 16, err = 9.2)
## |   |   [21] slope_ton_vs_time > 10.5719: -5.246 (n = 14, err = 20.7)
## 
## Number of inner nodes:    10
## Number of terminal nodes: 11
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(tree_formula, data = train_set)
plot(ev.raw)
```

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# For classification:
# cat("Predicted in training data: \n")
# table(predict(ev.raw), train_set[[params$response_variable]])

# For continuous response:
plot(predict(ev.raw), train_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in training set")
```

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
plot(predict(ev.raw, newdata = valid_set), 
     valid_set[[params$response_variable]])
abline(0, 1, lty = "dashed")
mtext("Predictions in validation set")
```

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
cat("R-square, predicting in validation set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in training set: 
## [1] 0.9860287
## R-square, predicting in validation set: 
## [1] 0.9156423
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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
cat("R-square, predicting in entire data set: \n")
summary(lm(Observed ~ Predicted, data = df_plot))$r.sq
```

```
## R-square, predicting in entire data set: 
## [1] 0.9864204
```

#### c2. Importance of variables

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 6)  
```

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-22-2.png)<!-- -->

```r
importance %>%
  arrange(desc(times_a_root))
```

```
##                  variable mean_min_depth no_of_nodes mse_increase node_purity_increase
## 1                  TOCTON        1.63408        3668 1.4381414133          264.5293394
## 2       slope_ton_vs_time        1.56800        4511 1.2386784599          192.1615505
## 3               slope_pre        3.46816        2560 0.3535657771           78.0226551
## 4               slope_tmp        3.04004        2598 0.5132980699           62.8846959
## 5                     pre        3.74608        2479 0.3510365725           41.9876395
## 6       slope_dep_vs_time        3.03200        2908 0.4361900191           38.2037650
## 7                     tmp        4.02620        2419 0.1794013133           28.4247580
## 8                TOTN_dep        3.85604        2802 0.1609095072           19.4040679
## 9          catchment_area        4.27604        2922 0.0054984348            5.9264254
## 10            bare_sparse        6.32156        1197 0.0276052274            4.2656402
## 11             lake_water        4.10008        2765 0.0201437060            8.3960732
## 12      slope_toc_vs_time        2.96000        3668 0.0706764441           17.0924560
## 13 total_shrub_herbaceous        4.15212        2593 0.0339310079            7.2900846
## 14             cultivated        7.77488         709 0.0053404084            0.9773472
## 15           total_forest        3.54204        2770 0.0688687041           14.4908567
## 16                  urban        4.96916        1438 0.0273181787            4.4883123
## 17                wetland        4.52404        2564 0.0005944997            4.9275407
##    no_of_trees times_a_root       p_value
## 1          498          148  1.261914e-88
## 2          500          112 5.550308e-264
## 3          496           75  8.955074e-01
## 4          499           57  6.870128e-01
## 5          498           37  9.981782e-01
## 6          500           35  7.560811e-09
## 7          495           15  9.999827e-01
## 8          499           10  1.702771e-04
## 9          499            5  1.488868e-09
## 10         461            3  1.000000e+00
## 11         498            1  2.177329e-03
## 12         500            1  1.261914e-88
## 13         497            1  7.218607e-01
## 14         378            0  1.000000e+00
## 15         499            0  1.587693e-03
## 16         471            0  1.000000e+00
## 17         499            0  8.800528e-01
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

![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-1.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-2.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-3.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-4.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-5.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/5c3_plot_partial_effects2-6.png)<!-- -->








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
##         (Int)   bar_spr    ctc_are     clt  lak_wtr        pre slp_dep_vs_tim slp_tmp
## 36752  1.9430 -0.010050 -0.0005634 0.09384 -0.01336                            -7.073
## 36624  1.6140 -0.009657 -0.0005717 0.08511 -0.01667                                  
## 53136  2.0610 -0.010780 -0.0005810 0.09848 -0.01425                            -7.775
## 102288 1.9280 -0.009622 -0.0005553 0.09419 -0.01274                            -7.564
## 36768  2.0550 -0.009261 -0.0005671 0.09550 -0.01381 -0.0000897                 -7.890
## 114567 0.9125           -0.0005425 0.10640                                     -8.450
## 81799  0.9531           -0.0005632 0.08606                                     -9.302
## 36748  1.9340 -0.010380 -0.0004772         -0.01420                            -6.206
## 118672 2.0520 -0.010360 -0.0005734 0.09914 -0.01364                            -8.340
## 53152  2.1970 -0.009964 -0.0005867 0.10080 -0.01484 -0.0001001                 -8.752
## 36784  1.9790 -0.010340 -0.0005696 0.10010 -0.01362                  0.001926  -7.260
## 36620  1.6430 -0.010000 -0.0004917         -0.01707                                  
##        slp_toc_vs_tim slp_ton_vs_tim      tmp      TOC  ttl_frs ttl_shr_hrb    TOT_dep
## 36752           5.388        -0.1745 -0.07589 -0.05091                                
## 36624           5.448        -0.1801 -0.05105 -0.05235                                
## 53136           5.461        -0.1739 -0.05628 -0.05263                      -0.0002232
## 102288          5.352        -0.1730 -0.07565 -0.05111                                
## 36768           5.192        -0.1705 -0.06845 -0.05089                                
## 114567          5.391        -0.1723 -0.07998 -0.05094 0.010290     0.01053           
## 81799           5.191        -0.1687 -0.08544 -0.05102 0.009999     0.01094           
## 36748           5.334        -0.1718 -0.06898 -0.05232                                
## 118672          5.427        -0.1724 -0.05486 -0.05294                      -0.0002363
## 53152           5.249        -0.1694 -0.04618 -0.05275                      -0.0002437
## 36784           5.468        -0.1748 -0.07152 -0.05149                                
## 36620           5.392        -0.1771 -0.04750 -0.05348                                
##             urb      wtl df   logLik  AICc delta weight
## 36752  -0.07674          12 -218.317 461.9  0.00  0.159
## 36624  -0.08950          11 -219.841 462.7  0.86  0.103
## 53136  -0.07872          13 -217.679 462.8  0.93  0.100
## 102288 -0.06972 0.005797 13 -217.758 462.9  1.09  0.092
## 36768  -0.08505          13 -217.824 463.1  1.22  0.086
## 114567 -0.05796 0.016350 13 -217.919 463.3  1.41  0.078
## 81799           0.017910 12 -219.047 463.3  1.46  0.076
## 36748  -0.05460          11 -220.320 463.7  1.81  0.064
## 118672 -0.07134 0.006182 14 -217.043 463.7  1.89  0.062
## 53152  -0.08817          14 -217.068 463.8  1.94  0.060
## 36784  -0.08062          13 -218.186 463.8  1.95  0.060
## 36620  -0.06775          10 -221.491 463.8  1.98  0.059
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

![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-1.png)<!-- -->![](167c2_Time_series_tocton_continuous_files/figure-html/unnamed-chunk-27-2.png)<!-- -->








