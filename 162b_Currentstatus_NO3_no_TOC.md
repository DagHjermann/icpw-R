---
title: "162b Analyse NO3 medians 2012-2016 - without TOC + catchment area"
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
  logistic_formula: 
    value: 'log_median_no3 ~ TOTN_dep + slope_dep_vs_time + TOTN_dep:slope_dep_vs_time + log_median_toc + TOTN_dep:log_median_toc + tmp + pre + altitude + decid_mixed + bare_sparse + coniferous + catchment_area + lake_water + total_shrub_herbaceous'

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
library(mapview)
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
  select(station_id, `NO3.N_µg.l.N`, `TOTN_µg.l.N`, `TOC_mg.C.l`, TOC.TON) %>%
  rename(median_no3 = `NO3.N_µg.l.N`,
         median_totn = `TOTN_µg.l.N`,
         median_toc = `TOC_mg.C.l`,
         median_tocton = `TOC.TON`) %>%
  mutate(log_median_no3 = log10(median_no3 + 0.1),
         log_median_totn = log10(median_totn),
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

dat <- df1 %>%
  left_join(df2, by = "station_id")

cat("dat, n =", nrow(dat), "\n")
```

```
## 
## df1, n = 494 
## df2, n = 498 
## dat, n = 494
```

### Deposition trends 1992-2006 and median 2012-2016      

```r
fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/deposition/totn_dep_trends_icpw_stns.csv"  

# For trends
df_deposition <- read.csv(fn) %>% 
  filter(variable == "totn_mgNpm2" & !is.na(sen_slp)) %>%
  select(station_id, sen_slp, mk_p_val) %>%
              rename(slope_dep_vs_time = sen_slp,
                     p_dep_vs_time = mk_p_val) 

cat("n =", nrow(df_deposition), "\n")

# Deposition medians 2012-2016
fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Deposition data/Processed/dep_TOTN.csv"
df_deposition_medians <- read_csv(fn) %>%
  filter(year %in% 2012:2016) %>%
  group_by(station_id) %>%
  summarize(TOTN_dep = median(TOTN_dep))
```

```
## Parsed with column specification:
## cols(
##   station_id = col_double(),
##   year = col_double(),
##   TOTN_dep = col_double()
## )
```

```r
df_deposition <- df_deposition %>%
  left_join(df_deposition_medians)
```

```
## Joining, by = "station_id"
```

```r
cat("n =", nrow(df_deposition), "\n")

xtabs(~is.na(TOTN_dep), df_deposition)
```

```
## n = 556 
## n = 556 
## is.na(TOTN_dep)
## FALSE 
##   556
```

### Add deposition slope and medians to data  

```r
dat <- dat %>% 
  left_join(df_deposition,
                 by = "station_id")

cat("dat, n =", nrow(dat), "\n")

# names(dat)
```

```
## dat, n = 494
```



### Add climate medians   

```r
fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/climate/cru_climate_trends_icpw_stns.csv"
df_climate_mean <- read_csv(fn) %>%
  select(station_id, variable, median) %>%
  pivot_wider(names_from = "variable", values_from = "median")
```

```
## Parsed with column specification:
## cols(
##   station_id = col_double(),
##   variable = col_character(),
##   median = col_double(),
##   mk_p_val = col_double(),
##   mk_trend = col_character(),
##   sen_slp = col_double(),
##   sen_incpt = col_double(),
##   sen_trend = col_character()
## )
```

```r
# names(df_climate_mean)

# Add
dat <- dat %>%
  left_join(df_climate_mean, by = "station_id")

cat("dat, n =", nrow(dat), "\n")
```

```
## dat, n = 494
```




### Combine land cover types   
* Data including UK read using script 159  
* bare_sparse = bare_rock + sparsely_vegetated + glacier   
* Select: coniferous, deciduous, lake, mixed_forest, wetland, bare_sparse   

```r
df_landcover3 <- readRDS("Data/159_df_landcover3.rds")

df_landcover3 <- df_landcover3 %>%
  mutate(bare_sparse = bare_rock + sparsely_vegetated + glacier,
         decid_mixed = deciduous + mixed_forest,
         lake_water = lake + water_ex_lake) %>%
  select(-c(bare_rock, sparsely_vegetated, glacier, 
            deciduous, mixed_forest, 
            lake, water_ex_lake))
```


### Add land cover columns to main data    

```r
dat <- left_join(dat, 
                 df_landcover3, 
                 by = "station_id"
)

cat("dat, n =", nrow(dat), "\n")
```

```
## dat, n = 494
```



## 3. Plot data      


```r
gg <- ggplot(dat, aes(TOTN_dep, log_median_no3)) + 
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
  
gg
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## 4. Select data   

### a. Selection of variables    
* Select variables to use, and thereby also cases

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
## log_median_no3,slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
## Number of missing values per variable: 
##         log_median_no3      slope_dep_vs_time               TOTN_dep               latitude 
##                      0                      0                      0                      0 
##              longitude               altitude                    pre                    tmp 
##                      0                     11                      0                      0 
##                  urban             cultivated             coniferous            decid_mixed 
##                     18                     18                     27                     27 
## total_shrub_herbaceous                wetland             lake_water            bare_sparse 
##                     18                     18                     18                     18 
## 
## Number of complete observations: 
## complete
## FALSE  TRUE 
##    37   457 
## 
## 
## Number of complete observations by country: 
##                 complete
##                  FALSE TRUE
##   Canada             0  115
##   Czech Republic     0    8
##   Estonia            1    0
##   Finland            0   23
##   Germany            0    3
##   Ireland            9    2
##   Italy              0    4
##   Latvia             0    5
##   Moldova            2    0
##   Netherlands        0    3
##   Norway             0   83
##   Poland             0   10
##   Slovakia           0   12
##   Sweden             0   92
##   Switzerland        9    0
##   United Kingdom     0   22
##   United States     16   75
## 
## 
## Original data: n = 494 
## Analysis: n = 457
```


### b. Correlations   

```r
gg <- GGally::ggcorr(df_analysis, method = c("complete.obs", "kendall"), label = TRUE) # +
gg + theme(plot.margin = unit(c(.8, 2, .8, 2.5), "cm"))
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
# SHOULD also workaccording to ?element_rect (update ggplot2?)
# gg + theme(plot.margin = margin(.6, .5, .6, 1.7, "cm"))
```



## 5. Tree and forest classification


### Split into training and validation data

```r
set.seed(123)

x <- runif(nrow(df_analysis))
train <- ifelse(x < 0.9, TRUE, FALSE)

train_set <- df_analysis[train,]  %>% 
  select(-longitude, - latitude) %>%
  as.data.frame()
valid_set <- df_analysis[!train,] %>% 
  select(-longitude, - latitude) %>%
  as.data.frame()

# plot(train_set)
```


### a. Tree classification using 'party'   

```r
# train_set$X <- 10^train_set$log_median_no3
# (ct = ctree(X ~ ., data = train_set))

(ct = ctree(as.formula(params$tree_formula), 
            data = train_set))

plot(ct, main="Conditional Inference Tree")
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```
## 
## Model formula:
## log_median_no3 ~ slope_dep_vs_time + TOTN_dep + altitude + pre + 
##     tmp + urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous + 
##     wetland + lake_water + bare_sparse
## 
## Fitted party:
## [1] root
## |   [2] TOTN_dep <= 626.4585
## |   |   [3] slope_dep_vs_time <= -17.25314: 1.494 (n = 56, err = 18.8)
## |   |   [4] slope_dep_vs_time > -17.25314
## |   |   |   [5] pre <= 1226.09998
## |   |   |   |   [6] urban <= 0.001: 0.694 (n = 81, err = 28.7)
## |   |   |   |   [7] urban > 0.001
## |   |   |   |   |   [8] slope_dep_vs_time <= -8.22583: 1.355 (n = 24, err = 3.4)
## |   |   |   |   |   [9] slope_dep_vs_time > -8.22583: 0.871 (n = 10, err = 1.7)
## |   |   |   [10] pre > 1226.09998
## |   |   |   |   [11] slope_dep_vs_time <= -13.34755: 1.618 (n = 7, err = 0.5)
## |   |   |   |   [12] slope_dep_vs_time > -13.34755
## |   |   |   |   |   [13] slope_dep_vs_time <= -3.44335
## |   |   |   |   |   |   [14] coniferous <= 11.193
## |   |   |   |   |   |   |   [15] altitude <= 236
## |   |   |   |   |   |   |   |   [16] cultivated <= 2.491: 1.304 (n = 31, err = 0.0)
## |   |   |   |   |   |   |   |   [17] cultivated > 2.491: 1.328 (n = 7, err = 0.0)
## |   |   |   |   |   |   |   [18] altitude > 236: 1.430 (n = 7, err = 0.5)
## |   |   |   |   |   |   [19] coniferous > 11.193: 1.216 (n = 26, err = 0.5)
## |   |   |   |   |   [20] slope_dep_vs_time > -3.44335: 0.991 (n = 9, err = 0.8)
## |   [21] TOTN_dep > 626.4585
## |   |   [22] altitude <= 252
## |   |   |   [23] coniferous <= 70.029
## |   |   |   |   [24] cultivated <= 3.29
## |   |   |   |   |   [25] total_shrub_herbaceous <= 8.677: 1.742 (n = 27, err = 2.3)
## |   |   |   |   |   [26] total_shrub_herbaceous > 8.677: 2.268 (n = 9, err = 1.3)
## |   |   |   |   [27] cultivated > 3.29: 2.285 (n = 8, err = 2.5)
## |   |   |   [28] coniferous > 70.029: 1.260 (n = 18, err = 8.5)
## |   |   [29] altitude > 252
## |   |   |   [30] TOTN_dep <= 698.16864: 1.848 (n = 34, err = 19.2)
## |   |   |   [31] TOTN_dep > 698.16864: 2.217 (n = 59, err = 8.0)
## 
## Number of inner nodes:    15
## Number of terminal nodes: 16
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(as.formula(params$tree_formula), 
                data = train_set)

plot(ev.raw)
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


### c. Random forest  
* Model called 'model1'

```r
model1 <- randomForest(as.formula(params$tree_formula), 
                       data = train_set, 
                       mtry = 5,
                       importance = TRUE)

model1
```

```
## 
## Call:
##  randomForest(formula = as.formula(params$tree_formula), data = train_set,      mtry = 5, importance = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##           Mean of squared residuals: 0.2237839
##                     % Var explained: 55.04
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
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 12)  
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
plot_multi_way_importance(importance, x_measure = "mse_increase", y_measure = "node_purity_increase",
                          size_measure = "p_value", no_of_labels = 12)
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-18-2.png)<!-- -->



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
  strsplit(x, split = ";") %>%
  .[[1]] %>%
  purrr::map(~strsplit(., split = ",")[[1]])

for (plotvar in plotpairs){
  # print(plotvar)
  if (plotvar[1] %in% names(train_set) & plotvar[2] %in% names(train_set)){
    i <- i + 1
    plotdata[[i]] <- model1 %>%
      partial(pred.var = c(plotvar[1], plotvar[2]), chull = TRUE, progress = "text",
             which.class = "1", prob = TRUE)
  }
}
```



```r
# Plot the plots 
for (i in 1:max_number_of_plots){
  autoplot(plotdata[[i]], contour = TRUE, legend.title = "log(NO3)") %>%
    print()
}
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-1.png)<!-- -->![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-2.png)<!-- -->![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-3.png)<!-- -->![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-4.png)<!-- -->![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-5.png)<!-- -->![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-19-6.png)<!-- -->





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
##      (Int)       alt  bar_spr       cnf   dcd_mxd   lak_wtr slp_dep_vs_tim     tmp ttl_shr_hrb
## 2014 1.087 0.0002494          -0.006057 -0.004419 -0.010160       -0.01779 0.03679   -0.005208
## 2016 1.028 0.0002103 0.001743 -0.005406 -0.003729 -0.009788       -0.01760 0.03655   -0.004645
##        TOT_dep slp_dep_vs_tim:TOT_dep df   logLik  AICc delta weight
## 2014 0.0006846              8.730e-06 11 -385.011 792.6  0.00  0.693
## 2016 0.0007231              8.986e-06 12 -384.772 794.2  1.63  0.307
## Models ranked by AICc(x)
```


### Plots  

```r
# Pick model with lowest AICc
mod1 <- get.models(dredged_models, 1)[[1]]  

modelvars <- get_model_variables(mod1)

# Interactions: 3D plot 
# visreg2d(mod1, xvar = vars[1], yvar = vars[2], 
#          type = 'conditional', scale = "response") 

# Interactions: 2D plot 
modelvars$interaction_list %>% purrr::walk(
  ~visreg(mod1, .x[1], by = .x[2])
)
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
par(mfrow = c(2,3), mar = c(4,5,2,1), oma = c(0,0,2,0))
for (var in modelvars$additive_vars)
  visreg(mod1, var)  
```

![](162b_Currentstatus_NO3_no_TOC_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

```
## Conditions used in construction of plot
## coniferous: 19.312
## decid_mixed: 16.891
## lake_water: 11.96
## slope_dep_vs_time: -15.65393
## tmp: 5.458334
## total_shrub_herbaceous: 1.692
## TOTN_dep: 475.5
## Conditions used in construction of plot
## altitude: 253
## decid_mixed: 16.891
## lake_water: 11.96
## slope_dep_vs_time: -15.65393
## tmp: 5.458334
## total_shrub_herbaceous: 1.692
## TOTN_dep: 475.5
## Conditions used in construction of plot
## altitude: 253
## coniferous: 19.312
## lake_water: 11.96
## slope_dep_vs_time: -15.65393
## tmp: 5.458334
## total_shrub_herbaceous: 1.692
## TOTN_dep: 475.5
## Conditions used in construction of plot
## altitude: 253
## coniferous: 19.312
## decid_mixed: 16.891
## slope_dep_vs_time: -15.65393
## tmp: 5.458334
## total_shrub_herbaceous: 1.692
## TOTN_dep: 475.5
## Conditions used in construction of plot
## altitude: 253
## coniferous: 19.312
## decid_mixed: 16.891
## lake_water: 11.96
## slope_dep_vs_time: -15.65393
## total_shrub_herbaceous: 1.692
## TOTN_dep: 475.5
## Conditions used in construction of plot
## altitude: 253
## coniferous: 19.312
## decid_mixed: 16.891
## lake_water: 11.96
## slope_dep_vs_time: -15.65393
## tmp: 5.458334
## TOTN_dep: 475.5
```








