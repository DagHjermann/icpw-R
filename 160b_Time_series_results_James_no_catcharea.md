---
title: "160b Analyse NO3 decline - excl. catchment_area"
author: "DHJ"
date: "8 5 2020"
output: 
  html_document:
    toc: true    
    toc_float: true
    keep_md: true
params:
  document_title: 
    value: '160x Analyse NO3 decline - test run'
  text_dataset: 
    value: 'Data with slope_dep_vs_time, NO3, and TOTN_dep'
  selected_vars: 
    value: 'no3_decline, slope_dep_vs_time, longitude, latitude, NO3, TOTN_dep'
  logistic_formula: 
    value: 'no3_decline ~ slope_dep_vs_time + NO3 + TOTN_dep'

---




**Analysis of NO3 decrease (categorical, "decrease or not"), based on James' trend results**  
  
**Dataset: all variables except catchment area (but including TOC)**   

* Response variable: 'Significant /NO3 decline' (locations with signif. *increase* are *not* excluded)  
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
  
Technical details: This html file was rendered with `160parm_run_markdown.R` which runs the script `160parm_Time_series_results_James.Rmd` with different inputs, resulting in html files 160a, 160b and 160c.    

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
```


## 2. Data

### James' trends and medians     

```r
#
# Regression results
#
folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_no3"
file <- "trends_1992-2016_no3_results.csv"
fn <- paste0(folder, "/", file)

reg_no3 <- read.csv(fn, encoding = "UTF-8")
cat("Regression results:", sQuote(file), ",n =", nrow(reg_no3), "\n\n")

# Station metadata

# THESE WILL BE ADDED TOGETHER WITH LAND COVER  

# file <- "trends_1992-2016_no3_stations.csv"
# fn <- paste0(folder, "/", file)
# 
# df_metadata <- read.csv(fn, encoding = "UTF-8")
# cat("Regression result metadata:", sQuote(file), ",n =", nrow(df_metadata), "\n\n")
# 
# cat("Countries with trends: \n")
# xtabs(~country, df_metadata)  

#
# Medians NO3
#

if (FALSE){

  # OLD: 2012-2016 medians NO3  

  cat("--------------------------------------------------------------------------\n")
  folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/medians_2012-2016"
  file <- "medians_2012-2016_no3.csv"
  fn <- paste0(folder, "/", file)
  # data:
  medians_no3 <- read.csv(fn, encoding = "UTF-8")
  cat("Medians NO3:", sQuote(file), ",n =", nrow(medians_no3), "\n\n")
  file <- "medians_2012-2016_no3_stations.csv"
  fn <- paste0(folder, "/", file)
  # metadata:
  medians_no3_st <- read.csv(fn, encoding = "UTF-8")
  cat("Median metadata (not used):", sQuote(file), "n =", nrow(medians_no3_st), "\n\n")
  # stats:
  cat("Countries with medians: \n")
  xtabs(~country, medians_no3_st)  
  
  #
  #  OLD: 2012-2016 Medians TOC
  #
  cat("--------------------------------------------------------------------------\n")
  
  file <- "medians_2012-2016_toc_totn_no3_nh4.csv"
  fn <- paste0(folder, "/", file)
  
  medians_toc <- read.csv(fn, encoding = "UTF-8")
  cat("Medians TOC:", sQuote(file), ",n =", nrow(medians_toc), "\n\n")
  
  cat("Countries with medians: \n")
  xtabs(~country, medians_no3_st)  
  
}
```

```
## Regression results: 'trends_1992-2016_no3_results.csv' ,n = 3176
```
### Start 'dat'  
With slope regression data  
* Make one line per station  

```r
# table(reg_no3$variable)

# Slope 
df1 <- reg_no3 %>%
  filter(variable %in% c("NO3-N_µg/l N", "TOC/TON")) %>%
  select(station_id, variable, sen_slp) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "sen_slp") %>%
  rename(slope_no3_vs_time = `NO3-N_µg/l N`, 
         slope_tocton_vs_time = `TOC/TON`)
  
# Slope p-value
df2 <- reg_no3 %>%
  filter(variable %in% c("NO3-N_µg/l N", "TOC/TON")) %>%
  select(station_id, variable, mk_p_val) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "mk_p_val") %>%
  rename(p_no3_vs_time = `NO3-N_µg/l N`, 
         p_tocton_vs_time = `TOC/TON`)

# Medians
df3 <- reg_no3 %>%
  filter(variable %in% c("NO3-N_µg/l N", "TOC_mg C/l")) %>%
  select(station_id, variable, median) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "median") %>%
  rename(NO3 = `NO3-N_µg/l N`, 
         TOC = `TOC_mg C/l`)

cat("\n")
cat("df1, n =", nrow(df1), "\n")
cat("df2, n =", nrow(df2), "\n")
cat("df3, n =", nrow(df3), "\n")

dat <- df1 %>%
  full_join(df2, by = "station_id") %>%
  full_join(df3, by = "station_id")

cat("dat, n =", nrow(dat), "\n")
```

```
## 
## df1, n = 498 
## df2, n = 498 
## df3, n = 498 
## dat, n = 498
```


```r
# dat
# 
# str <- "slope_no3_vs_time ~ p_no3_vs_time"
# plot(as.formula(str), data = dat)
# lm(as.formula(str), data = dat)
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
cat("dat, n =", nrow(dat), "\n")

dat <- dat %>% 
  left_join(df_deposition %>% 
              select(station_id, median, sen_slp, mk_p_val) %>%
              rename(TOTN_dep = median,
                     slope_dep_vs_time = sen_slp,
                     p_dep_vs_time = mk_p_val),
                 by = "station_id")

cat("dat, n =", nrow(dat), "\n")

# names(dat)
```

```
## dat, n = 498 
## dat, n = 498
```
### Add medians and station metadata   

```r
# dat <- dat %>%
#   left_join(df_metadata, by = "station_id")

cat("dat, n =", nrow(dat), "\n")

# Simplify names by removing units
# names(dat)
# names(dat) <- sub(".N_µg.l.N", "", names(dat))
# names(dat) <- sub("_mg.C.l", "", names(dat))
# names(dat) <- sub("_µg.l.P", "", names(dat))

cat("\nVariable names: \n")
names(dat)
```

```
## dat, n = 498 
## 
## Variable names: 
##  [1] "station_id"           "slope_no3_vs_time"    "slope_tocton_vs_time" "p_no3_vs_time"       
##  [5] "p_tocton_vs_time"     "NO3"                  "TOC"                  "TOTN_dep"            
##  [9] "slope_dep_vs_time"    "p_dep_vs_time"
```

### Add climate and deposition medians 

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
## dat, n = 498
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
  select(-c(bare_rock, sparsely_vegetated, glacier, deciduous, mixed_forest, lake, water_ex_lake))
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
## dat, n = 498
```



## 3. Plot slopes    


```r
ggplot(dat, aes(slope_dep_vs_time, slope_no3_vs_time)) + 
  geom_point(data = dat %>% filter(p_no3_vs_time < 0.05), size = rel(2)) +
  geom_point(aes(color = country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) 
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggplot(dat, aes(slope_dep_vs_time, slope_no3_vs_time,
                color = (p_no3_vs_time < 0.05))) + 
  geom_point() +
  facet_wrap(vars(country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(title = "A selection of countries")
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
dat %>%
  filter(!country %in% c("Latvia","Ireland","Italy","Netherlands")) %>%
  ggplot(aes(slope_dep_vs_time, slope_no3_vs_time,
             color = (p_no3_vs_time < 0.05))) + 
  geom_point() +
  facet_wrap(vars(country)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  labs(title = "A selection of countries") + 
  ylim(-50, 25)
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-12-3.png)<!-- -->


## 4. Select data   
* Select variables to use, and thereby also cases  

```r
get_df_no3_decline <- function(data, variable_string){
  variable_string <- gsub(" ", "", variable_string)
  variables <- strsplit(variable_string, split = ",")[[1]]
  # Data for analyses
  df <- data %>%
    mutate(
      no3_decline = case_when(
        slope_no3_vs_time < 0 & p_no3_vs_time <= 0.05 ~ 1,
        TRUE ~ 0)
    )
  df[variables]
}

cat("-------------------------------------------------------------\n")
cat("Variables: \n")
cat(params$selected_vars)
cat("\n-------------------------------------------------------------\n")

df_analysis <- get_df_no3_decline(dat, params$selected_vars)  

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
## no3_decline,TOC,slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude,pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous,wetland, lake_water, bare_sparse
## -------------------------------------------------------------
## Number of missing values per variable: 
##            no3_decline                    TOC      slope_dep_vs_time                    NO3               TOTN_dep 
##                      0                     33                      0                      0                      0 
##               latitude              longitude               altitude                    pre                    tmp 
##                      0                      0                      2                      0                      0 
##                  urban             cultivated             coniferous            decid_mixed total_shrub_herbaceous 
##                     16                     16                     22                     22                     16 
##                wetland             lake_water            bare_sparse 
##                     16                     16                     16 
## 
## Number of complete observations: 
## complete
## FALSE  TRUE 
##    48   450 
## 
## 
## Number of complete observations by country: 
##                 complete
##                  FALSE TRUE
##   Canada             0  114
##   Czech Republic     1    7
##   Estonia            1    0
##   Finland            0   26
##   Germany            5   18
##   Ireland            3    0
##   Italy              6    0
##   Latvia             3    0
##   Netherlands        1    2
##   Norway             0   83
##   Poland             0    6
##   Slovakia           0   12
##   Sweden             6   86
##   Switzerland        6    0
##   United Kingdom     0   21
##   United States     16   75
## 
## 
## Original data: n = 498 
## Analysis: n = 450
```






## 5. Tree and forest classification


### Split into training and validation data

```r
set.seed(123)

x <- runif(nrow(df_analysis))
train <- ifelse(x < 0.9, TRUE, FALSE)

train_set <- df_analysis[train,]  %>% 
  mutate(no3_decline_f = factor(no3_decline)) %>% select(-no3_decline, -longitude, - latitude) %>%
  as.data.frame()
valid_set <- df_analysis[!train,] %>% 
  mutate(no3_decline_f = factor(no3_decline)) %>% select(-no3_decline, -longitude, - latitude) %>%
  as.data.frame()
```


### a. Tree classification using 'party'   

```r
(ct = ctree(no3_decline_f ~ ., data = train_set))

plot(ct, main="Conditional Inference Tree")
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
cat("\n\n")
cat("Table of prediction errors \n")
table(predict(ct), train_set$no3_decline_f)
cat("\n\n")

cat("Classification of training set \n")
tr.pred = predict(ct, newdata = valid_set, type="prob")
colnames(tr.pred) <- c("P0", "P1")
# tr.pred <- tr.pred %>% map_dfr(~data.frame(P0 = .[1], P1 = .[2]))
table(tr.pred[,"P1"] > 0.5, valid_set$no3_decline_f)
```

```
## 
## Model formula:
## no3_decline_f ~ TOC + slope_dep_vs_time + NO3 + TOTN_dep + altitude + 
##     pre + tmp + urban + cultivated + coniferous + decid_mixed + 
##     total_shrub_herbaceous + wetland + lake_water + bare_sparse
## 
## Fitted party:
## [1] root
## |   [2] altitude <= 227
## |   |   [3] NO3 <= 63.5
## |   |   |   [4] tmp <= 1.05: 1 (n = 8, err = 37.5%)
## |   |   |   [5] tmp > 1.05: 0 (n = 138, err = 8.0%)
## |   |   [6] NO3 > 63.5: 1 (n = 33, err = 48.5%)
## |   [7] altitude > 227
## |   |   [8] decid_mixed <= 70.405
## |   |   |   [9] bare_sparse <= 23.181: 1 (n = 167, err = 43.7%)
## |   |   |   [10] bare_sparse > 23.181: 1 (n = 28, err = 3.6%)
## |   |   [11] decid_mixed > 70.405: 0 (n = 32, err = 18.8%)
## 
## Number of inner nodes:    5
## Number of terminal nodes: 6
## 
## 
## Table of prediction errors 
##    
##       0   1
##   0 153  17
##   1  93 143
## 
## 
## Classification of training set 
##        
##          0  1
##   FALSE 12  4
##   TRUE  11 17
```

### b. Evtree (Evolutionary Learning)   

```r
ev.raw = evtree(no3_decline_f ~ ., data = train_set)

plot(ev.raw)
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
cat("Predicted in training data: \n")
table(predict(ev.raw), train_set$no3_decline_f)

cat("\n\nPrediction errors in training data: \n")
1-mean(predict(ev.raw) == train_set$no3_decline_f)
```

```
## Predicted in training data: 
##    
##       0   1
##   0 204  29
##   1  42 131
## 
## 
## Prediction errors in training data: 
## [1] 0.1748768
```


### c. Random forest  
* *For results/interpretation, see separate document '160_randomforest_James_data.html'*  
* Model called 'model1'

```r
model1 <- randomForest(no3_decline_f ~ ., 
                       data = train_set, 
                       mtry = 5,
                       importance = TRUE)

model1
```

```
## 
## Call:
##  randomForest(formula = no3_decline_f ~ ., data = train_set, mtry = 5,      importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 24.88%
## Confusion matrix:
##     0   1 class.error
## 0 196  50    0.203252
## 1  51 109    0.318750
```


#### c1. Predict on training data

```r
# Predicting on train set
pred_valid <- predict(model1, valid_set, type = "class")
# Checking classification accuracy
table(pred_valid, valid_set$no3_decline_f)  
```

```
##           
## pred_valid  0  1
##          0 19  8
##          1  4 13
```

#### c2. Importance of variables

```r
# Calculation
importance <- measure_importance(model1)
```




```r
plot_multi_way_importance(importance, size_measure = "no_of_nodes", no_of_labels = 12)  
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
plot_multi_way_importance(importance, x_measure = "accuracy_decrease", y_measure = "gini_decrease", 
                          size_measure = "p_value", no_of_labels = 12)
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-19-2.png)<!-- -->



#### c3. Random forest, show partial effects  


```r
# Which variables to include:
variables_for_plot <- importance %>%
  mutate(variable = levels(variable)[as.numeric(variable)]) %>%
  arrange(desc(gini_decrease)) %>%
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

#
# Two extra plots:
#
i <- i + 1
varno1 <- "slope_dep_vs_time"
varno2 <- "TOTN_dep"
plotdata[[i]] <- model1 %>%
  partial(pred.var = c(varno1, varno2), chull = TRUE, progress = "text",
          which.class = "1", prob = TRUE)

if ("TOC" %in% names(train)){
  i <- i + 1
  varno1 <- "TOC"
  varno2 <- "NO3"
  plotdata[[i]] <- model1 %>%
    partial(pred.var = c(varno1, varno2), chull = TRUE, progress = "text",
            which.class = "1", prob = TRUE)
}

if (substr(params$document_title,1,4) == "160b"){
  i <- i + 1
  varno1 <- "altitude"
  varno2 <- "decid_mixed"
  plotdata[[i]] <- model1 %>%
    partial(pred.var = c(varno1, varno2), chull = TRUE, progress = "text",
            which.class = "1", prob = TRUE)
}


# saveRDS(plotdata, "Data/160a_plotdata.Rmd")

# plotdata <- readRDS("Data/160a_plotdata.Rmd")
```



```r
# Plot the plots 
for (i in 1:length(plotdata)){
  autoplot(plotdata[[i]], contour = TRUE, legend.title = "Probability\nNO3 decline") %>%
    print() 
}
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-2.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-3.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-4.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-5.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-6.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-7.png)<!-- -->![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-20-8.png)<!-- -->

```r
if (FALSE) {
  
  # Put on same color scale
  
  # Find range of predicted values for each graph
  ranges <- plotdata %>% purrr::map_dfc(~range(.$yhat))
  
  # use range of all the ranges
  for (i in 1:length(plotdata)){
    gg <- autoplot(plotdata[[i]], contour = TRUE, legend.title = "Probability\nNO3 decline") +
      scale_fill_viridis_c(limits = range(ranges))
    print(gg) 
  }
  
}
```


## 6. Logistic regression      

```r
#   no3_decline ~ as.formula(params$logistic_formula),

fm <- glm(
  as.formula(params$logistic_formula),
  data = df_analysis, 
  family = "binomial",
  na.action = "na.fail")

dd1b <- dredge(fm)                       # only once
```

```
## Fixed term is "(Intercept)"
```

```r
saveRDS(dd1b, "Data/160_all_dd1b.rds")    # save it as it takes a couple of minutes
# dd1b <- readRDS("Data/160_all_dd1b.rds")

# subset(dd1b, delta < 1)
subset(dd1b, delta < 2)

cat("\n\nR2: \n")
dd1b_mod1 <- get.models(dd1b, 1)[[1]]  
# summary(dd1b_mod1)  

par(mfrow = c(2,3), mar = c(4,5,2,1), oma = c(0,0,2,0))
visreg(dd1b_mod1, scale = "response")
```

![](160b_Time_series_results_James_no_catcharea_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```
## Global model call: glm(formula = as.formula(params$logistic_formula), family = "binomial", 
##     data = df_analysis, na.action = "na.fail")
## ---
## Model selection table 
##       (Int)      alt      cnf  dcd_mxd   lak_wtr       NO3 slp_dep_vs_tim     tmp      TOC    TOT_dep     wtl df
## 616  0.4891 0.001497 -0.02122 -0.02908                           -0.05947 -0.2440                     0.03970  7
## 744  0.5987 0.001345 -0.01797 -0.02782                           -0.05717 -0.2312 -0.04344            0.04447  8
## 872  0.4220 0.001674 -0.02127 -0.02868                           -0.07116 -0.2008          -0.0006572 0.03857  8
## 1000 0.5351 0.001532 -0.01760 -0.02715                           -0.07163 -0.1773 -0.04959 -0.0008114 0.04371  9
## 888  0.5316 0.001563 -0.02159 -0.02806           0.0011280       -0.07468 -0.1821          -0.0011850 0.03971  9
## 632  0.5638 0.001381 -0.02128 -0.02890           0.0005365       -0.05643 -0.2510                     0.04070  8
## 624  0.5926 0.001477 -0.02161 -0.02926 -0.004047                 -0.05922 -0.2468                     0.03910  8
##        logLik  AICc delta weight
## 616  -235.590 485.4  0.00  0.241
## 744  -234.822 486.0  0.54  0.184
## 872  -235.172 486.7  1.24  0.130
## 1000 -234.201 486.8  1.38  0.121
## 888  -234.229 486.9  1.43  0.118
## 632  -235.297 486.9  1.49  0.115
## 624  -235.530 487.4  1.95  0.091
## Models ranked by AICc(x) 
## 
## 
## R2:
```


