---
title: "159parm_get_landcover"
author: "DHJ"
date: "26 4 2021"
output: 
  html_document:
    keep_md: true
    toc: true  
    toc_float: true
---


## 1. Packages  

```r
library(readxl)
library(dplyr)

knitr::opts_chunk$set(results = 'hold') # collect the results from a chunk  
knitr::opts_chunk$set(out.width = "120")  
```


### Data source  

```r
localfiles <- TRUE  # if FALSE: reading from K  
```


## 2. Metadata - full station list  

```r
# Put in newer metadata (variable Continent has values, includes variable Group)
if (localfiles){
  fn <- "Data_landcover/all_icpw_sites_may_2020.xlsx"
} else {
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/all_icpw_sites_may_2020.xlsx"
}

df_meta_without_landcover <- read_excel(fn)
```


## 3. Land cover  

### Get land cover and catchment_area    
* Main land cover file has lines for UK data, but they are mostly empty  
* Read UK data separately and add to data  

```r
#
# Define names of land cover variables
# 
vars_landcover <- c("catchment_area",
                    "urban", "cultivated", "total_forest", "deciduous", "coniferous", 
                    "mixed_forest", "total_shrub_herbaceous", "grasslands", "heathlands", 
                    "transitional_woodland_shrub", "bare_rock", "sparsely_vegetated", 
                    "glacier", "wetland", "lake", "water_ex_lake", "other")

#
# MAIN LAND COVER FILE  
#

if (localfiles){
  fn <- "Data_landcover/ICPW_All_Stations_2020_land cover_2020_05_04.xlsx"
} else {
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/ICPW_All_Stations_2020_land cover_05_04.xlsx"
}

df_landcover1 <- read_excel(fn)
cat(nrow(df_landcover1), "lines of data read into df_landcover1 \n")
# names(df_landcover1); cat("\n")
```

```
## 556 lines of data read into df_landcover1
```


### Extra UK data

#### Read file  

```r
#
# EXTRA UK DATA  
#
# UK data 'df_landcover1_new'

if (localfiles){
  fn <- "Data_landcover/icpw_land_cover_United Kingdom_29-01-2021.xlsx"
} else {
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/Excel documents - received/icpw_land_cover_United Kingdom_29-01-2021.xlsx"
}

df_landcover1_new1 <- read_excel(fn, 
                               sheet = "uk", 
                               range = "A1:U23",    # NOTE: specific range! 
                               col_types = c("numeric", rep("text", 2), rep("numeric", 18))) %>%
  as.data.frame()
cat(nrow(df_landcover1_new1), "lines of UK data read into df_landcover1_new1 \n")
```

```
## 22 lines of UK data read into df_landcover1_new1
```


#### Remove lines from main data   

```r
#
# REMOVE UK LINES FROM MAIN DATA
#

# Existing data in 'df_landcover1'  
stations <- df_landcover1_new1$station_id
sel_to_remove <- df_landcover1$station_id %in% stations

# Check existing data - just have altitide, a few (erronous) catchment area, and no land cover: 
# unique(df_landcover1$station_id[sel_to_remove])
# df_landcover1[sel_to_remove,] %>% View()
# df_landcover1[sel_to_remove,] %>% select(-(station_code:longitude), -(altitude:region)) %>% View()

# Remove existing UK data:
df_landcover1b <- df_landcover1[!sel_to_remove,]
cat(sum(sel_to_remove), "lines of data removed from df_landcover1 \n")
cat(nrow(df_landcover1b), "lines of data in df_landcover1b \n")
```

```
## 22 lines of data removed from df_landcover1 
## 534 lines of data in df_landcover1b
```


#### Make data ready to be added     

```r
#
# MAKE UK DATA READY FOR BEING ADDED  
#

# All empty cells are replaced with zeros
df_landcover1_new1[is.na(df_landcover1_new1)] <- 0

# Get new column names for UK data 
# str(df_landcover1_new1)
new_names <- names(df_landcover1_new1) %>% 
  tolower() %>% 
  gsub(" (%)", "", ., fixed = TRUE) %>%
  gsub(" ", "_", ., fixed = TRUE)
# Check (compare):
# names(df_landcover)
# new_names

# Change all names (most are correct)
df_landcover1_new2 <- df_landcover1_new1
names(df_landcover1_new2) <- new_names

# Change the rest of the names
df_landcover1_new2 <- df_landcover1_new2 %>%
  select(-station_code, -station_name) %>%
  rename(
    catchment_area = `catchment_area_(km2)`,
    total_shrub_herbaceous = `total_shrub_and/or_herbaceous_vegetation`,
    transitional_woodland_shrub = `transitional_woodland/scrub`,
    water_ex_lake = `water_(excl._lake)`
  )

# Check column names
check <- !(names(df_landcover1_new2) %in% c("station_id", vars_landcover))
if (sum(check) > 0){
  names(df_landcover1_new2)[check] %>% print()
  stop("Some names don't fit")
} 
```


#### Add data      

```r
#
# ADD UK DATA TO MAIN DATA (without metadata)
#

# Combine 
df_landcover2 <- bind_rows(
  df_landcover1b[c("station_id", vars_landcover)],
  df_landcover1_new2[c("station_id", vars_landcover)],
)
cat(nrow(df_landcover2), "lines of data in df_landcover2 after adding UK data \n")
```

```
## 556 lines of data in df_landcover2 after adding UK data
```

### Extra Canada data

#### Read file  

```r
if (localfiles){
  fn <- "Data_landcover/20211026 Canada combined.xlsx"
} else {
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/Excel documents - received/Canada/20211026 Canada combined.xlsx"
}

df_landcover2_new1 <- read_excel(fn, 
                               sheet = "canada")

# Set this to numeric:
df_landcover2_new1$`Total shrub and/or herbaceous vegetation (%)` <- df_landcover2_new1$`Total shrub and/or herbaceous vegetation (%)` %>% as.numeric()

# Number of rows (should be the same)
cat(nrow(df_landcover2_new1), " lines of data read into df_landcover2_new1 \n")

cat("Number of rows in df_landcover2: ", 
        df_landcover2 %>% 
               filter(station_id %in% df_landcover2_new1$station_id) %>%
          nrow(),
    "\n")
```

```
## 115  lines of data read into df_landcover2_new1 
## Number of rows in df_landcover2:  115
```


#### Remove lines from main data   

```r
# Existing data in 'df_landcover2'  
stations <- df_landcover2_new1$station_id
sel_to_remove <- df_landcover2$station_id %in% stations

# Check existing data - just have altitide, a few (erronous) catchment area, and no land cover: 
# unique(df_landcover2$station_id[sel_to_remove])
# df_landcover2[sel_to_remove,] %>% View()
# df_landcover2[sel_to_remove,] %>% select(-(station_code:longitude), -(altitude:region)) %>% View()

# Remove existing data:
df_landcover2b <- df_landcover2[!sel_to_remove,]
cat(sum(sel_to_remove), "lines of data removed from df_landcover2 \n")
cat(nrow(df_landcover2b), "lines of data in df_landcover2b \n")
```

```
## 115 lines of data removed from df_landcover2 
## 441 lines of data in df_landcover2b
```


#### Make data ready to be added     

```r
# All empty cells are replaced with zeros
df_landcover2_new1[is.na(df_landcover2_new1)] <- 0

# Get new column names for the new data 
# str(df_landcover2_new1)
new_names <- names(df_landcover2_new1) %>% 
  tolower() %>% 
  gsub(" (%)", "", ., fixed = TRUE) %>%
  gsub(" ", "_", ., fixed = TRUE)
# Check (compare):
# names(df_landcover)
# new_names

# Change all names (most are correct)
df_landcover2_new2 <- df_landcover2_new1
names(df_landcover2_new2) <- new_names

# Change the rest of the names
df_landcover2_new2 <- df_landcover2_new2 %>%
  select(-station_code, -station_name) %>%
  rename(
    catchment_area = `catchment_area_(km2)`,
    total_shrub_herbaceous = `total_shrub_and/or_herbaceous_vegetation`,
    transitional_woodland_shrub = `transitional_woodland/scrub`,
    water_ex_lake = `water_(excl._lake)`
  )

# Special for Canada data:
df_landcover2_new2 <- df_landcover2_new2 %>%
  select(-total)

# Check column names
check <- !(names(df_landcover2_new2) %in% c("station_id", vars_landcover))
if (sum(check) > 0){
  names(df_landcover2_new2)[check] %>% print()
  stop("Some names don't fit")
} 
```


#### Add data      

```r
#
# ADD DATA TO MAIN DATA (without metadata)
#

# Combine 
df_landcover2 <- bind_rows(
  df_landcover2b[c("station_id", vars_landcover)],
  df_landcover2_new2[c("station_id", vars_landcover)],
)
cat(nrow(df_landcover2), "lines of data in df_landcover2 after adding Canada data \n")
```

```
## 556 lines of data in df_landcover2 after adding Canada data
```



## 4. Add land cover to metadata  

```r
n1 <- nrow(df_meta_without_landcover)

df_meta1 <- df_meta_without_landcover %>% 
  left_join(df_landcover2, by = "station_id")

n2 <- nrow(df_meta1)

# Check that join is ok
if (n2 != n1)
  stop("Number of rows changed; check whether there are only unique values of 'station_id'")

# Order columns
df_meta1 <- df_meta1[c(names(df_meta_without_landcover), vars_landcover)]

# names(df_landcover1); cat("\n")

cat(nrow(df_meta1), "lines of data in df_meta1 after adding new metadata \n")
```

```
## 556 lines of data in df_meta1 after adding new metadata
```
### Add altitude for Ireland  

#### Read file  

```r
if (localfiles){
  fn <- "Data_landcover/Ireland_icpsites_may2020.xlsx"
} else {
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/Ireland_icpsites_may2020.xlsx"
}

df_landcover_ie <- read_excel(fn)

cat("Number of rows: ", 
        nrow(df_landcover_ie),
    "\n\n")

cat("Number of rows in df_meta1: ", 
        df_meta1 %>% 
               filter(station_id %in% df_landcover_ie$station_id) %>%
          nrow(),
    "\n\n")
```

```
## Number of rows:  21 
## 
## Number of rows in df_meta1:  21
```

#### Add new altitude

```r
df_meta2 <- df_meta1 %>%
  left_join(
    df_landcover_ie %>% 
      select(station_id, altitude) %>%
      rename(altitude_ie = altitude)
  ) %>%
  mutate(
    altitude = case_when(
      is.na(altitude_ie) ~ altitude,
      !is.na(altitude_ie) ~ altitude_ie)
    )
```

```
## Joining, by = "station_id"
```

```r
xtabs(~is.na(altitude), df_meta1)
xtabs(~is.na(altitude), df_meta2)
```

```
## is.na(altitude)
## FALSE  TRUE 
##   540    16 
## is.na(altitude)
## FALSE  TRUE 
##   554     2
```


### Save     

```r
saveRDS(df_meta2, "Data/159_df_meta1.rds")
writexl::write_xlsx(df_meta2, "Data/159_df_meta1.xlsx")

# df_landcover2 <- readRDS("Data/159_df_landcover2.rds")

cat("\n------------------------------------------\n")
cat("Coverage per country\n")
xtabs(~country + is.na(total_forest), df_meta1)
```

```
## 
## ------------------------------------------
## Coverage per country
##                 is.na(total_forest)
## country          FALSE TRUE
##   Canada           115    0
##   Czech Republic     8    0
##   Estonia            1    0
##   Finland           26    0
##   Germany           35    0
##   Ireland           14    7
##   Italy             12    0
##   Latvia             8    0
##   Moldova            0    2
##   Netherlands        3    0
##   Norway            83    0
##   Poland            12    0
##   Slovakia          12    0
##   Sweden            92    0
##   Switzerland        9    0
##   United Kingdom    22    0
##   United States     75   20
```


## 5. Combine land cover types 'df_meta2'      
* bare_sparse = bare_rock + sparsely_vegetated + glacier   
* Select: coniferous, deciduous, lake, mixed_forest, wetland, bare_sparse   
* Also: removes all metadata (keeps only station_id, catchment_area, and land cover columns)  

```r
# df_landcover2 %>% select(-(station_code:longitude), -(altitude:region)) %>% names() %>% dput()

# Select columns
sel_columns <- c("station_id",
                 "station_code", "station_name", "latitude", "longitude", 
                 "altitude", "continent", "country", "region", "group",
                 "catchment_area", 
                 "urban", "cultivated", "total_forest", 
                 "deciduous", "coniferous", "mixed_forest", "total_shrub_herbaceous", 
                 "grasslands", "heathlands", "transitional_woodland_shrub", "bare_rock", 
                 "sparsely_vegetated", "glacier", "wetland", "lake", "water_ex_lake", "other")

# Add new combined data
df_meta3 <- df_meta2[sel_columns] %>%
  mutate(bare_sparse = bare_rock + sparsely_vegetated + glacier,
         decid_mixed = deciduous + mixed_forest,
         lake_water = lake + water_ex_lake)

# Remove 'old' columns
df_meta4 <- df_meta3 %>%
  select(-c(bare_rock, sparsely_vegetated, glacier, deciduous, mixed_forest, lake, water_ex_lake))

cat("df_meta3, n =", nrow(df_meta2), "\n")
```

```
## df_meta3, n = 556
```



## 6. Save  

```r
# Metadata including all original columns
saveRDS(df_meta3, "Data/159_df_meta3.rds")
writexl::write_xlsx(df_meta3, "Data/159_df_metadata_merged_landcover_allcols.xlsx")

# Metadata excluding columns that have been combined 
saveRDS(df_meta4, "Data/159_df_meta4.rds")
writexl::write_xlsx(df_meta4, "Data/159_df_metadata_merged_landcover.xlsx")
```


## 7. Statistics   

### Variables lacking   
* 76 stations lack only catchment_area   
* 11 stations lack catchment_area + land cover   
* 1 station lack catchment_area + land cover + altitude   
* 1 station lack catchment_area + altitude     
* 28 stations have land cover but lack only grasslands, heathlands and transitional_woodland_shrub  
* 9 stations have land cover but lack only coniferous + decid_mixed   


```r
cat("Number of stations which lack x variables")
tab1 <- apply(is.na(df_meta4), 1, sum)
tab2 <- table(tab1)
tab2

cat("\n\n")

for (i in as.numeric(names(tab2))){
  cat("Variables lacking for stations lacking", i, "variables: \n")
  tab_i <- apply(is.na(df_meta4[tab1 == i,]), 2, sum)
  print(
    tab_i[tab_i > 0])
  cat("\n")
}
```

```
## Number of stations which lack x variablestab1
##   0   1   2   3  13  14  15 
## 413  76  10  28  17  11   1 
## 
## 
## Variables lacking for stations lacking 0 variables: 
## named integer(0)
## 
## Variables lacking for stations lacking 1 variables: 
## catchment_area 
##             76 
## 
## Variables lacking for stations lacking 2 variables: 
##       altitude catchment_area     coniferous    decid_mixed 
##              1              1              9              9 
## 
## Variables lacking for stations lacking 3 variables: 
##                  grasslands                  heathlands 
##                          28                          28 
## transitional_woodland_shrub 
##                          28 
## 
## Variables lacking for stations lacking 13 variables: 
##                       urban                  cultivated 
##                          17                          17 
##                total_forest                  coniferous 
##                          17                          17 
##      total_shrub_herbaceous                  grasslands 
##                          17                          17 
##                  heathlands transitional_woodland_shrub 
##                          17                          17 
##                     wetland                       other 
##                          17                          17 
##                 bare_sparse                 decid_mixed 
##                          17                          17 
##                  lake_water 
##                          17 
## 
## Variables lacking for stations lacking 14 variables: 
##              catchment_area                       urban 
##                          11                          11 
##                  cultivated                total_forest 
##                          11                          11 
##                  coniferous      total_shrub_herbaceous 
##                          11                          11 
##                  grasslands                  heathlands 
##                          11                          11 
## transitional_woodland_shrub                     wetland 
##                          11                          11 
##                       other                 bare_sparse 
##                          11                          11 
##                 decid_mixed                  lake_water 
##                          11                          11 
## 
## Variables lacking for stations lacking 15 variables: 
##                    altitude              catchment_area 
##                           1                           1 
##                       urban                  cultivated 
##                           1                           1 
##                total_forest                  coniferous 
##                           1                           1 
##      total_shrub_herbaceous                  grasslands 
##                           1                           1 
##                  heathlands transitional_woodland_shrub 
##                           1                           1 
##                     wetland                       other 
##                           1                           1 
##                 bare_sparse                 decid_mixed 
##                           1                           1 
##                  lake_water 
##                           1
```


### Variables lacking, by country    

```r
cat("Availablility of altitude: \n")
df_meta4 %>%
  xtabs(~is.na(altitude) + country, .) %>% print()

cat("-----------------------------------------------------------------------------------------\n")
cat("Availablility of catchment_area: \n")
df_meta4 %>%
  xtabs(~is.na(catchment_area) + country, .) %>% print()

cat("-----------------------------------------------------------------------------------------\n")
cat("Availablility of catchment_area, for stations having land cover: \n")
df_meta4 %>%
  filter(!is.na(total_forest)) %>%
  xtabs(~is.na(catchment_area) + country, .)

cat("-----------------------------------------------------------------------------------------\n")
cat("Availablility of land cover, for stations having catchment_area: \n")
df_meta4 %>%
  filter(!is.na(catchment_area)) %>%
  xtabs(~is.na(total_forest) + country, .)

cat("-----------------------------------------------------------------------------------------\n")
cat("Availablility of 'grasslands', for stations having land cover: \n")
df_meta4 %>%
  filter(!is.na(total_forest)) %>%
  xtabs(~is.na(grasslands) + country, .)

cat("-----------------------------------------------------------------------------------------\n")
cat("Availablility of 'coniferous', for stations having land cover: \n")
df_meta4 %>%
  filter(!is.na(total_forest)) %>%
  xtabs(~is.na(coniferous) + country, .)
```

```
## Availablility of altitude: 
##                country
## is.na(altitude) Canada Czech Republic Estonia Finland Germany Ireland Italy
##           FALSE    115              8       0      26      35      21    12
##           TRUE       0              0       1       0       0       0     0
##                country
## is.na(altitude) Latvia Moldova Netherlands Norway Poland Slovakia Sweden
##           FALSE      8       1           3     83     12       12     92
##           TRUE       0       1           0      0      0        0      0
##                country
## is.na(altitude) Switzerland United Kingdom United States
##           FALSE           9             22            95
##           TRUE            0              0             0
## -----------------------------------------------------------------------------------------
## Availablility of catchment_area: 
##                      country
## is.na(catchment_area) Canada Czech Republic Estonia Finland Germany Ireland
##                 FALSE    115              8       0      26      34      14
##                 TRUE       0              0       1       0       1       7
##                      country
## is.na(catchment_area) Italy Latvia Moldova Netherlands Norway Poland Slovakia
##                 FALSE    12      8       1           3     83     12       12
##                 TRUE      0      0       1           0      0      0        0
##                      country
## is.na(catchment_area) Sweden Switzerland United Kingdom United States
##                 FALSE     92           9             22            16
##                 TRUE       0           0              0            79
## -----------------------------------------------------------------------------------------
## Availablility of catchment_area, for stations having land cover: 
##                      country
## is.na(catchment_area) Canada Czech Republic Estonia Finland Germany Ireland
##                 FALSE    115              8       0      26      34      14
##                 TRUE       0              0       1       0       1       0
##                      country
## is.na(catchment_area) Italy Latvia Netherlands Norway Poland Slovakia Sweden
##                 FALSE    12      8           3     83     12       12     92
##                 TRUE      0      0           0      0      0        0      0
##                      country
## is.na(catchment_area) Switzerland United Kingdom United States
##                 FALSE           9             22             0
##                 TRUE            0              0            75
## -----------------------------------------------------------------------------------------
## Availablility of land cover, for stations having catchment_area: 
##                    country
## is.na(total_forest) Canada Czech Republic Finland Germany Ireland Italy Latvia
##               FALSE    115              8      26      34      14    12      8
##               TRUE       0              0       0       0       0     0      0
##                    country
## is.na(total_forest) Moldova Netherlands Norway Poland Slovakia Sweden
##               FALSE       0           3     83     12       12     92
##               TRUE        1           0      0      0        0      0
##                    country
## is.na(total_forest) Switzerland United Kingdom United States
##               FALSE           9             22             0
##               TRUE            0              0            16
## -----------------------------------------------------------------------------------------
## Availablility of 'grasslands', for stations having land cover: 
##                  country
## is.na(grasslands) Canada Czech Republic Estonia Finland Germany Ireland Italy
##             FALSE    115              8       1      26      35      14    12
##             TRUE       0              0       0       0       0       0     0
##                  country
## is.na(grasslands) Latvia Netherlands Norway Poland Slovakia Sweden Switzerland
##             FALSE      8           3     83     12       12     64           9
##             TRUE       0           0      0      0        0     28           0
##                  country
## is.na(grasslands) United Kingdom United States
##             FALSE             22            75
##             TRUE               0             0
## -----------------------------------------------------------------------------------------
## Availablility of 'coniferous', for stations having land cover: 
##                  country
## is.na(coniferous) Canada Czech Republic Estonia Finland Germany Ireland Italy
##             FALSE    115              8       1      26      35      14    12
##             TRUE       0              0       0       0       0       0     0
##                  country
## is.na(coniferous) Latvia Netherlands Norway Poland Slovakia Sweden Switzerland
##             FALSE      8           3     83     12       12     92           0
##             TRUE       0           0      0      0        0      0           9
##                  country
## is.na(coniferous) United Kingdom United States
##             FALSE             22            75
##             TRUE               0             0
```

## Appendix: compare with other metadata    

```r
run_this <- FALSE
# run_this <- TRUE

### Check other land cover files
if (run_this){
  
  # Station metadata
  # THESE WILL BE ADDED TOGETHER WITH LAND COVER  
  
  folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_no3"
  file <- "trends_1992-2016_no3_stations.csv"
  fn <- paste0(folder, "/", file)
  df1 <- read.csv(fn, encoding = "UTF-8") %>% 
    arrange(station_id)
  nrow(df1)
  
  fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/all_icpw_sites_may_2020.xlsx"
  df2 <- read_excel(fn) %>% 
    arrange(station_id)
  nrow(df2)
  
  # Column names:
  n1 <- names(df1)
  n2 <- names(df2)
  cat("Missing in df1: \n")
  n2[!n2 %in% n1]
  cat("Missing in df2: \n")
  n1[!n1 %in% n2]
  
  cat("\n")
  cat("---------------------------------------------------------------\n")
  cat("Compare common variables: \n")
  cat("---------------------------------------------------------------\n")
  n_common <- n1[n1 %in% n2]
  for (col in n_common){
    # col <- "station_id"
    x1 <- sum(!df1[[col]] == df2[[col]], na.rm = TRUE)
    x2a <- sum(is.na(df1[[col]]))
    x2b <- sum(is.na(df2[[col]]))
    x3 <- sum(is.na(df1[[col]]) != is.na(df2[[col]]))
    cat(col, ": \n  n differences (rows with values):", x1, "\n")
    cat("  Rows lacking values:", x2a, "/", x2b, "\n")
    cat("  Rows lacking values different places:", x3, "\n")
  }


# View side by side
df1_forjoin <- df1[n_common]
names(df1_forjoin) <- paste0(n_common, "_1")  
df2_forjoin <- df2[n_common]
names(df2_forjoin) <- paste0(n_common, "_2")
# bind_cols(df1_forjoin, df2_forjoin) %>% View()
bind_cols(df1_forjoin$region_1, df2_forjoin$region_2) %>% View()
# fn <- "K:/Prosjekter/langtransporterte forurensninger/O-23300 - ICP-WATERS - HWI/Faglige rapporter/2020 report/Land cover/ICPW_All_Stations_2020_land cover_2020_05_04.xlsx"
  # df3 <- read_excel(fn)

}
```

