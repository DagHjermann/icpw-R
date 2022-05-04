# icpw-R  

R code for the ICP waters nitrogen report (autumn 2021-March 2022)        

## Scripts used in final analysis   

* Also see [script 180](180_fix_partial_plots.Rmd) used to add proper axis/legend titles to the final plots. (If you 
just want to fix things like axis and legend texts, use this!)    

### 3.1.2	Spatial variability of median NO3 concentrations 2012-2016  
* Run part **164ab1** in script `164parm_run_markdown.R` (this script runs `164parm_Currentstatus.Rmd`)      
    * Fig. 11: plot no. 7  
    * Fig. 12: plot no. 1  
    * Fig. 9: correlation plot (Figures/Correlation_matrices) 
    * In addition: Fig. 1 in the paper (example of classification tree, Figures/Methods/Example_tree_164ab1.png)   
    * Note; Fig. 1 and 9 were created using '164parm_Currentstatus_without_randomforest.Rmd'  

### 3.1.3	Statistical analysis of NO3 trends 1992-2016  
* Run part **166d1x** in script `166parm_run_markdown.R` (this script runs `166parm_Time_series_results_James.Rmd`)         
    * Fig. 15: plot no. 1  
    * Fig. 16: plot no. 2  
    * Fig. 17: plot no. 7  
    * Fig. 13: correlation plot (Figures/Correlation_matrices) 

### 3.2.2	Spatial variability of TOC/TON median concentration ratios 2012-2016   
* Run part **164c1** in script `164parm_run_markdown.R` (this script runs `164parm_Currentstatus.Rmd`)      
    * Fig. 27: plot no. 8  
    * Fig. 28: plot no. 9  
    * Fig. 30: plot no. 7    
    * Fig. 25: correlation plot (Figures/Correlation_matrices)   
    * Note; Fig. 25 was created using '164parm_Currentstatus_without_randomforest.Rmd'  

### 3.2.3 TOC/TON trends 1992-2016  
* Run part **167d1** in script `167parm_run_markdown.R` (this script runs `167parm_Time_series_tocton_continuous.Rmd`)      
    * Fig. 33: plot no. 1  
    * Fig. 34: plot no. 2    
    * Fig. 31: correlation plot (Figures/Correlation_matrices) 

## Overview of results  
[010_Methods_results.md](010_Methods_results.md) was made 'midway' in the project and doesn't contain final analyses   

## Overview of scripts   

* Note that the main scripts here are [parameterized markdown reports](https://bookdown.org/yihui/rmarkdown/parameterized-reports.html), where one Rmd script (e.g. `164parm_Currentstatus.Rmd`) containing the main code is run using several sets of variables and response variables using another script (in this case `164parm_run_markdown.R`). See 'Main scripts' section below.   

* Scripts 160-164 are explained below  

* Scripts 165-167 are *not* explained below, but see [Issue #2](https://github.com/DagHjermann/icpw-R/issues/2) on Github has an overview/history of scripts 165-167 (plus new variants of 160-164)   

* Also see Kari's overview on `\\niva-of5\osl-data-niva\Prosjekter\langtransporterte forurensninger\O-23300 - ICP-WATERS - HWI\Faglige rapporter\2020 report\Report\Oversikt datagrunnlag N-trend-rapport 2021.docx`  

**Note: also see [010_Methods_results.md](010_Methods_results.md) which has an overview of analyses and main results.** However this was made 'midway' in the project and doesn't contain final analyses.  



### Main scripts

For each of the 'series' 160-167, there are four types of files, with naming pattern is as follows:  

```  
1. [NUMBER]parm_[description].Rmd       - main code (Rmd for parametrized report)
2. [NUMBER]parm_run_markdown            - code for running script (1) with different parameters (a-d below), creating scripts (3) and (4)  
3. [NUMBER][letter]_[description].html  - resulting html file (cannot be shown on github)   
4. [NUMBER][letter]_[description].md    - resulting markdown file (can be shown on github)   
```

Example:  

1. `160parm_Time_series_results_James.Rmd` - the actual code for the analyses, but with placeholders for names of input data file and output results. This is used for all analyses in the '160 series'.       
2. `160parm_run_markdown.R` - 'master' script telling file 1 which input data file to use, which variables to use in models, and how to name the file with the output results 
3. `160a_Time_series_results_James_allvars.html` - the outputs in html format (for use on pc)    
4. `160a_Time_series_results_James_allvars.md`   - the outputs in hmarkdown format  

The difference between the 160, 161 and 162 results is the response variable:    

- 160: analysis of NO3 trend 1992-2016, categorical                     -
    - a,b,c,d: differ in selection of variables  
    - response variable = `no3_decline`, defined as 1 if there was a significant decrease in NO3 over the period 1992-2016 (`slope_no3_vs_time < 0 & p_no3_vs_time <= 0.05`)  
    - a,b,c,d: differ in selection of variables  
- 161: analysis of TOC/TON trend 1992-2016, categorical                   -
    - response variable = `tocton_decrease`, defined as 1 if there was a significant decrease in TOC/TON ratio over the period 1992-2016  
    - a,b,c: differ in selection of variables    
- 162: analysis of median 2012-2016, both NO3 and TOC/TON    
    - a and b: response variable = log_median_no3 = log10(median_no3 + 0.1)   
    - c and d: response variable = log_median_tocton = log10(median_tocton)     
- 
- 164: as 162, but without log transformation  


Note:

> The selection of variables is crucial, as some countries lack some variables. 

Variables used for each analysis:  

```  
For series 160:  
a - All variables including catchment_area and TOC  
b - All variables except catchment area (but including TOC)  
c - All variables except catchment area and TOC  
d - All variables except catchment area and altitude (but including TOC)  

For series 161:  

a. All variables including the slope of TOC and the slope of TON      
b. All variables excluding the slopes (TOC and TON)        
c. All variables excluding the slopes, and excludong TOC (in order to include 6 Italian stations)

For series 162:   
a - NO3 medians data set incl. catchment_area + TOC  
b - NO3 medians data set excl. TOC and catchment_area  
c - TOC/TON medians 2012-2016 - all variables  
d - TOC/TON medians data set, but not incuding TOC and TON medians in the analysis    
```


### Helper scripts
```
159                 - reads and put together land cover data. Used by 160, 161, 162
160params_functions - functions used by 160, 161, 162
```  

### Outdated scripts  
```
160_Time_series_results_James.Rmd
160_randomforest_James_data_model1.html (generated by randomForestExplainer::explain_forest()
160_randomforest_James_data_model2.html    " 
160_randomforest_James_data_model3.html    " 
```

