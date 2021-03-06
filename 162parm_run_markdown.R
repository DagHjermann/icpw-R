
# 
# This is the file used to render HTML documents based on 161parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#
  
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 162a - log_median_no3 ~ incl. catchment_area + TOC ----
#
# Using dataset for NO3: 'medians_2012-2016_no3.csv'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "log_median_no3", 
  "catchment_area, log_median_toc",     # change here
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars
# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "log_median_no3 ~ TOTN_dep +", 
  "slope_dep_vs_time + TOTN_dep:slope_dep_vs_time +",
  "log_median_toc + TOTN_dep:log_median_toc +",
  "tmp + pre + altitude +",
  "decid_mixed + bare_sparse + coniferous +",
  "catchment_area + lake_water + total_shrub_herbaceous"
)

# form

# To find file names used:
# dir(pattern = "162*")

# Render HTML and .md files  
rmarkdown::render(
  input = '162parm_Currentstatus.Rmd',                                        # always the same         
  output_file = '162a_Currentstatus_NO3_allvars.html',                        # change here
  params = list(
    document_title = "162a Analyse NO3 medians 2012-2016 - all variables",    # change here
    text_line1 = "Analysis of NO3 medians (2012-2016)",                       # change here
    text_line2 = "Dataset: NO3 medians data set incl. catchment_area + TOC",                             # change here
    medians_filename = "medians_2012-2016_no3.csv",
    selected_vars = vars,
    tree_formula = 'log_median_no3 ~ .'  ,                                     # change here
    logistic_formula = form)
)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 162b - log_median_no3 ~ excluding catchment_area + TOC ----
#
# Using dataset for NO3: 'medians_2012-2016_no3.csv'  
# Without 'log_median_toc'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "log_median_no3", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars
# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "log_median_no3 ~ TOTN_dep +", 
  "slope_dep_vs_time + TOTN_dep:slope_dep_vs_time +",
  "tmp + pre + altitude +",
  "decid_mixed + bare_sparse + coniferous +",
  "lake_water + total_shrub_herbaceous"
)

# form

# To find file names used:
# dir(pattern = "162*")

# Render HTML and .md files  
rmarkdown::render(
  input = '162parm_Currentstatus.Rmd',                                        # always the same         
  output_file = '162b_Currentstatus_NO3_no_TOC.html',                        # change here
  params = list(
    document_title = "162b Analyse NO3 medians 2012-2016 - without TOC + catchment area",    # change here
    text_line1 = "Analysis of NO3 medians (2012-2016)",                       # change here
    text_line2 = "Dataset: NO3 medians data set excl. TOC and catchment_area",                             # change here
    medians_filename = "medians_2012-2016_no3.csv",
    selected_vars = vars,
    tree_formula = 'log_median_no3 ~ .'  ,                                     # change here
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 162c - log_median_tocton ~ all variables ----
#
# Using dataset for TOC/TON: 'medians_2012-2016_toc_totn_no3.csv'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "log_median_tocton", 
  "catchment_area, log_median_ton, log_median_toc",     # change here
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "log_median_tocton ~ log_median_ton + log_median_toc +", 
  "log_median_ton:log_median_toc +",
  "tmp + pre +",                          # change here
  "altitude + bare_sparse + coniferous +",
  "lake_water + decid_mixed +", 
  "urban + catchment_area"
)
# form

# To find file names used:
# dir(pattern = "162*")

# Render HTML and .md files  
rmarkdown::render(
  input = '162parm_Currentstatus.Rmd',                                          # always the same         
  output_file = '162c_Currentstatus_TOCTON_allvars.html',                          # change here
  params = list(
    document_title = "162c Analyse TOC/TON medians 2012-2016 - all variables",  # change here
    text_line1 = "Analysis of TOC/TON medians (2012-2016)",                     # change here
    text_line2 = "Dataset: TOC/TON medians data set",                             # change here
    medians_filename = "medians_2012-2016_toc_totn_no3.csv",
    selected_vars = vars,
    tree_formula = 'log_median_tocton ~ .'  ,                                     # change here
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 162d - log_median_tocton ~ all variables ----
#
# Using dataset for TOC/TON: 'medians_2012-2016_toc_totn_no3.csv'  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "log_median_tocton", 
  "catchment_area",     # change here
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "log_median_tocton ~", 
  "tmp + pre +",                          # change here
  "altitude + bare_sparse + coniferous +",
  "lake_water + decid_mixed +", 
  "urban + catchment_area"
)
# form

# To find file names used:
# dir(pattern = "162*")

# Render HTML and .md files  
rmarkdown::render(
  input = '162parm_Currentstatus.Rmd',                                          # always the same         
  output_file = '162d_Currentstatus_TOCTON_no_TOC_TON.html',                          # change here
  params = list(
    document_title = "162d Analyse TOC/TON medians 2012-2016 - without TOC and TON",  # change here
    text_line1 = "Analysis of TOC/TON medians (2012-2016)",                        # change here
    text_line2 = "Dataset: TOC/TON medians data set, but not incudig TOC and TIN medians",                             # change here
    medians_filename = "medians_2012-2016_toc_totn_no3.csv",
    selected_vars = vars,
    tree_formula = 'log_median_tocton ~ .'  ,                                     # change here
    logistic_formula = form)
)

