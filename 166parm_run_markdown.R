
# 
# This is the file used to render HTML documents based on 166parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#
# NOTE: form / logistic_formula can be ignored, it is not used in the script 
#   (runs only random forest, not the logistic regression)
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166a1 - All variables incl. catchment_area + TOC (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "catchment_area, TOC",     # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_no3_vs_time ~ catchment_area + TOC +",                # change here
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "NO3 + TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166a1_Time_series_results_James_allvars.html',                   # change here
  params = list(
    document_title = "166a1 Analyse NO3 decline - all variables",                 # change here
    text_dataset = "Dataset: all variables (including catchment area and TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166a2 - All variables incl. catchment_area + TOC (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "catchment_area, TOC",     # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_no3_vs_time ~ catchment_area + TOC +",                # change here
  "altitude + total_forest + slope_dep_vs_time +", 
  "NO3 + TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166a2_Time_series_results_James_allvars.html',                   # change here
  params = list(
    document_title = "166a2 Analyse NO3 decline - all variables",                 # change here
    text_dataset = "Dataset: all variables (including catchment area and TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166b1 - All variables incl. TOC, excl. catchment_area (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "TOC, altitude",                                # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars

#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "slope_no3_vs_time ~ TOC*altitude +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166b1_Time_series_results_James_no_catcharea.html',                     # change here
  params = list(
    document_title = "166b1 Analyse NO3 decline - excl. catchment_area",                 # change here
    text_dataset = "Dataset: all variables except catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166b2 - All variables incl. TOC, excl. catchment_area (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "TOC, altitude",                                # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars

#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "slope_no3_vs_time ~ TOC*altitude +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "total_forest +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166b2_Time_series_results_James_no_catcharea.html',                     # change here
  params = list(
    document_title = "166b2 Analyse NO3 decline - excl. catchment_area",                 # change here
    text_dataset = "Dataset: all variables except catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166c1 - All variables excl. TOC, excl. catchment_area (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
                                  # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_no3_vs_time ~ ",                # change here
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "NO3 + TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166c1_Time_series_results_James_no_catcharea_TOC.html',                     # change here
  params = list(
    document_title = "166c1 Analyse NO3 decline - excl. catchment_area and TOC",                 # change here
    text_dataset = "Dataset: all variables except catchment area and TOC",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166c2 - All variables excl. TOC, excl. catchment_area (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_no3_vs_time ~ ",                # change here
  "altitude + total_forest + slope_dep_vs_time +", 
  "NO3 + TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166c2_Time_series_results_James_no_catcharea_TOC.html',                     # change here
  params = list(
    document_title = "166c2 Analyse NO3 decline - excl. catchment_area and TOC",                 # change here
    text_dataset = "Dataset: all variables except catchment area and TOC",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166d1 - All variables incl. TOC, excl. altitude (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "TOC",                                
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",   # altitude removed
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars
#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "slope_no3_vs_time ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166d1_Time_series_results_James_no_altitude.html',                     # change here
  params = list(
    document_title = "166d1 Analyse NO3 decline - excl. altitude and catchment_area",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166d1x - As 166d1 but including slope_toc_vs_time (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "TOC, slope_toc_vs_time",                                
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",   # altitude removed
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars
#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "slope_no3_vs_time ~ TOC + slope_toc_vs_time +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166d1x_Time_series_results_James_no_altitude.html',                     # change here
  params = list(
    document_title = "166d1x Analyse NO3 decline - excl. altitude and catchment_area",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 166d2 - All variables incl. TOC, excl. altitude (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_no3_vs_time", 
  "TOC",                                
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude",   # altitude removed
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars
#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "slope_no3_vs_time ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "total_forest +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "166*")

# Render HTML and .md files  
rmarkdown::render(
  input = '166parm_Time_series_results_James.Rmd',          
  output_file = '166d2_Time_series_results_James_no_altitude.html',                     # change here
  params = list(
    document_title = "166d2 Analyse NO3 decline - excl. altitude and catchment_area",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)



