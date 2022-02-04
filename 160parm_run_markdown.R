
# 
# This is the file used to render HTML documents based on 160parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160a1 - All variables incl. catchment_area + TOC (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
  "catchment_area, TOC",     # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "no3_decline ~ catchment_area + TOC +",                # change here
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "NO3 + TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160a1_Time_series_results_James_allvars.html',                   # change here
  params = list(
    document_title = "160a1 Analyse NO3 decline - all variables",                 # change here
    text_dataset = "Dataset: all variables (including catchment area and TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160a2 - All variables incl. catchment_area + TOC (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
  "catchment_area, TOC",     # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "no3_decline ~ catchment_area + TOC +",                # change here
  "altitude + total_forest + slope_dep_vs_time +", 
  "NO3 + TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160a2_Time_series_results_James_allvars.html',                   # change here
  params = list(
    document_title = "160a2 Analyse NO3 decline - all variables",                 # change here
    text_dataset = "Dataset: all variables (including catchment area and TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160b1 - All variables incl. TOC, excl. catchment_area (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
  "TOC",                                # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars

#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "no3_decline ~ TOC*altitude +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160b1_Time_series_results_James_no_catcharea.html',                     # change here
  params = list(
    document_title = "160b1 Analyse NO3 decline - excl. catchment_area",                 # change here
    text_dataset = "Dataset: all variables except catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160b2 - All variables incl. TOC, excl. catchment_area (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
  "TOC",                                # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this
vars

#
# If long formula, split up the lines with a 'paste()'  
#
form <- paste(
  "no3_decline ~ TOC*altitude +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "total_forest +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160b2_Time_series_results_James_no_catcharea.html',                     # change here
  params = list(
    document_title = "160b2 Analyse NO3 decline - excl. catchment_area",                 # change here
    text_dataset = "Dataset: all variables except catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160c1 - All variables excl. TOC, excl. catchment_area (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
                                  # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "no3_decline ~ ",                # change here
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "NO3 + TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160c1_Time_series_results_James_no_catcharea_TOC.html',                     # change here
  params = list(
    document_title = "160c1 Analyse NO3 decline - excl. catchment_area and TOC",                 # change here
    text_dataset = "Dataset: all variables except catchment area and TOC",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160c2 - All variables excl. TOC, excl. catchment_area (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
  # change here
  "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, slope_pre, slope_tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "no3_decline ~ ",                # change here
  "altitude + total_forest + slope_dep_vs_time +", 
  "NO3 + TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160c2_Time_series_results_James_no_catcharea_TOC.html',                     # change here
  params = list(
    document_title = "160c2 Analyse NO3 decline - excl. catchment_area and TOC",                 # change here
    text_dataset = "Dataset: all variables except catchment area and TOC",  # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160d1 - All variables incl. TOC, excl. altitude (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
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
  "no3_decline ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160d1_Time_series_results_James_no_altitude.html',                     # change here
  params = list(
    document_title = "160d1 Analyse NO3 decline - excl. altitude and catchment_area",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160d2 - All variables incl. TOC, excl. altitude (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
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
  "no3_decline ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "total_forest +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160d2_Time_series_results_James_no_altitude.html',                     # change here
  params = list(
    document_title = "160d2 Analyse NO3 decline - excl. altitude and catchment_area",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form)
)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160e1 - As 160d1, but for North America only ----
#       - All variables incl. TOC, excl. altitude (coniferous + decid_mixed separately)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
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
  "no3_decline ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160e1_Time_series_results_James_no_altitude_NAmerica.html',                     # change here
  params = list(
    document_title = "160e1 Analyse NO3 decline - excl. altitude and catchment_area - N. America",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form,
    extra_selection_variable = 'continent',
    extra_selection_values = 'North America'
  )
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 160f1 - As 160d1, but for Europe only ----
#       - All variables incl. TOC, excl. altitude (coniferous + decid_mixed separately)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "no3_decline", 
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
  "no3_decline ~ TOC +",                # change here
  "TOTN_dep*slope_dep_vs_time + NO3 +", 
  "decid_mixed + coniferous +",
  "tmp + lake_water + wetland"
)
# form 

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '160parm_Time_series_results_James.Rmd',          
  output_file = '160f1_Time_series_results_James_no_altitude_Europe.html',                     # change here
  params = list(
    document_title = "160f1 Analyse NO3 decline - excl. altitude and catchment_area - Europe",                 # change here
    text_dataset = "Dataset: all variables except altitude and catchment area (but including TOC)",  # change here
    selected_vars = vars,
    logistic_formula = form,
    extra_selection_variable = 'continent',
    extra_selection_values = 'Europe'
  )
)




