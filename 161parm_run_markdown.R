
# 
# This is the file used to render HTML documents based on 161parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. USING coniferous + decid_mixed ----
# 161a1, 161b1, 161c1
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161a1 - All variables ----
#   incl. slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161a1_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "161a1 Analyse TOC/TON decrease - all variables",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161b1 - Excluding slope_toc_vs_time + slope_ton_vs_time ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '161b1_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "161b1 Analyse TOC/TON decrease - without TOC and TON slopes",   # change here
    text_dataset = "Dataset: all variables except TOC and TON slopes",               # change here
    selected_vars = vars,
    logistic_formula = form)
)





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161c1 - Without TOC and TON ----
#   as a1 but without TOC and TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area +",                    # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161c1_Time_series_tocton_without_2vars.html',                          # change here
  params = list(
    document_title = "161c1 Analyse TOC/TON decrease - w/o TOC, TON",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161d1 - Without TOC, TON, slope_toc_vs_time and slope_ton_vs_time ----
#   as a1 but without TOC, TON, slope_toc_vs_time and slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area +",                    # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161d1_Time_series_tocton_without_4vars.html',                          # change here
  params = list(
    document_title = "161d1 Analyse TOC/TON decrease - w/o TOC, TON, slope_toc_vs_time, slope_ton_vs_time",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. USING total_forest instead of coniferous + decid_mixed ---- 
# 161a2, 161b2, 161c2
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161a2 - All variables ----
#   incl. slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161a2_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "161a2 Analyse TOC/TON decrease - all variables",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161b2 - Excluding slope_toc_vs_time + slope_ton_vs_time ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '161b2_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "161b2 Analyse TOC/TON decrease - without TOC and TON slopes",   # change here
    text_dataset = "Dataset: all variables except TOC and TON slopes",               # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161c2 - Without TOC and TON ----
#   as a1 but without TOC and TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area +",                    # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161c2_Time_series_tocton_without_2vars.html',                          # change here
  params = list(
    document_title = "161c2 Analyse TOC/TON decrease - w/o TOC, TON",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161d2 - Without TOC, TON, slope_toc_vs_time and slope_ton_vs_time ----
#   as a1 but without TOC, TON, slope_toc_vs_time and slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area +",                    # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161d2_Time_series_tocton_without_4vars.html',                          # change here
  params = list(
    document_title = "161d2 Analyse TOC/TON decrease - w/o TOC, TON, slope_toc_vs_time, slope_ton_vs_time",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)

