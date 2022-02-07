
# 
# This is the file used to render HTML documents based on 163parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. USING coniferous + decid_mixed ----
# 163a1, 163b1, 163c1, 163d1
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163a1 - All variables ----
#   model incl. TOC, TON, slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + coniferous + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '163a1_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "163a1 Analyse TOC/TON decrease - medians and slopes of TOC and TON)",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163b1 - Excluding slope_toc_vs_time + slope_ton_vs_time ----
#   model incl. TOC, TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + coniferous + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '163b1_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "163b1 Analyse TOC/TON decrease - medians of TOC and TON",   # change here
    text_dataset = "Dataset: all variables except TOC and TON slopes",               # change here
    selected_vars = vars,
    logistic_formula = form)
)





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163c1 - All variables except TOC and TON ----
#   model incl. slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + coniferous + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '163c1_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "163c1 Analyse TOC/TON decrease - slopes of TOC and TON",            # change here
    text_dataset = "Dataset: slopes of TOC and TON",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163d1 - Excluding all TOC and TON medians or slopes ----
#   model incl. neither medians or slopes of TOC, TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + coniferous + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '163d1_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "163d1 Analyse TOC/TON decrease - neither medians or slopes of TOC and TON",   # change here
    text_dataset = "Dataset: neither medians or slopes of TOC and TON",               # change here
    selected_vars = vars,
    logistic_formula = form)
)





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. USING total_forest instead of coniferous + decid_mixed ---- 
# 163a2, 163b2, 163c2, 163d2
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163a2 - All variables ----
#   model incl. TOC, TON, slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '163a2_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "163a2 Analyse TOC/TON decrease - medians and slopes of TOC and TON)",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163b2 - Excluding slope_toc_vs_time + slope_ton_vs_time ----
#   model incl. TOC, TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '163b2_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "163b2 Analyse TOC/TON decrease - medians of TOC and TON",   # change here
    text_dataset = "Dataset: all variables except TOC and TON slopes",               # change here
    selected_vars = vars,
    logistic_formula = form)
)





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163c2 - All variables except TOC and TON ----
#   model incl. slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '163c2_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "163c2 Analyse TOC/TON decrease - slopes of TOC and TON",            # change here
    text_dataset = "Dataset: slopes of TOC and TON",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 163d2 - Excluding all TOC and TON medians or slopes ----
#   model incl. neither medians or slopes of TOC, TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOCTON",     # change here
  "slope_pre, slope_tmp", 
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOCTON +",                # change here
  "slope_dep_vs_time +",
  "slope_pre + slope_tmp +",
  "altitude + total_forest + slope_dep_vs_time +", 
  "TOTN_dep +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "163*")

# Render HTML and .md files  
rmarkdown::render(
  input = '163parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '163d2_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "163d2 Analyse TOC/TON decrease - neither medians or slopes of TOC and TON",   # change here
    text_dataset = "Dataset: neither medians or slopes of TOC and TON",               # change here
    selected_vars = vars,
    logistic_formula = form)
)

