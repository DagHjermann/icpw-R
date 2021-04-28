
# 
# This is the file used to render HTML documents based on 161parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161a - All variables ----
#   incl. slope_toc_vs_time + slope_ton_vs_time
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_toc_vs_time, slope_ton_vs_time, slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_toc_vs_time + slope_ton_vs_time + slope_dep_vs_time +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)

# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                      # always the same         
  output_file = '161a_Time_series_tocton_allvars.html',                          # change here
  params = list(
    document_title = "161a Analyse TOC/TON decrease - all variables",            # change here
    text_dataset = "Dataset: all variables (including catchment area)",          # change here
    selected_vars = vars,
    logistic_formula = form)
)



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161b - Excluding slope_toc_vs_time + slope_ton_vs_time ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "catchment_area, TOC, TON",     # change here
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ catchment_area + TOC + TON +",                # change here
  "slope_dep_vs_time +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '161b_Time_series_tocton_wo_slopes.html',                            # change here
  params = list(
    document_title = "161b Analyse TOC/TON decrease - without TOC and TON slopes",   # change here
    text_dataset = "Dataset: all variables except TOC and TON slopes",               # change here
    selected_vars = vars,
    logistic_formula = form)
)





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 161c - Also exclude 'catchment_area' + TOC  ----
#   (in additon to excluding slope_toc_vs_time + slope_ton_vs_time)
# Because I thought that it would include the Italian data, but it didn't  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "tocton_decrease", 
  "TON",     # change here
  "slope_dep_vs_time, TOTN_dep, latitude, longitude, altitude",
  "pre, tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
# Remember to end each line with '+', or use sep = "+"  
form <- paste(
  "tocton_decrease ~ TON +",                      # change here
  "slope_dep_vs_time +",
  "altitude + decid_mixed + slope_dep_vs_time +", 
  "TOTN_dep + coniferous +",
  "tmp + lake_water + wetland"
)


# To find file names used:
# dir(pattern = "161*")

# Render HTML and .md files  
rmarkdown::render(
  input = '161parm_Time_series_tocton.Rmd',                                          # always the same         
  output_file = '161c_Time_series_tocton_wo_slopes_catcharea.html',                            # change here
  params = list(
    document_title = "161c Analyse TOC/TON decrease - without catchment area, TOC, and TOC + TON slopes",   # change here
    text_dataset = "Dataset: all variables except without catchment area, TOC, and TOC and TON slopes",     # change here
    selected_vars = vars,
    logistic_formula = form)
)





  
