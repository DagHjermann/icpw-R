
# 
# This is the file used to render HTML documents based on 160parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165a1 - Not including TOC + slope_toc_vs_time (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165a1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165a1 Analyse TON change",                      # change here
    text_dataset = "Dataset: Not including TOC + slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165b1 - Including TOC, not including slope_toc_vs_time (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165b1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165b1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Including TOC, not including slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165c1 - Not including TOC, including slope_toc_vs_time (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, slope_toc_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + slope_toc_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165c1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165c1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Not including TOC, including slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165d1 - Including TOC and slope_toc_vs_time (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, slope_toc_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC + slope_toc_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165d1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165d1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Including TOC and slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165a2 - Not including TOC + slope_toc_vs_time (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165a1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165a1 Analyse TON change",                      # change here
    text_dataset = "Dataset: Not including TOC + slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165b2 - Including TOC, not including slope_toc_vs_time (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165b1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165b1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Including TOC, not including slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165c2 - Not including TOC, including slope_toc_vs_time (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, slope_toc_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + slope_toc_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165c1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165c1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Not including TOC, including slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 165d2 - Including TOC and slope_toc_vs_time (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_ton_vs_time", 
  "catchment_area, TON, slope_pre, slope_tmp, slope_dep_vs_time, TOTN_dep, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, slope_toc_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_ton_vs_time ~ catchment_area + TON +",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOTN_dep + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC + slope_toc_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '165parm_Time_series_ton_continuous.Rmd',          
  output_file = '165d1_Time_series_ton_continuous.html',                   # change here
  params = list(
    document_title = "165d1 Analyse TON change",                 # change here
    text_dataset = "Dataset: Including TOC and slope_toc_vs_time",  # change here
    selected_vars = vars,
    regression_formula = form)
)

