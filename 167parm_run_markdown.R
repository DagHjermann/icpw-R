
# 
# This is the file used to render HTML documents based on 167parm_Time_series_results_James.Rmd'  
#
# NOTE WHEN PASSING PARAMETERS FROM rmarkdown::render: 
#   even if you pass 'params = ..."  from the 'params' MUST be in the YAML of the Rmd file (with some default values)
#   Otherwise you will get the error message 'params not found'.  
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167a1 - Basis (coniferous + decid_mixed separately) ----
#
# As 165a1 but without TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167a1_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167a1 Analyse TOC/TON change",                      # change here
    text_dataset = "Dataset: basis",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167b1 - Plus TOC + TON medians (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, TON",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC + TON"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167b1_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167b1 Analyse TOC/TON change",                 # change here
    text_dataset = "Basis + TOC + TON medians",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167c1 - Plus TOC + TON slopes (coniferous + decid_mixed separately) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, slope_toc_vs_time, slope_ton_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + coniferous + decid_mixed + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + slope_toc_vs_time + slope_ton_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167c1_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167c1 Analyse TOC/TON change",                 # change here
    text_dataset = "Basis + TOC + TON slopes",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167d1 - Plus TOC + TON medians and slopes (coniferous + decid_mixed separately) ----
#
# Note: wetland, lake_water and urban deleted from 'regression_formula'   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, TON, slope_toc_vs_time, slope_ton_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "cultivated + coniferous + decid_mixed +",
  "bare_sparse + TOC + TON + slope_toc_vs_time + slope_ton_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167d1_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167d1 Analyse TOC/TON change",                 # change here
    text_dataset = "Dataset: Basis + TOC + TON medians and slopes",  # change here
    selected_vars = vars,
    regression_formula = form,
    extra_pairwise_plots = 'TON,slope_pre;slope_pre,slope_tmp;slope_tmp,slope_pre'  # added
    )
)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167a2 - Basis (total_forest) ----
#
# As 165a2 but without TON
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167a2_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167a2 Analyse TOC/TON change",                      # change here
    text_dataset = "Dataset: basis",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167b2 - Plus TOC + TON medians  (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, TON",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + TOC + TON"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167b2_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167b2 Analyse TOC/TON change",                 # change here
    text_dataset = "Basis + TOC + TON medians",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167c2 - Plus TOC + TON slopes (total_forest) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, slope_toc_vs_time, slope_ton_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "urban + cultivated + total_forest + total_shrub_herbaceous +",
  "wetland + lake_water + bare_sparse + slope_toc_vs_time + slope_ton_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167c2_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167c2 Analyse TOC/TON change",                 # change here
    text_dataset = "Dataset: Basis + TOC + TON slopes",  # change here
    selected_vars = vars,
    regression_formula = form)
)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 167d2 - Plus TOC + TON medians and slopes (total_forest) ----
#
# Note: wetland, lake_water and urban deleted from 'regression_formula'   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# If many parameters, split up the lines with a 'paste()'  
# Remember to set that the parts should be joined using a comma (sep = ",") 
#   (or let each part end with a comma)
vars <- paste(
  "slope_tocton_vs_time", 
  "catchment_area, slope_pre, slope_tmp, slope_dep_vs_time, TOCTON, TOTN_dep, pre, tmp", 
  "urban, cultivated, total_forest, total_shrub_herbaceous",
  "wetland, lake_water, bare_sparse, TOC, TON, slope_toc_vs_time, slope_ton_vs_time",
  sep = ",")    # remember this

# If long formula, split up the lines with a 'paste()'  
form <- paste(
  "slope_tocton_vs_time ~ catchment_area+",                # change here
  "slope_pre + slope_tmp + slope_dep_vs_time + TOCTON + TOTN_dep+ pre + tmp +", 
  "cultivated + total_forest + total_shrub_herbaceous +",
  "bare_sparse + TOC + TON + slope_toc_vs_time + slope_ton_vs_time"
)

# To find file names used:
# dir(pattern = "160*")

# Render HTML and .md files  
rmarkdown::render(
  input = '167parm_Time_series_tocton_continuous.Rmd',          
  output_file = '167d2_Time_series_tocton_continuous.html',                   # change here
  params = list(
    document_title = "167d2 Analyse TOC/TON change",                 # change here
    text_dataset = "Dataset: Basis + TOC + TON medians and slopes",  # change here
    selected_vars = vars,
    regression_formula = form)
)

