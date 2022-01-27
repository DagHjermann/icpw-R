
#
# left_join2 
#
# as 'left_join', but     
# - Does not accept common variables that are not key variables (specified in "by")  
# - Gives warning if there are nn-unique combinations of key variables i data set 2, so number of rows increase (duplicating rows in data set no. 1)   
# - Optionally, Lists existing variables, key variables and added variables (if print_vars = TRUE)  

left_join2 <- function(data1, data2, by = by, ..., print_vars = FALSE){
  
  common_vars <- intersect(names(data1), names(data2))
  common_vars_not_in_by <- common_vars[!common_vars %in% by]
  
  if (length(common_vars_not_in_by) > 0){
    stop("Columns ", paste(sQuote(common_vars_not_in_by), collapse = ", "), " exists in both data sets")
  }
  result <- left_join(data1, data2, by = by, ...)
  
  if (nrow(result) > nrow(data1)){
    warning("Data set 2 does not have unique rows. Number of rows increased from ", 
            nrow(data1), " to ", nrow(result))
  }
  
  vars_added <-   names(data2)[!names(data2) %in% names(data1)]
  
  if (print_vars){
    cat("Variables before join: \n")
    paste(sQuote(names(data1), q = FALSE), collapse = ", ") %>% cat()
    cat("\n\nVariables used to join: \n")
    paste(sQuote(by, q = FALSE), collapse = ", ") %>% cat()
    cat("\n\nVariables added: \n")
    paste(sQuote(vars_added, q = FALSE), collapse = ", ") %>% cat()
    cat("\n")
    
  }
  
  result
  
}





get_data_no3trend <- function(print_steps = FALSE, data_folder = "Data") {
  
  library(dplyr)
  library(tidyr)
  
  #
  # Regression results
  #
  folder <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/trends_1992-2016_no3"
  file <- "trends_1992-2016_no3_results.csv"
  fn <- paste0(folder, "/", file)
  
  reg_no3 <- read.csv(fn, encoding = "UTF-8")
  cat("Based on regression results:", sQuote(file), ",n =", nrow(reg_no3), "\n\n")
  
  # Station metadata
  # Slope 
  df1 <- reg_no3 %>%
    filter(variable %in% c("NO3-N_µg/l N", "TOC/TON")) %>%
    select(station_id, variable, sen_slp) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "sen_slp") %>%
    rename(slope_no3_vs_time = `NO3-N_µg/l N`, 
           slope_tocton_vs_time = `TOC/TON`)
  
  # Slope p-value
  df2 <- reg_no3 %>%
    filter(variable %in% c("NO3-N_µg/l N", "TOC/TON")) %>%
    select(station_id, variable, mk_p_val) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "mk_p_val") %>%
    rename(p_no3_vs_time = `NO3-N_µg/l N`, 
           p_tocton_vs_time = `TOC/TON`)
  
  # Medians
  df3 <- reg_no3 %>%
    filter(variable %in% c("NO3-N_µg/l N", "TOC_mg C/l")) %>%
    select(station_id, variable, median) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "median") %>%
    rename(NO3 = `NO3-N_µg/l N`, 
           TOC = `TOC_mg C/l`)
  
  if (print_steps){
    cat("\n")
    cat("df1, n =", nrow(df1), "\n")
    cat("df2, n =", nrow(df2), "\n")
    cat("df3, n =", nrow(df3), "\n")
  }
  
  dat_1 <- df1 %>%
    full_join(df2, by = "station_id") %>%
    full_join(df3, by = "station_id")
  
  if (print_steps)
    cat("dat_1, n =", nrow(dat_1), "\n")
  
  #
  # Deposition
  #
  
  fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/deposition/totn_dep_trends_icpw_stns.csv"  
  
  df_deposition <- read.csv(fn) %>% 
    filter(variable == "totn_mgNpm2")  
  
  if (print_steps)
    cat("n =", nrow(df_deposition), "\n")
    
  
  dat_2 <- dat_1 %>% 
    left_join2(df_deposition %>% 
                 select(station_id, median, sen_slp, mk_p_val) %>%
                 rename(TOTN_dep = median,
                        slope_dep_vs_time = sen_slp,
                        p_dep_vs_time = mk_p_val),
               by = "station_id",
               print_vars = print_steps)
  
  
  #
  # Climate means and trends
  #
  
  fn <- "https://github.com/JamesSample/icpw2/raw/master/thematic_report_2020/results/climate/cru_climate_trends_icpw_stns.csv"
  
  df_climate_mean <- read_csv(fn, show_col_types = FALSE) %>% 
    select(station_id, variable, median) %>%
    pivot_wider(names_from = "variable", values_from = "median")
  cat("\n")
  # names(df_climate_mean)
  
  df_climate_slope <- read_csv(fn, show_col_types = FALSE) %>%
    select(station_id, variable, sen_slp) %>%
    pivot_wider(names_from = "variable", values_from = "sen_slp", names_prefix = "Slope_")
  
  # Add
  dat_3 <- dat_2 %>%
    left_join2(df_climate_mean, by = "station_id", print_vars = print_steps) %>%
    left_join2(df_climate_slope, by = "station_id", print_vars = print_steps)
  
  #
  # Land cover
  #
  df_landcover3 <- readRDS(paste0(data_folder, "/159_df_meta3.rds"))
  
  df_landcover3 <- df_landcover3 %>%
    mutate(bare_sparse = bare_rock + sparsely_vegetated + glacier,
           decid_mixed = deciduous + mixed_forest,
           lake_water = lake + water_ex_lake) %>%
    select(-c(bare_rock, sparsely_vegetated, glacier, deciduous, mixed_forest, lake, water_ex_lake))
  
  
  dat_4 <- left_join2(dat_3, 
                      df_landcover3, 
                      by = "station_id", 
                      print_vars = print_steps
  )
  
  dat_4
  
}


# test <- get_data_no3trend()

#
# extract_rmd - extract all code from Rmd file  
#
# - Used to extract code for 'get_data_no3trend' from '160parm_Time_series_results_James.Rmd' 
#
extract_rmd <- function(file_path, output_file = NULL) {
  
  library(readr)
  library(stringr)
  
  stopifnot(is.character(file_path) && length(file_path) == 1)
  if (is.null(output_file)){
    output_file <- tempfile(fileext = ".R")
  }
  .con <- file(output_file) 
  on.exit(close(.con))
  full_rmd <- read_file(file_path)
  codes <- str_match_all(string = full_rmd, pattern = "```(?s)\\{r[^{}]*\\}\\s*\\n(.*?)```")
  stopifnot(length(codes) == 1 && ncol(codes[[1]]) == 2)
  codes <- paste(codes[[1]][, 2], collapse = "\n")
  codes <- gsub("\r\n", "\n", codes)
  writeLines(codes, .con)
  flush(.con)
  cat(sprintf("R code extracted to tempfile: %s\nSourcing tempfile...", output_file))
  # source(output_file)
  
}

# extract_rmd("160parm_Time_series_results_James.Rmd", "temp.R")
