
#
# Function to get interactions and effects 
#
get_model_variables <- function(model){  
  all_terms <- names(model$coefficients)
  # Remove '(Intercept)'
  all_terms <- all_terms[!all_terms %in% "(Intercept)"]
  sel_interactions <- grepl(":", all_terms, fixed = TRUE)
  # Removing interaction terms leaves us with the variables:
  all_variables <- all_terms[!sel_interactions]
  # Get interactions in a list
  interaction_terms <- all_terms[sel_interactions]
  interaction_list <- interaction_terms %>% strsplit(split = ":")
  # Get additive variables
  interaction_vars <- interaction_list %>% unlist()
  additive_vars <- all_variables[!all_variables %in% interaction_vars]
  list(
    interaction_list = interaction_list,
    additive_vars = additive_vars)
}

# Test
# fit <- lm(sr ~ pop15*dpi + pop75*dpi + ddpi, data = LifeCycleSavings)
# get_model_variables(fit)  


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

