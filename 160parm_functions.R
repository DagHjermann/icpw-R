
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
