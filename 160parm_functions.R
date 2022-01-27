
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




# Function for plotting partial effects  
# Used in 160parm, 161parm, 162parm   

# NOTE: assumes the existence of 'plotdata' and 'full_set' 
#  'plotdata' = a list consisting of data frames with 3 variables, where number 3 is called 'yhat'  

plot_pair_number <- function(i, zrange = NULL, legend_title = ""){
  
  variable_x <- rlang::sym(names(plotdata[[i]])[1])
  variable_y <- rlang::sym(names(plotdata[[i]])[2])
  
  gg <- ggplot(plotdata[[i]], aes(!!variable_x, !!variable_y)) +
    geom_tile(aes(fill = yhat)) +
    geom_contour(aes(z = yhat), color = "white") +
    geom_point(data = full_set, shape = 21, size = 1, colour = "white", bg = "red") +
    theme_bw()
  
  if (is.null(zrange)){
    gg + scale_fill_viridis_c(legend_title)
  } else {
    gg + scale_fill_viridis_c(legend_title, limits = zrange)
  }

}

