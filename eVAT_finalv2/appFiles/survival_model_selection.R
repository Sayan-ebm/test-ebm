# Get selected Comparator Curve
#########################################################
#### Get Active Model ###################################
#########################################################
get_selected_model_name <- function(
    fit_table,
    selected_row
){
  if(is.null(fit_table) || nrow(fit_table) == 0){
    return(NULL)
  }
  #######################################################
  #### NO USER SELECTION -> USE BEST AIC MODEL ##########
  #######################################################
  if(is.null(selected_row) || length(selected_row) == 0){
    return(fit_table$Model[1])
  }
  selected_row <- selected_row[
    selected_row <= nrow(fit_table)
  ]
  #######################################################
  #### USER SELECTION ###################################
  #######################################################
  fit_table$Model[selected_row]
}


# Extract one model survival curve
get_model_survival_curve <- function(
    all_survival,
    selected_model
){
  all_survival |>
    dplyr::filter(
      model == selected_model
    )
}




