#########################################################
#### Trial Duration From Occupancy ######################
#########################################################
get_trial_duration_months <- function(
    occupancy_df
){
  if(!"PF" %in% names(occupancy_df)){
    stop("PF state missing")
  }
  pf_months <- which(
    occupancy_df$PF > 0
  )
  if(length(pf_months) == 0){
    return(0)
  }
  max(pf_months)
}

#########################################################
#### Observed Trial Duration ############################
#########################################################
get_observed_pfs_duration_months <- function(
    survival_df
){
  km_rows <- survival_df[
    survival_df$model == "Kaplan-Meier",
  ]
  if(nrow(km_rows) == 0){
    stop("No Kaplan-Meier data found")
  }
  max(km_rows$time)
}

#########################################################
#### Trial Duration From KM #############################
#########################################################
get_trial_duration_from_ipd <- function(
    ipd
){
  if(
    is.null(ipd)
  ){
    stop("IPD missing")
  }
  km <- create_km_from_ipd(ipd)
  max(
    km$time,
    na.rm = TRUE
  )
}

#########################################################
#### Intervention Trial Duration ########################
#########################################################
int_trial_duration_months <- reactive({
  #######################################################
  #### Survival Data ####################################
  #######################################################
  if(
    input$int_pfs_input_type ==
    "Survival Data"
  ){
    km <- create_km_from_ipd(
      int_pfs_ipd()
    )
    return(
      max(
        km$time,
        na.rm = TRUE
      )
    )
  }
  #######################################################
  #### Hazard Ratio #####################################
  #######################################################
  if(
    input$int_pfs_input_type ==
    "Hazard Ratio"
  ){
    km <- create_km_from_ipd(
      comp_pfs_ipd()
    )
    return(
      max(
        km$time,
        na.rm = TRUE
      )
    )
  }
  #######################################################
  #### Median PFS #######################################
  #######################################################
  if(
    input$int_pfs_input_type ==
    "Median PFS"
  ){
    return(
      input$int_median_pfs
    )
  }
})

#########################################################
#### Comparator Trial Duration ##########################
#########################################################
comp_trial_duration_months <- reactive({
  #######################################################
  #### Survival Data ####################################
  #######################################################
  if(
    input$comp_pfs_input_type ==
    "Survival Data"
  ){
    km <- create_km_from_ipd(
      comp_pfs_ipd()
    )
    return(
      max(
        km$time,
        na.rm = TRUE
      )
    )
  }
  #######################################################
  #### Median PFS #######################################
  #######################################################
  if(
    input$comp_pfs_input_type ==
    "Median PFS"
  ){
    return(
      input$comp_median_pfs
    )
  }
})






