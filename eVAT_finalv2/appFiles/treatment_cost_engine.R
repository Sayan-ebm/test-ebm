#########################################################
#### Extend Occupancy To Horizon ########################
#########################################################
extend_occupancy_to_horizon <- function(
    occupancy_df,
    horizon_months
){
  current_horizon <- max(occupancy_df$time, na.rm = TRUE)
  if(
    current_horizon >= horizon_months
  ){
    return(occupancy_df)
  }
  last_row <- occupancy_df[
    nrow(occupancy_df),
  ]
  extension <- data.frame(
    time = (current_horizon + 1):horizon_months,
    PF   = last_row$PF,
    PD   = last_row$PD,
    Dead = last_row$Dead
  )
  rbind(
    occupancy_df,
    extension
  )
}

#########################################################
#### Calculate Monthly Treatment Costs ##################
#########################################################
calculate_monthly_treatment_costs <- function(
    occupancy_df,
    administration_schedule,
    horizon_months
){
  
  #######################################################
  #### INITIALIZE #######################################
  #######################################################
  monthly_costs <- rep(0, horizon_months + 1)
  
  #######################################################
  #### NO ADMINISTRATIONS ###############################
  #######################################################
  if (nrow(administration_schedule) == 0) {
    return(list(
      monthly_costs = monthly_costs,
      audit_df = NULL
    ))
  }
  
  #######################################################
  #### VALIDATION #######################################
  #######################################################
  if (!"PF" %in% names(occupancy_df)) {
    stop("PF state missing from occupancy_df")
  }
  
  #######################################################
  #### AGGREGATE ADMIN COSTS ############################
  #######################################################
  monthly_admin_costs <- aggregate(
    cost ~ model_month,
    data = administration_schedule,
    FUN = sum
  )
  
  #######################################################
  #### APPLY PF-WEIGHTED COSTS ##########################
  #######################################################
  for (i in seq_len(nrow(monthly_admin_costs))) {
    
    month <- monthly_admin_costs$model_month[i]
    idx   <- month + 1
    
    if (idx > length(monthly_costs)) {
      next
    }
    
    monthly_costs[idx] <-
      monthly_admin_costs$cost[i] *
      occupancy_df$PF[idx]
  }
  
  #######################################################
  #### AUDIT TABLE ######################################
  #######################################################
  audit_df <- data.frame(
    time = 0:horizon_months,
    PF = occupancy_df$PF[1:(horizon_months + 1)],
    admin_cost = 0,
    final_cost = monthly_costs
  )
  
  for (i in seq_len(nrow(monthly_admin_costs))) {
    
    idx <- monthly_admin_costs$model_month[i] + 1
    
    if (idx <= nrow(audit_df)) {
      audit_df$admin_cost[idx] <-
        monthly_admin_costs$cost[i]
    }
  }
  
  #######################################################
  #### RETURN ###########################################
  #######################################################
  list(
    monthly_costs = monthly_costs,
    audit_df = audit_df
  )
}

#########################################################
#### Build Treatment Object #############################
#########################################################
build_treatment_object <- function(
    occupancy_df,
    therapy_settings,
    horizon_months,
    discount_rate_cost = 0
){
  #######################################################
  #### VALIDATE SETTINGS ################################
  #######################################################
  validate_therapy_settings(therapy_settings)
  
  #######################################################
  #### GENERATE ADMINISTRATION SCHEDULE #################
  #######################################################
  administration_schedule <-
    generate_administration_schedule(
      therapy_settings = therapy_settings,
      horizon_months = horizon_months
    )
  
  #######################################################
  #### ASSIGN COSTS #####################################
  #######################################################
  administration_schedule <-
    assign_administration_costs(
      administration_schedule = administration_schedule,
      therapy_settings = therapy_settings
    )
  
  cat("\n====================\n")
  cat("ADMINISTRATION SCHEDULE\n")
  cat("====================\n")
  print(head(administration_schedule,20))
  
  #write.csv(administration_schedule,  paste0(arm_name, "_", therapy_settings$therapy_name, "_administration_schedule.csv"), row.names = FALSE)
 
  # timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  # file_name <- paste0(gsub(" ","_",therapy_settings$therapy_name), "_", timestamp, "_administration_schedule.csv")
  # write.csv(administration_schedule, file_name, row.names = FALSE)
  # 
  #######################################################
  #### EXTEND OCCUPANCY #################################
  #######################################################
  occupancy_df <-
    extend_occupancy_to_horizon(
      occupancy_df = occupancy_df,
      horizon_months = horizon_months
    )
  #######################################################
  #### DETERMINE TREATMENT RULE #########################
  #######################################################
  # treatment_until_progression <-
  #   therapy_settings$max_cycles_flag == "No"
  #######################################################
  #### CALCULATE COSTS ##################################
  #######################################################
  cost_results <-
    calculate_monthly_treatment_costs(
      occupancy_df = occupancy_df,
      administration_schedule = administration_schedule,
      horizon_months = horizon_months
    )

  monthly_costs_undisc <- cost_results$monthly_costs
  
  monthly_costs_disc <- apply_discount_vector(
    monthly_costs_undisc,
    rate = discount_rate_cost,
    cycle_length_months = 1
  )
  
  audit_df<- cost_results$audit_df
  
  cat("\n====================\n")
  cat("THERAPY COST SUMMARY\n")
  cat("====================\n")
  cat("\nTherapy:",therapy_settings$therapy_name)
  
  cat("\nTotal Cost Undiscounted:", sum(monthly_costs_undisc))
  cat("\nTotal Cost Discounted:", sum(monthly_costs_disc))
  cat("\nAdministrations:",nrow(administration_schedule))
  cat("\nTreatment Horizon:",horizon_months)
  cat("\n====================\n")
  
  #######################################################
  #### BUILD OBJECT #####################################
  #######################################################
  list(
    settings = therapy_settings,
    administration_schedule = administration_schedule,
    audit_df = audit_df,
    monthly_costs_undiscounted = monthly_costs_undisc,
    monthly_costs_discounted = monthly_costs_disc,
    total_cost_undiscounted = sum(monthly_costs_undisc, na.rm = TRUE),
    total_cost_discounted = sum(monthly_costs_disc, na.rm = TRUE)
  )
}


#########################################################
#### Extract Monthly Costs ##############################
#########################################################
get_monthly_treatment_costs <- function(
    treatment_object,
    discounted = TRUE
){
  if(discounted){
    return(treatment_object$monthly_costs_discounted)
  }
  treatment_object$monthly_costs_undiscounted
}

#########################################################
#### Extract Total Cost #################################
#########################################################
get_total_treatment_cost <- function(
    treatment_object,
    discounted = TRUE
){
  if(discounted){
    return(treatment_object$total_cost_discounted)
  }
  treatment_object$total_cost_undiscounted
}

#########################################################
#### Extract Administration Schedule ####################
#########################################################
get_administration_schedule <- function(
    treatment_object
){
  treatment_object$administration_schedule
}

#########################################################
#### Print Treatment Summary ############################
#########################################################
print_treatment_summary <- function(
    treatment_object
){
  cat("\n====================\n")
  cat("TREATMENT SUMMARY\n")
  cat("====================\n")
  cat("\nTherapy:\n")
  print(treatment_object$settings$therapy_name)
  
  cat("\nTotal Cost Undiscounted:\n")
  print(treatment_object$total_cost_undiscounted)
  
  cat("\nTotal Cost Discounted:\n")
  print(treatment_object$total_cost_discounted)
  
  cat("\nNumber of Administrations:\n")
  print(nrow(treatment_object$administration_schedule))
  
  cat("\n====================\n")
}

#########################################################
#### Build Arm Treatment Object #########################
#########################################################
build_arm_treatment_object <- function(
    occupancy_df,
    therapy_settings_list,
    horizon_months,
    discount_rate_cost = 0
){
 # write.csv(occupancy_df, "occupancy_states_used.csv", row.names = FALSE)
  #######################################################
  #### STORE THERAPY OBJECTS ############################
  #######################################################
  therapy_objects <- list()
  
  #######################################################
  #### BUILD EACH THERAPY ###############################
  #######################################################
  for(i in seq_along(therapy_settings_list)){
    therapy_objects[[i]] <-
      build_treatment_object(
        occupancy_df = occupancy_df,
        therapy_settings = therapy_settings_list[[i]],
        horizon_months = horizon_months,
        discount_rate_cost = discount_rate_cost
      )
  }
  
  #######################################################
  #### TOTAL MONTHLY COSTS (UNDISCOUNTED) ###############
  #######################################################
  total_monthly_costs_undisc <- rep(0, horizon_months + 1)
  
  for(i in seq_along(therapy_objects)){
    total_monthly_costs_undisc <- total_monthly_costs_undisc + therapy_objects[[i]]$monthly_costs_undiscounted
  }
  
  #######################################################
  #### DISCOUNTING (ARM LEVEL) ##########################
  #######################################################
  total_monthly_costs_disc <- rep(0, horizon_months + 1)
  
  for(i in seq_along(therapy_objects)){
    total_monthly_costs_disc <-
      total_monthly_costs_disc +
      therapy_objects[[i]]$monthly_costs_discounted
  }
  
  #######################################################
  #### RETURN ARM OBJECT ################################
  #######################################################
  list(
    therapies = therapy_objects,
    monthly_costs_undiscounted = total_monthly_costs_undisc,
    monthly_costs_discounted   = total_monthly_costs_disc,
    total_cost_undiscounted = sum(total_monthly_costs_undisc, na.rm = TRUE),
    total_cost_discounted   = sum(total_monthly_costs_disc, na.rm = TRUE)
  )
}

#########################################################
#### Extract Total Arm Cost #############################
#########################################################
get_total_arm_cost <- function(
    arm_treatment_object,
    discounted = TRUE
){
  if(discounted){
    return(arm_treatment_object$total_cost_discounted)
  }
  arm_treatment_object$total_cost_undiscounted
}

#########################################################
#### Extract Monthly Arm Costs ##########################
#########################################################
get_arm_monthly_costs <- function(
    arm_treatment_object,
    discounted = TRUE
){
  if(discounted){
    return(arm_treatment_object$monthly_costs_discounted)
  }
  arm_treatment_object$monthly_costs_undiscounted
}

#########################################################
#### Extract Therapy Breakdown ##########################
#########################################################
get_therapy_breakdown <- function(
    arm_treatment_object
){
  data.frame(
    Therapy =
      sapply(
        arm_treatment_object$therapies,
        function(x)
          x$settings$therapy_name
      ),
    Undiscounted_Cost =
      sapply(
        arm_treatment_object$therapies,
        function(x)
          x$total_cost_undiscounted
      ),
    Discounted_Cost =
      sapply(
        arm_treatment_object$therapies,
        function(x)
          x$total_cost_discounted
      )
  )
}


#########################################################
#### Build Arm Validation Table #########################
#########################################################
build_arm_validation_table <- function(
    arm_treatment_object,
    occupancy_df,
    ae_object = NULL,
    health_state_object = NULL
){
  n_months <- nrow(occupancy_df)
  df <- data.frame(
    Month = occupancy_df$time,
    PF    = occupancy_df$PF,
    PD    = occupancy_df$PD,
    Dead  = occupancy_df$Dead
  )
  #######################################################
  #### TREATMENTS #######################################
  #######################################################
  therapy_list <- arm_treatment_object$therapies
  for(i in seq_along(therapy_list)){
    therapy <- therapy_list[[i]]
    therapy_name <- gsub(" ", "_", therapy$settings$therapy_name)
    #####################################################
    #### ON TREATMENT ###################################
    #####################################################
    on_vec <- rep(0,n_months)
    schedule <- therapy$administration_schedule
    if(nrow(schedule) > 0){
      valid_idx <- schedule$model_month + 1
      valid_idx <- valid_idx[
        valid_idx <= n_months
      ]
      on_vec[valid_idx] <- 1
    }
    #####################################################
    #### COST VECTORS ###################################
    #####################################################
    cost_vec <- therapy$monthly_costs_undiscounted[
      seq_len(n_months)
    ]
    cost_vec_disc <- therapy$monthly_costs_discounted[
      seq_len(n_months)
    ]
    #####################################################
    #### STORE ##########################################
    #####################################################
    df[
      ,
      paste0(therapy_name,"_On")
    ] <- on_vec
    df[
      ,
      paste0(therapy_name,"_Cost")
    ] <- cost_vec
    df[
      ,
      paste0(therapy_name,"_Cost_Disc")
    ] <- cost_vec_disc
  }
  #######################################################
  #### TOTAL TREATMENT ##################################
  #######################################################
  cost_cols <- grep("_Cost$", names(df))
  cost_disc_cols <- grep("_Cost_Disc$",names(df))
  df$Treatment_Cost <- rowSums(df[,cost_cols,drop=FALSE])
  df$Treatment_Cost_Disc <- rowSums(df[,cost_disc_cols,drop=FALSE])
  #######################################################
  #### AE RESULTS #######################################
  #######################################################
  if(!is.null(ae_object)){
    df$AE_Cost <- ae_object$monthly_costs
    if(
      !is.null(
        ae_object$monthly_costs_discounted
      )
    ){
      df$AE_Cost_Disc <-
        ae_object$monthly_costs_discounted
    }
    df$AE_Disutility <-
      ae_object$monthly_disutility
  }
  #######################################################
  #### HEALTH STATES ####################################
  #######################################################
  if(!is.null(health_state_object)){
    cycle_df <- health_state_object$cycle_results
    df$HealthState_Cost <- cycle_df$Total_State_Cost[seq_len(n_months)]
    df$HealthState_Cost_Disc <- cycle_df$Cost_Discounted[seq_len(n_months)]
    df$LY <- cycle_df$LYs[seq_len(n_months)]
    df$LY_Disc <- cycle_df$LY_Discounted[seq_len(n_months)]
    df$QALY_Undisc <- cycle_df$Total_QALY[seq_len(n_months)]
    df$QALY_Disc <- cycle_df$QALY_Discounted[seq_len(n_months)]
  }
  #######################################################
  #### OVERALL TOTAL COST ###############################
  #######################################################
  df$Total_Cost_Undisc <- rowSums(df[,c("Treatment_Cost", "AE_Cost", "HealthState_Cost")], na.rm = TRUE)
  df$Total_Cost_Disc <- rowSums(df[,c("Treatment_Cost_Disc", "AE_Cost_Disc", "HealthState_Cost_Disc")], na.rm = TRUE)
  df
}







