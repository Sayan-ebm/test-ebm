#########################################################
#### Build AE Settings ##################################
#########################################################
build_ae_settings <- function(
    ae_df,
    include_ae = TRUE
){
  required_cols <- c("Event", "prob_comparator", "N_comparator", "prob_intervention", "N_intervention", "cost", "disutility")
  missing_cols <- setdiff(required_cols, names(ae_df))
  if(length(missing_cols) > 0){
    stop(
      paste(
        "Missing AE columns:",
        paste(missing_cols, collapse = ", "))
    )
  }
  list(
    ae_df = ae_df,
    include_ae = include_ae
  )
}

#########################################################
#### Build AE Object ####################################
#########################################################
build_ae_object <- function(ae_settings){
  ae_df <- ae_settings$ae_df
  # ALWAYS ensure numeric vectors (critical for PSA)
  ae_df$prob_intervention <- as.numeric(ae_df$prob_intervention)
  ae_df$prob_comparator   <- as.numeric(ae_df$prob_comparator)
  ae_df$cost              <- as.numeric(ae_df$cost)
  ae_df$disutility        <- as.numeric(ae_df$disutility)
  
  #########################################################
  #### OPTIONAL PSA UNCERTAINTY COLUMNS ###################
  #########################################################
  
  optional_numeric <- c(
    "SE_disutility",
    "LCI_disutility",
    "UCI_disutility"
  )
  
  for(col in optional_numeric){
    
    if(!(col %in% names(ae_df))){
      ae_df[[col]] <- NA_real_
    } else {
      ae_df[[col]] <- as.numeric(ae_df[[col]])
    }
    
  }
  
  if(!ae_settings$include_ae){
    ae_df$prob_intervention <- 0
    ae_df$prob_comparator   <- 0
    ae_df$cost             <- 0
    ae_df$disutility       <- 0
  }
  list(data = ae_df)
}

#########################################################
#### Briggs Conversion ##################################
#########################################################
probability_to_hazard <- function(
    probability
){
  ifelse(
    probability <= 0,
    0,
    -log(1 - probability)
  )
}

#########################################################
#### Hazard To Monthly Probability ######################
#########################################################
hazard_to_monthly_probability <- function(
    hazard_rate,
    cycle_length_months = 1
){
  1 - exp(-hazard_rate *cycle_length_months)
}

#########################################################
#### Determine AE Exposure Window #######################
#########################################################
get_ae_exposure_window <- function(
    therapy_list
){
  max_model_month <- 0
  treatment_until_progression <- FALSE
  for(therapy in therapy_list){
    #####################################################
    #### FIXED DURATION #################################
    #####################################################
    if(
      therapy$settings$max_cycles_flag == "Yes"
    ){
      if(
        nrow(
          therapy$administration_schedule
        ) > 0
      ){
        max_model_month <-
          max(
            max_model_month,
            max(
              therapy$
                administration_schedule$
                model_month,
              na.rm = TRUE
            )
          )
      }
    } else {
      ###################################################
      #### CONTINUOUS THERAPY ###########################
      ###################################################
      treatment_until_progression <- TRUE
    }
  }
  list(
    max_model_month = max_model_month,
    treatment_until_progression = treatment_until_progression
  )
}

#########################################################
#### AE Exposure Duration ###############################
#########################################################
# get_ae_exposure_months <- function(
#     therapy_list,
#     horizon_months
# ){
#   has_ttp <- any(
#     sapply(
#       therapy_list,
#       function(x)
#         x$settings$max_cycles_flag == "No"
#     )
#   )
#   if(has_ttp){
#     return(horizon_months)
#   }
#   max(
#     sapply(
#       therapy_list,
#       function(x){
#         #################################################
#         #### ACTUAL TREATMENT EXPOSURE ##################
#         #################################################
#         exposure_weeks <- (x$settings$max_cycles - 1) * x$settings$cycle_length_weeks
#         exposure_weeks * 7 / (365.25 / 12)
#       }
#     ),
#     na.rm = TRUE
#   )
# }
#########################################################
#### AE Exposure Duration ###############################
#########################################################
get_ae_exposure_months <- function(
    therapy_list,
    horizon_months
){
  #######################################################
  #### Treatment Until Progression ######################
  #######################################################
  has_ttp <- any(
    sapply(
      therapy_list,
      function(x)
        x$settings$max_cycles_flag == "No"
    )
  )
  if(has_ttp){
    return(horizon_months)
  }
  #######################################################
  #### Fixed Duration ###################################
  #######################################################
  max(
    sapply(
      therapy_list,
      function(x){
        
        max(
          x$administration_schedule$model_month,
          na.rm = TRUE
        ) + 1
      }
    ),
    na.rm = TRUE
  )
}

calculate_monthly_ae_impact <- function(
    ae_object,
    therapy_list,
    occupancy_df,
    horizon_months,
    arm = c("intervention", "comparator")
){
  
  arm <- match.arg(arm)
  
  ae_df <- ae_object$data
  
  prob_col <- if(arm == "intervention"){
    "prob_intervention"
  } else {
    "prob_comparator"
  }
  
  #######################################################
  #### EXPOSURE DURATION ################################
  #######################################################
  
  ae_duration_months <- get_ae_exposure_months(
    therapy_list = therapy_list,
    horizon_months = horizon_months
  )
  
  exposure_intervals <- ae_duration_months
  
  #######################################################
  #### PROB → HAZARD ####################################
  #######################################################
  
  prob <- pmin(pmax(ae_df[[prob_col]], 0), 0.999999)
  ae_df$cumulative_hazard <- probability_to_hazard(prob)
  ae_df$monthly_hazard <- ae_df$cumulative_hazard / exposure_intervals
  
  #######################################################
  #### EXPECTED EVENT IMPACT (PER PATIENT) ##############
  #######################################################
  
  event_risk <- (1 - exp(-ae_df$monthly_hazard))
  
  event_costs <- event_risk * ae_df$cost
  event_disutilities <- event_risk * ae_df$disutility
  
  #######################################################
  #### COLLAPSE ACROSS EVENTS (KEY STEP) #################
  #######################################################
  
  expected_monthly_cost <- sum(event_costs, na.rm = TRUE)
  expected_monthly_disutility <- sum(event_disutilities, na.rm = TRUE)
  
  #######################################################
  #### TIME STRUCTURE ####################################
  #######################################################
  
  monthly_costs <- rep(0, horizon_months + 1)
  monthly_disutility <- rep(0, horizon_months + 1)
  
  idx <- seq_len(min(exposure_intervals + 1, horizon_months + 1))
  
  #######################################################
  #### APPLY OVER TIME (NO OCCUPANCY HERE) ##############
  #######################################################
  
  monthly_costs[idx] <- expected_monthly_cost
  monthly_disutility[idx] <- expected_monthly_disutility
  
  #######################################################
  #### VALIDATION ########################################
  #######################################################
  
  raw_expected_cost <- sum(ae_df[[prob_col]] * ae_df$cost, na.rm = TRUE)
  raw_expected_disutility <- sum(ae_df[[prob_col]] * ae_df$disutility, na.rm = TRUE)
  
  cat("\n==============================")
  cat("\nAE CHECK (FIXED MODEL)")
  cat("\n==============================")
  cat("\nAE duration (months): ", round(ae_duration_months, 2))
  cat("\nRaw expected cost: ", round(raw_expected_cost, 2))
  cat("\nModelled AE cost: ", round(sum(monthly_costs), 2))
  cat("\nRaw expected disutility: ", round(raw_expected_disutility, 5))
  cat("\nModelled AE disutility: ", round(sum(monthly_disutility), 5))
  cat("\n==============================\n")
  
  #######################################################
  #### AUDIT ############################################
  #######################################################
  
  audit_df <- data.frame(
    Month = 0:horizon_months,
    AE_Cost = monthly_costs,
    AE_Disutility = monthly_disutility
  )
  
  #######################################################
  #### RETURN ###########################################
  #######################################################
  
  list(
    monthly_costs = monthly_costs,
    monthly_disutility = monthly_disutility,
    total_cost = sum(monthly_costs, na.rm = TRUE),
    total_disutility = sum(monthly_disutility, na.rm = TRUE),
    audit_df = audit_df,
    event_costs = event_costs,
    event_disutilities = event_disutilities
  )
}

#########################################################
#### Build Arm AE Object ################################
#########################################################
build_arm_ae_object <- function(
    arm_treatment_object,
    ae_object,
    occupancy_df,
    horizon_months,
    discount_rate_cost = 0,
    arm = c("intervention", "comparator")
){
  arm <- match.arg(arm)
  #######################################################
  #### SINGLE ARM CALCULATION ###########################
  #######################################################
  ae_results <- calculate_monthly_ae_impact(
      ae_object = ae_object,
      therapy_list = arm_treatment_object$therapies,
      occupancy_df = occupancy_df,
      horizon_months = horizon_months,
      arm = arm
    )
  #######################################################
  #### DISCOUNT AE COSTS #################################
  #######################################################
  monthly_costs_discounted <-
    apply_discount_vector(
      ae_results$monthly_costs,
      rate = discount_rate_cost,
      cycle_length_months = 1
    )
  #######################################################
  #### RETURN ###########################################
  #######################################################
  list(
    monthly_costs = ae_results$monthly_costs,
    monthly_costs_discounted = monthly_costs_discounted,
    monthly_disutility = ae_results$monthly_disutility,
    total_cost = sum(ae_results$monthly_costs, na.rm = TRUE),
    total_cost_discounted = sum(monthly_costs_discounted, na.rm = TRUE),
    total_disutility = sum(ae_results$monthly_disutility, na.rm = TRUE),
    event_costs = ae_results$event_costs,
    event_disutilities = ae_results$event_disutilities,
    audit_df = ae_results$audit_df
  )
}




