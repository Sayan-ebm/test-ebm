#########################################################
#### Build Therapy Settings #############################
#########################################################
#########################################################
#### Build Therapy Settings #############################
#########################################################
build_therapy_settings <- function(
    therapy_name,
    cost_per_cycle,
    cycle_length_weeks,
    max_cycles_flag,
    max_cycles = NULL,
    cost_change_flag,
    cost_change_cycle = NULL,
    cost_after_change = NULL,
    discount = 0,
    #####################################################
    #### PSA Inputs #####################################
    #####################################################
    cost_distribution = "gamma",
    cost_se = NULL
){
  #######################################################
  #### Sanitize cost_per_cycle ##########################
  #######################################################
  # Guarantees a negative/blank cost can never reach the
  # model, regardless of any UI-side timing issue.
  if (is.null(cost_per_cycle) || length(cost_per_cycle) == 0 ||
      is.na(cost_per_cycle) || cost_per_cycle < 0) {
    warning(
      paste0(
        therapy_name %||% "Unnamed therapy",
        " - Cost per cycle was missing/negative (",
        if (is.null(cost_per_cycle) || length(cost_per_cycle) == 0) "NULL" else cost_per_cycle,
        "); using 0 instead."
      )
    )
    cost_per_cycle <- 0
  }
  #######################################################
  #### Build ############################################
  #######################################################
  list(
    #####################################################
    #### General ########################################
    #####################################################
    therapy_name = therapy_name,
    #####################################################
    #### Base-case cost #################################
    #####################################################
    cost_per_cycle = cost_per_cycle,
    #####################################################
    #### PSA information ################################
    #####################################################
    cost = list(
      mean = cost_per_cycle,
      se = cost_se,
      distribution = cost_distribution
    ),
    #####################################################
    #### Scheduling #####################################
    #####################################################
    cycle_length_weeks = cycle_length_weeks,
    #####################################################
    #### Treatment duration #############################
    #####################################################
    max_cycles_flag = max_cycles_flag,
    max_cycles = max_cycles,
    #####################################################
    #### Cost change ####################################
    #####################################################
    cost_change_flag = cost_change_flag,
    cost_change_cycle = cost_change_cycle,
    cost_after_change = cost_after_change,
    #####################################################
    #### Discount #######################################
    #####################################################
    discount = discount
  )
}
#########################################################
#### Validate Therapy Settings ##########################
#########################################################
validate_therapy_settings <- function(
    therapy_settings
){
  #######################################################
  #### Therapy Name #####################################
  #######################################################
  if(
    is.null(therapy_settings$therapy_name) ||
    therapy_settings$therapy_name == ""
  ){
    stop("Therapy name is missing")
  }
  #######################################################
  #### Cost #############################################
  #######################################################
  cost_value <-
    if(!is.null(therapy_settings$cost)){
      therapy_settings$cost$mean
    }else{
      therapy_settings$cost_per_cycle
    }
  if(is.null(cost_value) || cost_value < 0){
    stop(
      paste(
        therapy_settings$therapy_name,
        "- Cost per cycle cannot be negative"
      )
    )
  }
  #######################################################
  #### Cycle Length #####################################
  #######################################################
  if(
    therapy_settings$cycle_length_weeks <= 0
  ){
    stop(
      paste(
        therapy_settings$therapy_name,
        "- Cycle length must be greater than zero"
      )
    )
  }
  #######################################################
  #### Maximum Cycles ###################################
  #######################################################
  if(
    therapy_settings$max_cycles_flag == "Yes"
  ){
    if(
      is.null(therapy_settings$max_cycles) ||
      therapy_settings$max_cycles <= 0
    ){
      stop(
        paste(
          therapy_settings$therapy_name,
          "- Invalid maximum cycles"
        )
      )
    }
  }
  #######################################################
  #### Cost Change ######################################
  #######################################################
  if(
    therapy_settings$cost_change_flag == "Yes"
  ){
    if(
      is.null(therapy_settings$cost_change_cycle) ||
      therapy_settings$cost_change_cycle <= 0
    ){
      stop(
        paste(
          therapy_settings$therapy_name,
          "- Invalid cost change cycle"
        )
      )
    }
    if(
      is.null(therapy_settings$cost_after_change)
    ){
      stop(
        paste(
          therapy_settings$therapy_name,
          "- Cost after change is missing"
        )
      )
    }
  }
  #######################################################
  #### Discount #########################################
  #######################################################
  if(
    therapy_settings$discount < 0 ||
    therapy_settings$discount > 100
  ){
    stop(
      paste(
        therapy_settings$therapy_name,
        "- Discount must be between 0 and 100"
      )
    )
  }
  TRUE
}

#########################################################
#### Build Therapy Object ###############################
#########################################################
build_therapy_object <- function(
    therapy_settings
){
  validate_therapy_settings(
    therapy_settings
  )
  list(
    settings = therapy_settings,
    valid = TRUE
  )
}

#########################################################
#### Print Therapy Summary ##############################
#########################################################
print_therapy_summary <- function(
    therapy_object
){
  cat("\n====================\n")
  cat("THERAPY SETTINGS\n")
  cat("====================\n")
  cat("\nTherapy:",
      therapy_object$settings$therapy_name)
  cat("\nCost per cycle:",
      therapy_object$settings$cost$mean)
  cat("\nDistribution:",
      therapy_object$settings$cost$distribution)
  cat("\nSE:",
      therapy_object$settings$cost$se)
  cat("\nCycle length:",
      therapy_object$settings$cycle_length_weeks)
  cat("\nDiscount:",
      therapy_object$settings$discount)
  cat("\nValid:",
      therapy_object$valid)
  cat("\n====================\n")
}

