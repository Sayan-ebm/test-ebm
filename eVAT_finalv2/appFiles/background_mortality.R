#########################################################
#### Apply Background Mortality To OS ###################
#########################################################
apply_background_mortality <- function(
    os_curve,
    start_age,
    mortality_table,
    apply_bgm,
    male_proportion
){
  #######################################################
  #### Skip Background Mortality ########################
  #######################################################
  if(!isTRUE(apply_bgm)){
    return(
      list(
        os_curve = os_curve,
        diagnostics = NULL,
        life_table = NULL
      )
    )
  }
  if(is.null(mortality_table)){
    return(
      list(
        os_curve = os_curve,
        diagnostics = NULL,
        life_table = NULL
      )
    )
  }
  #######################################################
  #### Validate Inputs ##################################
  #######################################################
  required_cols <- c(
    "age",
    "male_qx",
    "female_qx"
  )
  
  if(!all(required_cols %in% names(mortality_table))){
    stop(
      "Background mortality table must contain columns: age, male_qx and female_qx."
    )
  }
  #######################################################
  #### Validate Male Proportion #########################
  #######################################################
  if(is.null(male_proportion)){
    stop(
      "Male proportion must be supplied."
    )
  }
  
  male_proportion <- as.numeric(
    male_proportion
  )
  
  if(
    is.na(male_proportion) ||
    male_proportion < 0 ||
    male_proportion > 1
  ){
    stop(
      "Male proportion must lie between 0 and 1."
    )
  }
  
  female_proportion <- 1 - male_proportion
  
  #######################################################
  #### Sort Mortality Table #############################
  #######################################################
  mortality_table <-
    mortality_table[
      order(mortality_table$age),
      ,
      drop = FALSE
    ]
  
  #######################################################
  #### Fixed Sex Weights ###############################
  #######################################################
  mortality_table$male_weight <-
    male_proportion
  
  mortality_table$female_weight <-
    female_proportion
  
  #######################################################
  #### General Population Mortality #####################
  #######################################################
  mortality_table$general_qx <-
    mortality_table$male_weight *
    mortality_table$male_qx +
    mortality_table$female_weight *
    mortality_table$female_qx
  
  #######################################################
  #### Convert Annual To Monthly ########################
  #######################################################
  mortality_table$monthly_qx <-
    1 -
    (
      1 -
        mortality_table$general_qx
    )^(1/12)
  
  #######################################################
  #### Maximum Attained Age #############################
  #######################################################
  max_age <-
    max(
      mortality_table$age,
      na.rm = TRUE
    )
  
  #######################################################
  #### Map Model Cycles To Attained Age #################
  #######################################################
  n_cycles <-
    length(
      os_curve$surv
    )
  
  # attained_age <-
  #   floor(
  #     start_age +
  #       (os_curve$time / 12)
  #   )
  
  attained_age <- ifelse(
    os_curve$time == 0,
    start_age,
    floor(start_age + (os_curve$time - 1) / 12)
  )
  
  lookup_age <-
    pmin(
      attained_age,
      max_age
    )
  
  #######################################################
  #### Lookup Background Mortality ######################
  #######################################################
  lookup_index <-
    match(
      lookup_age,
      mortality_table$age
    )
  
  if(any(is.na(lookup_index))){
    stop(
      "One or more attained ages could not be matched to the background mortality table."
    )
  }
  
  #######################################################
  #### Extract Background Mortality #####################
  #######################################################
  male_qx_used <-
    mortality_table$male_qx[
      lookup_index
    ]
  
  female_qx_used <-
    mortality_table$female_qx[
      lookup_index
    ]
  
  male_weight_used <-
    mortality_table$male_weight[
      lookup_index
    ]
  
  female_weight_used <-
    mortality_table$female_weight[
      lookup_index
    ]
  
  general_qx_used <-
    mortality_table$general_qx[
      lookup_index
    ]
  
  monthly_qx_used <-
    mortality_table$monthly_qx[
      lookup_index
    ]
  
  #######################################################
  #### Original Overall Survival ########################
  #######################################################
  original_surv <- os_curve$surv
  #######################################################
  #### Disease Mortality From Original OS ###############
  #######################################################
  disease_mortality <-
    numeric(
      n_cycles
    )
  
  #######################################################
  #### Calculate Disease Mortality ######################
  #######################################################
  disease_mortality[1] <- 0
  
  for(i in 2:n_cycles){
    
    if(
      original_surv[i - 1] > 0
    ){
      
      disease_mortality[i] <-
        (
          original_surv[i - 1] -
            original_surv[i]
        ) /
        original_surv[i - 1]
      
    }else{
      
      disease_mortality[i] <- 0
      
    }
    
  }
  
  #######################################################
  #### Numerical Safeguards #############################
  #######################################################
  disease_mortality <-
    pmax(
      disease_mortality,
      0
    )
  
  disease_mortality <-
    pmin(
      disease_mortality,
      1
    )
  
  #######################################################
  #### Mortality Applied In Model ########################
  #######################################################
  mortality_used <-
    pmax(
      disease_mortality,
      monthly_qx_used
    )
  
  #######################################################
  #### Reconstruct Adjusted Overall Survival ###########
  #######################################################
  adjusted_surv <-
    numeric(
      n_cycles
    )
  
  #######################################################
  #### Starting Survival ################################
  #######################################################
  adjusted_surv[1] <-
    original_surv[1]
  
  #######################################################
  #### Apply Background Mortality ########################
  #######################################################
  for(i in 2:n_cycles){
    
    adjusted_surv[i] <-
      adjusted_surv[i - 1] *
      (
        1 -
          mortality_used[i]
      )
    
  }
  
  #######################################################
  #### Numerical Safeguards #############################
  #######################################################
  adjusted_surv <-
    pmax(
      adjusted_surv,
      0
    )
  
  adjusted_surv <-
    pmin(
      adjusted_surv,
      1
    )
  
  adjusted_surv <-
    cummin(
      adjusted_surv
    )
  
  #######################################################
  #### Update Overall Survival Curve ####################
  #######################################################
  os_curve$surv <-
    adjusted_surv
  #######################################################
  #### Diagnostics ######################################
  #######################################################
  diagnostics <-
    data.frame(
      cycle = 0:(n_cycles - 1),
      model_month = os_curve$time,
      attained_age = attained_age,
      male_qx = male_qx_used,
      female_qx = female_qx_used,
      male_weight = male_weight_used,
      female_weight = female_weight_used,
      general_population_qx = general_qx_used,
      monthly_population_qx = monthly_qx_used,
      disease_mortality = disease_mortality,
      mortality_used = mortality_used,
      original_os_survival = original_surv,
      adjusted_lifetable_os_survival = adjusted_surv,
      stringsAsFactors = FALSE
    )
  
  #######################################################
  #### Update Output ####################################
  #######################################################
  os_curve$surv <-
    adjusted_surv
  
  #######################################################
  #### Return ###########################################
  #######################################################
  return(
    list(
      os_curve = os_curve,
      diagnostics = diagnostics,
      life_table = mortality_table
    )
  )
  
}
  
  