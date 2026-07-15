#########################################################
#### Sample PSA Parameters (FINAL CLEAN VERSION) ########
#########################################################
sample_psa_parameters <- function(
    health_state_settings,
    ae_object,
    int_therapy_settings,
    comp_therapy_settings,
    seed = NULL
){
  if(!is.null(seed)){
    set.seed(seed)
  }
  sampled <- list()
  #######################################################
  #### SE helper ########################################
  #######################################################
  derive_se <- function(mean, se, distribution){
    if(!is.null(se) && !is.na(se) && se > 0){
      return(se)
    }
    switch(
      distribution,
      beta = sqrt(abs(mean * (1 - mean))) / 4,
      gamma = abs(mean) / 10,
      normal = abs(mean) / 10,
      lognormal = abs(mean) / 10,
      abs(mean) / 10
    )
  }
  #######################################################
  #### AE DISUTILITY SE HELPER ###########################
  #######################################################
  derive_ae_disutility_se <- function(
    mean,
    se = NA,
    lci = NA,
    uci = NA
  ){
    #####################################################
    #### 1. Use supplied SE ##############################
    #####################################################
    if(!is.na(se) && se > 0){
      return(se)
    }
    #####################################################
    #### 2. Derive from 95% CI ###########################
    #####################################################
    if(!is.na(lci) && !is.na(uci)){
      return(abs(uci - lci) / 3.92)
    }
    #####################################################
    #### 3. Default assumption ###########################
    #####################################################
    return(abs(mean) / 10)
  }
  
  #######################################################
  #### Sampler ##########################################
  #######################################################
  sample_parameter <- function(mean, distribution, se){
    switch(
      distribution,
      normal = sample_normal_from_mean_se(mean, se),
      beta   = sample_beta_from_mean_se(mean, se),
      gamma  = sample_gamma_from_mean_se(mean, se),
      lognormal = sample_lognormal_from_mean_se(mean, se),
      mean
    )
  }
  #######################################################
  #### UTILITIES (BETA) #################################
  #######################################################
  sampled$pf_utility <- sample_parameter(
    health_state_settings$utilities$PF$mean,
    "beta",
    derive_se(
      health_state_settings$utilities$PF$mean,
      health_state_settings$utilities$PF$se,
      "beta"
    )
  )
  sampled$pd_utility <- sample_parameter(
    health_state_settings$utilities$PD$mean,
    "beta",
    derive_se(
      health_state_settings$utilities$PD$mean,
      health_state_settings$utilities$PD$se,
      "beta"
    )
  )
  #######################################################
  #### COSTS (GAMMA - STRICT) ##########################
  #######################################################
  sampled$pf_first_cost <- max(0,
                               sample_parameter(
                                 health_state_settings$costs$PF_First$mean,
                                 "gamma",
                                 derive_se(
                                   health_state_settings$costs$PF_First$mean,
                                   health_state_settings$costs$PF_First$se,
                                   "gamma"
                                 )
                               )
  )
  sampled$pf_cost <- max(0,
                         sample_parameter(
                           health_state_settings$costs$PF$mean,
                           "gamma",
                           derive_se(
                             health_state_settings$costs$PF$mean,
                             health_state_settings$costs$PF$se,
                             "gamma"
                           )
                         )
  )
  sampled$pd_cost <- max(0,
                         sample_parameter(
                           health_state_settings$costs$PD$mean,
                           "gamma",
                           derive_se(
                             health_state_settings$costs$PD$mean,
                             health_state_settings$costs$PD$se,
                             "gamma"
                           )
                         )
  )
  #######################################################
  #### TREATMENT COSTS (GAMMA) ##########################
  #######################################################
  sampled$intervention_treatment_costs <- lapply(
    int_therapy_settings,
    function(therapy){
      max(0,
          sample_parameter(
            therapy$cost$mean,
            "gamma",
            derive_se(
              therapy$cost$mean,
              therapy$cost$se,
              "gamma"
            )
          )
      )
    }
  )
  sampled$comparator_treatment_costs <- lapply(
    comp_therapy_settings,
    function(therapy){
      max(0,
          sample_parameter(
            therapy$cost$mean,
            "gamma",
            derive_se(
              therapy$cost$mean,
              therapy$cost$se,
              "gamma"
            )
          )
      )
    }
  )
  #######################################################
  #### AE BLOCK (FIXED CLEAN PSA STRUCTURE) #############
  #######################################################
  ae_df <- ae_object$data
  n_ae <- nrow(ae_df)
  sampled$ae <- list(
    cost = numeric(n_ae),
    prob_intervention = numeric(n_ae),
    prob_comparator = numeric(n_ae),
    disutility = numeric(n_ae)
  )
  for(i in seq_len(n_ae)){
    #####################################################
    #### COST (GAMMA) ###################################
    #####################################################
    sampled$ae$cost[i] <- max(
      0,
      sample_parameter(
        ae_df$cost[i],
        "gamma",
        derive_se(ae_df$cost[i], ae_df$cost[i] / 10, "gamma")
      )
    )
    #####################################################
    #### PROB INTERVENTION (NORMAL, CLIPPED) ###########
    #####################################################
    sampled$ae$prob_intervention[i] <- min(1, max(0,
                                                  sample_parameter(
                                                    ae_df$prob_intervention[i],
                                                    "beta",
                                                    derive_se(ae_df$prob_intervention[i],
                                                              ae_df$prob_intervention[i] / 4,
                                                              "beta")
                                                  )
    ))
    #####################################################
    #### PROB COMPARATOR (NORMAL, CLIPPED) ##############
    #####################################################
    sampled$ae$prob_comparator[i] <- min(1, max(0,
                                                sample_parameter(
                                                  ae_df$prob_comparator[i],
                                                  "beta",
                                                  derive_se(ae_df$prob_comparator[i],
                                                            ae_df$prob_comparator[i] / 4,
                                                            "beta")
                                                )
    ))
    
    #####################################################
    #### DISUTILITY (BETA) ##############################
    #####################################################
    mean_disutility <- abs(ae_df$disutility[i])
    se_disutility <- derive_ae_disutility_se(
      mean = mean_disutility,
      se =
        if("SE_disutility" %in% names(ae_df))
          abs(ae_df$SE_disutility[i])
      else
        NA,
      lci =
        if("LCI_disutility" %in% names(ae_df))
          abs(ae_df$LCI_disutility[i])
      else
        NA,
      uci =
        if("UCI_disutility" %in% names(ae_df))
          abs(ae_df$UCI_disutility[i])
      else
        NA
    )
    
    sampled$ae$disutility[i] <-
      -sample_parameter(
        mean_disutility,
        "beta",
        se_disutility
      )
  }
  
  #######################################################
  #### AE DISUTILITY IMPACT ON QALY ####################
  #######################################################
  
  sampled$total_qaly_penalty <- sum(abs(sampled$ae$disutility))

  #######################################################
  #### DEBUG ###########################################
  #######################################################
  
  cat("\n=====================================\n")
  cat("PSA PARAMETER SAMPLE (CLEAN)\n")
  cat("=====================================\n")
  cat("PF Utility:", round(sampled$pf_utility, 4), "\n")
  cat("PD Utility:", round(sampled$pd_utility, 4), "\n")
  cat("PF Cost:", round(sampled$pf_cost, 2), "\n")
  cat("PD Cost:", round(sampled$pd_cost, 2), "\n")
  cat("AE Cost (first 3):", head(sampled$ae$cost, 3), "\n")
  
  return(sampled)
}

