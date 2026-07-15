run_single_iteration <- function(
    sampled_parameters,
    int_occ,
    comp_occ,
    int_therapy_settings_list,
    comp_therapy_settings_list,
    ae_data,
    health_state_settings,
    discount_rate_cost,
    discount_rate_qaly,
    wtp
){
  #########################################################
  #### SAFE COPY (avoid mutation across PSA iterations)
  #########################################################
  hs_settings      <- rlang::duplicate(health_state_settings)
  int_therapy_psa  <- rlang::duplicate(int_therapy_settings_list)
  comp_therapy_psa <- rlang::duplicate(comp_therapy_settings_list)
  ae_iteration     <- rlang::duplicate(ae_data)
  #########################################################
  #### HEALTH STATES PARAMETERS
  #########################################################
  hs_settings$utilities$PF$mean <- sampled_parameters$pf_utility
  hs_settings$utilities$PD$mean <- sampled_parameters$pd_utility
  
  hs_settings$costs$PF_First$mean <- sampled_parameters$pf_first_cost
  hs_settings$costs$PF$mean <- sampled_parameters$pf_cost
  hs_settings$costs$PD$mean <- sampled_parameters$pd_cost
  #########################################################
  #### TREATMENTS
  #########################################################
  if (length(int_therapy_psa) > 0) {
    for (i in seq_along(int_therapy_psa)) {
      int_therapy_psa[[i]]$cost$mean <- sampled_parameters$intervention_treatment_costs[[i]]
      int_therapy_psa[[i]]$cost_per_cycle <- sampled_parameters$intervention_treatment_costs[[i]]
    }
  }
  
  if (length(comp_therapy_psa) > 0) {
    for (i in seq_along(comp_therapy_psa)) {
      comp_therapy_psa[[i]]$cost$mean <- sampled_parameters$comparator_treatment_costs[[i]]
      comp_therapy_psa[[i]]$cost_per_cycle <- sampled_parameters$comparator_treatment_costs[[i]]
    }
  }
  #########################################################
  #### AE (SAFE VECTORISED FIX)
  #########################################################
  # if (!is.null(ae_iteration$data) && nrow(ae_iteration$data) > 0) {
  #   ae_iteration$data$cost <- pmax(0, sampled_parameters$ae$cost)
  #   ae_iteration$data$disutility <-
  #     pmax(-1, pmin(0, sampled_parameters$ae$disutility))
  #   ae_iteration$data$prob_intervention <- pmin(1, pmax(0, sampled_parameters$ae$prob_intervention))
  #   ae_iteration$data$prob_comparator <- pmin(1, pmax(0, sampled_parameters$ae$prob_comparator))
  # }
  
  if (!is.null(ae_iteration$data) && nrow(ae_iteration$data) > 0) {
    if (!is.null(sampled_parameters$ae$cost))
      ae_iteration$data$cost <-
        pmax(0, sampled_parameters$ae$cost)
    if (!is.null(sampled_parameters$ae$disutility))
      ae_iteration$data$disutility <-
        pmax(-1, pmin(0, sampled_parameters$ae$disutility))
    
    if (!is.null(sampled_parameters$ae$prob_intervention))
      ae_iteration$data$prob_intervention <-
        pmin(1, pmax(0, sampled_parameters$ae$prob_intervention))
    
    if (!is.null(sampled_parameters$ae$prob_comparator))
      ae_iteration$data$prob_comparator <-
        pmin(1, pmax(0, sampled_parameters$ae$prob_comparator))
  }
  #########################################################
  #### BUILD ARMS
  #########################################################
  int_treatment <- build_arm_treatment_object(
    occupancy_df = int_occ,
    therapy_settings_list = int_therapy_psa,
    horizon_months = max(int_occ$time),
    discount_rate_cost = discount_rate_cost
  )
  comp_treatment <- build_arm_treatment_object(
    occupancy_df = comp_occ,
    therapy_settings_list = comp_therapy_psa,
    horizon_months = max(comp_occ$time),
    discount_rate_cost = discount_rate_cost
  )
  #########################################################
  #### AE MODULE
  #########################################################
  int_ae <- build_arm_ae_object(
    arm_treatment_object = int_treatment,
    ae_object = ae_iteration,
    occupancy_df = int_occ,
    horizon_months = max(int_occ$time),
    discount_rate_cost = discount_rate_cost,
    arm = "intervention"
  )
  comp_ae <- build_arm_ae_object(
    arm_treatment_object = comp_treatment,
    ae_object = ae_iteration,
    occupancy_df = comp_occ,
    horizon_months = max(comp_occ$time),
    discount_rate_cost = discount_rate_cost,
    arm = "comparator"
  )
  #########################################################
  #### HEALTH STATES
  #########################################################
  int_health_state <- build_arm_health_state_object(
    occupancy_df = int_occ,
    health_state_settings = hs_settings,
    ae_monthly_disutility = int_ae$monthly_disutility,
    discount_rate_cost = discount_rate_cost,
    discount_rate_qaly = discount_rate_qaly
  )
  comp_health_state <- build_arm_health_state_object(
    occupancy_df = comp_occ,
    health_state_settings = hs_settings,
    ae_monthly_disutility = comp_ae$monthly_disutility,
    discount_rate_cost = discount_rate_cost,
    discount_rate_qaly = discount_rate_qaly
  )
  #########################################################
  #### COSTS
  #########################################################
  cost_intervention <-
    int_treatment$total_cost_discounted +
    int_ae$total_cost_discounted +
    int_health_state$cost$discounted
  cost_comparator <-
    comp_treatment$total_cost_discounted +
    comp_ae$total_cost_discounted +
    comp_health_state$cost$discounted
  #########################################################
  #### OUTCOMES
  #########################################################
  qaly_intervention <- int_health_state$qaly$discounted
  qaly_comparator   <- comp_health_state$qaly$discounted
  ly_intervention <- int_health_state$lys$discounted
  ly_comparator   <- comp_health_state$lys$discounted
  incremental_cost <- cost_intervention - cost_comparator
  incremental_qaly <- qaly_intervention - qaly_comparator
  incremental_ly   <- ly_intervention - ly_comparator
  icer <- if (abs(incremental_qaly) < 1e-10) NA_real_
  else incremental_cost / incremental_qaly
#  nmb <- incremental_qaly * wtp - incremental_cost
  #########################################################
  #### evLY ###############################################
  #########################################################
  evly_comparator <- qaly_comparator
  evly_intervention <- evly_comparator + 0.851 * incremental_ly
  incremental_evly <- evly_intervention - evly_comparator
  #########################################################
  #### HYT ################################################
  #########################################################
  hyt_comparator <- qaly_comparator
  hyt_intervention <- qaly_intervention + incremental_evly
  incremental_hyt <- hyt_intervention - hyt_comparator
  #########################################################
  #### AE DISUTILITY (NEW)
  #########################################################
  ae_disutility_intervention <- sum(int_ae$monthly_disutility, na.rm = TRUE)
  ae_disutility_comparator   <- sum(comp_ae$monthly_disutility, na.rm = TRUE)
  incremental_ae_disutility  <- ae_disutility_intervention - ae_disutility_comparator
  #########################################################
  #### NMB (FULL + INCREMENTAL)
  #########################################################
  nmb_intervention <- (qaly_intervention * wtp) - cost_intervention
  nmb_comparator   <- (qaly_comparator * wtp) - cost_comparator
  incremental_nmb <- nmb_intervention - nmb_comparator
  # 🔥 SINGLE SUMMARY NMB (ICER-style metric)
  nmb <- (incremental_qaly * wtp) - incremental_cost
  #########################################################
  #### COST BREAKDOWN
  #########################################################
  cost_breakdown <- data.frame(
    Cost_Component = c("Treatment", "AE", "Health State", "Total"),
    Intervention = c(
      int_treatment$total_cost_discounted,
      int_ae$total_cost_discounted,
      int_health_state$cost$discounted,
      cost_intervention
    ),
    Comparator = c(
      comp_treatment$total_cost_discounted,
      comp_ae$total_cost_discounted,
      comp_health_state$cost$discounted,
      cost_comparator
    )
  )
  cost_breakdown$Incremental <- cost_breakdown$Intervention - cost_breakdown$Comparator
  #########################################################
  #### PSA BASE CASE TABLE
  #########################################################
  base_case_results <- data.frame(
    Treatment = c("Comparator","Intervention"),
    Total_Cost = c(cost_comparator, cost_intervention),
    Total_LY = c(ly_comparator, ly_intervention),
    Total_QALY = c(qaly_comparator, qaly_intervention),
    Incremental_Cost = c( NA, incremental_cost),
    Incremental_QALY = c(NA, incremental_qaly),
    Incremental_LY = c(NA,incremental_ly),
    ICER = c(NA,icer),
    NMB = c(nmb_comparator,nmb_intervention),
    stringsAsFactors = FALSE
  )
  #########################################################
  #### RETURN #############################################
  #########################################################
  outcomes <- data.frame(
    #######################################################
    #### PSA INPUTS #######################################
    #######################################################
    pf_utility = sampled_parameters$pf_utility,
    pd_utility = sampled_parameters$pd_utility,
    pf_first_cost = sampled_parameters$pf_first_cost,
    pf_cost       = sampled_parameters$pf_cost,
    pd_cost       = sampled_parameters$pd_cost,
    #######################################################
    #### COSTS ############################################
    #######################################################
    treatment_cost_intervention = int_treatment$total_cost_discounted,
    treatment_cost_comparator   = comp_treatment$total_cost_discounted,
    ae_cost_intervention = int_ae$total_cost_discounted,
    ae_cost_comparator   = comp_ae$total_cost_discounted,
    health_state_cost_intervention =
      int_health_state$cost$discounted,
    health_state_cost_comparator =
      comp_health_state$cost$discounted,
    total_cost_intervention = cost_intervention,
    total_cost_comparator   = cost_comparator,
    #######################################################
    #### HEALTH OUTCOMES ##################################
    #######################################################
    qaly_intervention = qaly_intervention,
    qaly_comparator   = qaly_comparator,
    ly_intervention = ly_intervention,
    ly_comparator   = ly_comparator,
    #######################################################
    #### INCREMENTALS #####################################
    #######################################################
    incremental_cost = incremental_cost,
    incremental_qaly = incremental_qaly,
    incremental_ly   = incremental_ly,
    #######################################################
    #### ECONOMIC OUTCOMES ################################
    #######################################################
    icer = icer,
    #######################################################
    #### ADDITIONAL HEALTH OUTCOMES ########################
    #######################################################
    evly_intervention = evly_intervention,
    evly_comparator   = evly_comparator,
    incremental_evly  = incremental_evly,
    
    hyt_intervention  = hyt_intervention,
    hyt_comparator    = hyt_comparator,
    incremental_hyt   = incremental_hyt,
    #####################################################
    #### AE DISUTILITY (NEW)
    #####################################################
    ae_disutility_intervention = ae_disutility_intervention,
    ae_disutility_comparator   = ae_disutility_comparator,
    incremental_ae_disutility  = incremental_ae_disutility,
    #####################################################
    #### NMB
    #####################################################
    nmb_intervention = nmb_intervention,
    nmb_comparator   = nmb_comparator,
    incremental_nmb  = incremental_nmb,
    nmb              = nmb,
    stringsAsFactors = FALSE
  )
  #########################################################
  #### RETURN AS LIST #####################################
  #########################################################
  list(
    outcomes = outcomes,
    base_case_results = base_case_results,
    cost_breakdown = cost_breakdown
  )
}


