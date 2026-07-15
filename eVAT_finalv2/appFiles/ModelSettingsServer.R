#########################################################
#### Guard: auto-correct blank/invalid manual entries ###
#########################################################
guardNumericInput(input, session, "discount_rate_cost", default = 3.5, min = 0, label = "Discount Rate - Costs (%)")
guardNumericInput(input, session, "discount_rate_QALY", default = 3.5, min = 0, label = "Discount Rate - Health Outcomes (%)")
guardNumericInput(input, session, "WTP", default = 35000, min = 0, label = "Willingness-to-Pay Threshold")
guardNumericInput(input, session, "PSA_Iterations", default = 1000, min = 1, label = "PSA Iterations")

#########################################################
#### Analysis Status ####################################
#########################################################
analysis_status <- reactiveVal("ready")

#########################################################
#### Run Button #########################################
#########################################################
observeEvent(input$run_model, {
  analysis_status("running")
  shinyjs::disable("run_model")
})

#########################################################
#### Master Model Object ################################
#########################################################
model_object <- eventReactive(input$run_model, {
 
   on.exit({
     shinyjs::enable("run_model")
  }, add = TRUE)

  cat("\n=====================================\n")
  cat("MODEL RUN STARTED\n")
  cat("=====================================\n")
  #####################################################
  #### VALIDATION #####################################
  #####################################################
  req(int_occupancy_object())
  req(comp_occupancy_object())
  req(int_therapy_settings_list())
  req(comp_therapy_settings_list())
  #####################################################
  #### OCCUPANCY ######################################
  #####################################################
  int_occ <- int_occupancy_object()$states
  comp_occ <- comp_occupancy_object()$states
  cat("\nIntervention Occupancy Rows:", nrow(int_occ))
  cat("\nComparator Occupancy Rows:", nrow(comp_occ))
  #########################################################
  #### Background Mortality Validation ####################
  #########################################################
  int_bg_diag <- tryCatch(
    get_background_mortality(int_occupancy_object()),
    error = function(e) NULL
  )
  if (!is.null(int_bg_diag)) {
    occ <- get_states(int_occupancy_object())
    validation_df <-
      cbind(
        int_bg_diag,
        occ[, setdiff(names(occ), "time"), drop = FALSE]
      )
    write.csv(validation_df,"Intervention_Background_Mortality_Validation.csv",row.names = FALSE)
    cat("\n========================================\n")
    cat("Intervention validation CSV exported\n")
    cat("========================================\n")
  }
  #########################################################
  #### Comparator Background Mortality Validation #########
  #########################################################
  comp_bg_diag <- tryCatch(
    get_background_mortality(comp_occupancy_object()),
    error = function(e) NULL
  )
  if (!is.null(comp_bg_diag)) {
    occ <- get_states(comp_occupancy_object())
    validation_df <-
      cbind(
        comp_bg_diag,
        occ[, setdiff(names(occ), "time"), drop = FALSE]
      )
    write.csv(validation_df,"Comparator_Background_Mortality_Validation.csv",row.names = FALSE)
    cat("\n========================================\n")
    cat("Comparator validation CSV exported\n")
    cat("========================================\n")
  }
  #####################################################
  #### PSA ############################################
  #####################################################
  psa_results <- NULL
  psa_summary <- NULL
  psa_cost_breakdown <- NULL
  psa_base_case <- NULL
  psa_health_outcomes <- NULL
  psa_uncertainty <- NULL
  psa_convergence <- NULL
  psa_evpi <- NULL
  psa_meta <- NULL
  if(input$run_psa){
    cat("\n=====================================\n")
    cat("Running Probabilistic Sensitivity Analysis...\n")
    cat("=====================================\n")
    psa_output <-
      run_psa(
        n_iterations = input$PSA_Iterations,
        seed = input$PSA_Seed,
        int_occ = int_occ,
        comp_occ = comp_occ,
        int_therapy_settings_list = int_therapy_settings_list(),
        comp_therapy_settings_list = comp_therapy_settings_list(),
        ae_object = ae_object(),
        health_state_settings = health_state_settings(),
        discount_rate_cost = input$discount_rate_cost/100,
        discount_rate_qaly = input$discount_rate_QALY/100,
        wtp = input$WTP
      )
    ###################################################
    #### Extract ######################################
    ###################################################
    psa_results          <- psa_output$iterations
    psa_summary          <- psa_output$summary
    psa_cost_breakdown   <- psa_output$cost_breakdown
    psa_base_case        <- psa_output$base_case
    psa_health_outcomes  <- psa_output$health_outcomes
    psa_uncertainty      <- psa_output$uncertainty
    psa_convergence      <- psa_output$convergence
    psa_evpi             <- psa_output$evpi
    psa_meta             <- psa_output$meta
  }
  #####################################################
  #### TREATMENT OBJECTS ##############################
  #####################################################
  int_treatment <- build_arm_treatment_object(
    occupancy_df = int_occ,
    therapy_settings_list = int_therapy_settings_list(),
    horizon_months = max(int_occ$time),
    discount_rate_cost = input$discount_rate_cost / 100
  )
  comp_treatment <- build_arm_treatment_object(
    occupancy_df = comp_occ,
    therapy_settings_list = comp_therapy_settings_list(),
    horizon_months = max(comp_occ$time),
    discount_rate_cost = input$discount_rate_cost / 100
  )
  #####################################################
  #### AE OBJECTS #####################################
  #####################################################
  int_ae <- build_arm_ae_object(
    arm_treatment_object = int_treatment,
    ae_object = ae_object(),
    occupancy_df = int_occ,
    horizon_months = max(int_occ$time),
    discount_rate_cost = input$discount_rate_cost / 100,
    arm = "intervention"
  )
  comp_ae <- build_arm_ae_object(
    arm_treatment_object = comp_treatment,
    ae_object = ae_object(),
    occupancy_df = comp_occ,
    horizon_months = max(comp_occ$time),
    discount_rate_cost = input$discount_rate_cost / 100,
    arm = "comparator"
  )
  #####################################################
  #### HEALTH STATE OBJECTS ###########################
  #####################################################
  int_health_state <- build_arm_health_state_object(
    occupancy_df = int_occ,
    health_state_settings = health_state_settings(),
    ae_monthly_disutility = int_ae$monthly_disutility,
    discount_rate_cost = input$discount_rate_cost / 100,
    discount_rate_qaly = input$discount_rate_QALY / 100
  )
  comp_health_state <- build_arm_health_state_object(
    occupancy_df = comp_occ,
    health_state_settings = health_state_settings(),
    ae_monthly_disutility = comp_ae$monthly_disutility,
    discount_rate_cost = input$discount_rate_cost / 100,
    discount_rate_qaly = input$discount_rate_QALY / 100
  )
  #####################################################
  #### VALIDATION TABLES ##############################
  #####################################################
  int_validation <- build_arm_validation_table(
    arm_treatment_object = int_treatment,
    occupancy_df = int_occ,
    ae_object = int_ae,
    health_state_object = int_health_state
  )
  comp_validation <- build_arm_validation_table(
    arm_treatment_object = comp_treatment,
    occupancy_df = comp_occ,
    ae_object = comp_ae,
    health_state_object = comp_health_state
  )
  #####################################################
  #### TREATMENT COST BREAKDOWN ########################
  #####################################################
  int_breakdown <- if (length(int_treatment$therapies) > 0) {
    data.frame(
      Cost_Category = vapply(
        int_treatment$therapies,
        function(x) x$settings$therapy_name,
        character(1)
      ),
      Intervention = vapply(
        int_treatment$therapies,
        function(x) x$total_cost_discounted,
        numeric(1)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Cost_Category = character(0),
      Intervention = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  comp_breakdown <- if (length(comp_treatment$therapies) > 0) {
    data.frame(
      Cost_Category = vapply(
        comp_treatment$therapies,
        function(x) x$settings$therapy_name,
        character(1)
      ),
      Comparator = vapply(
        comp_treatment$therapies,
        function(x) x$total_cost_discounted,
        numeric(1)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Cost_Category = character(0),
      Comparator = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  #####################################################
  #### TREATMENT COST SUMMARY #########################
  #####################################################
  treatment_cost_summary <- data.frame(
    Metric = c(
      "Treatment Cost (Discounted)",
      "Treatment Cost (Undiscounted)"
    ),
    Intervention = c(
      int_treatment$total_cost_discounted,
      int_treatment$total_cost_undiscounted
    ),
    Comparator = c(
      comp_treatment$total_cost_discounted,
      comp_treatment$total_cost_undiscounted
    ),
    stringsAsFactors = FALSE
  )
  #####################################################
  #### AE SUMMARY #####################################
  #####################################################
  ae_summary <- data.frame(
    Metric = c(
      "AE Cost (Undiscounted)",
      "AE Cost (Discounted)"
    ),
    Intervention = c(
      int_ae$total_cost,
      int_ae$total_cost_discounted
    ),
    Comparator = c(
      comp_ae$total_cost,
      comp_ae$total_cost_discounted
    ),
    stringsAsFactors = FALSE
  )
  #####################################################
  #### HEALTH STATE SUMMARY ###########################
  #####################################################
  health_state_summary <- data.frame(
    Metric = c(
      "Healthstatecost (Undiscounted)",
      "Healthstatecost (Discounted)",
      "LYs (Undiscounted)",
      "LYs (Discounted)",
      "QALYs (Undiscounted)",
      "QALYs (Discounted)"
    ),
    Intervention = c(
      int_health_state$cost$undiscounted,
      int_health_state$cost$discounted,
      int_health_state$lys$undiscounted,
      int_health_state$lys$discounted,
      int_health_state$qaly$undiscounted,
      int_health_state$qaly$discounted
    ),
    Comparator = c(
      comp_health_state$cost$undiscounted,
      comp_health_state$cost$discounted,
      comp_health_state$lys$undiscounted,
      comp_health_state$lys$discounted,
      comp_health_state$qaly$undiscounted,
      comp_health_state$qaly$discounted
    )
  )
  #####################################################
  #### TOTAL COST SUMMARY #############################
  #####################################################
  int_total_cost_undisc <- int_treatment$total_cost_undiscounted +int_ae$total_cost + int_health_state$cost$undiscounted
  int_total_cost_disc <- int_treatment$total_cost_discounted + int_ae$total_cost_discounted + int_health_state$cost$discounted
  comp_total_cost_undisc <- comp_treatment$total_cost_undiscounted + comp_ae$total_cost + comp_health_state$cost$undiscounted
  comp_total_cost_disc <- comp_treatment$total_cost_discounted + comp_ae$total_cost_discounted + comp_health_state$cost$discounted
  
  total_cost_summary <- data.frame(
    Metric = c("Total Cost (Undiscounted)", "Total Cost (Discounted)"),
    Intervention = c(int_total_cost_undisc, int_total_cost_disc),
    Comparator = c(comp_total_cost_undisc, comp_total_cost_disc),
    stringsAsFactors = FALSE
  )
  
  #####################################################
  #### ECONOMIC OUTCOMES ##############################
  #####################################################
  incremental_cost <- int_total_cost_disc - comp_total_cost_disc
  incremental_ly <- int_health_state$lys$discounted - comp_health_state$lys$discounted
  incremental_qaly <- int_health_state$qaly$discounted - comp_health_state$qaly$discounted
  cost_pct_change <- ifelse(
    abs(comp_total_cost_disc) < 1e-10, 
    NA, 100 * incremental_cost / comp_total_cost_disc)
  
  ly_pct_change <- ifelse(
    abs(comp_health_state$lys$discounted) < 1e-10,
    NA, 100 * incremental_ly / comp_health_state$lys$discounted)
  
  qaly_pct_change <- ifelse(
    abs(comp_health_state$qaly$discounted) < 1e-10,
    NA, 100 * incremental_qaly / comp_health_state$qaly$discounted)
  
  dominance <- if (
    incremental_cost < 0 & incremental_qaly > 0
  ) {
    "Dominant"
  } else if (
    incremental_cost > 0 & incremental_qaly < 0
  ) {
    "Dominated"
  } else {
    "Non-dominant"
  }
  #####################################################
  #### ICER ###########################################
  #####################################################
  icer <- if (
    abs(incremental_qaly) < 1e-10
  ) {
    NA_real_
  } else {
    incremental_cost / incremental_qaly
  }
  #####################################################
  #### NMB ############################################
  #####################################################
  wtp <- input$WTP
  int_nmb <- (int_health_state$qaly$discounted * wtp) - int_total_cost_disc
  comp_nmb <- (comp_health_state$qaly$discounted * wtp) - comp_total_cost_disc
  incremental_nmb <- int_nmb - comp_nmb
  #####################################################
  #### ECONOMICALLY JUSTIFIABLE PRICE #################
  #####################################################
  economically_justifiable_price <-
    calculate_ejp_table(
      int_treatment = int_treatment,
      comp_treatment = comp_treatment,
      incremental_cost = incremental_cost,
      incremental_qaly = incremental_qaly,
      wtp = wtp
    )
  #########################################################
  #### Additional Economic Outcomes #######################
  #########################################################
  additional_outcomes <-
    calculate_additional_outcomes(
      int_occ           = int_occ,
      comp_occ          = comp_occ,
      int_health_state  = int_health_state,
      comp_health_state = comp_health_state,
      int_total_cost    = int_total_cost_disc,
      comp_total_cost   = comp_total_cost_disc
    )
  #####################################################
  #### BASE CASE RESULTS ##############################
  #####################################################
  base_case_results <- data.frame(
    Treatment = c("Comparator", "Intervention"),
    Total_Cost = c(comp_total_cost_disc, int_total_cost_disc),
    Total_LY = c(comp_health_state$lys$discounted, int_health_state$lys$discounted),
    Total_QALY = c(comp_health_state$qaly$discounted, int_health_state$qaly$discounted),
    Incremental_Cost = c(NA, incremental_cost),
    Incremental_QALY = c(NA, incremental_qaly),
    Incremental_LY = c(NA, incremental_ly),
    ICER = c(NA, icer),
    NMB = c(comp_nmb, int_nmb),
  #  Cost_Percent_Change = c(NA, cost_pct_change),
  #  QALY_Percent_Change = c(NA, qaly_pct_change),
  #  LY_Percent_Change = c(NA, ly_pct_change),
    stringsAsFactors = FALSE
  )
  
  #####################################################
  #### ECONOMIC OUTCOME OBJECT ########################
  #####################################################
  economic_outcomes <- list(
    incremental_cost = incremental_cost,
    incremental_qaly = incremental_qaly,
    incremental_ly = incremental_ly,
    cost_pct_change = cost_pct_change,
    ly_pct_change = ly_pct_change,
    qaly_pct_change = qaly_pct_change,
    dominance = dominance,
    icer = icer,
    intervention_nmb = int_nmb,
    comparator_nmb = comp_nmb,
    incremental_nmb = incremental_nmb,
    wtp = wtp
  )
  
  #####################################################
  #### MASTER MODEL SUMMARY ###########################
  #####################################################
  model_summary <- rbind(treatment_cost_summary, ae_summary, health_state_summary, total_cost_summary)
  
  #####################################################
  #### MODEL COMPLETE #################################
  #####################################################
  cat("\n=====================================\n")
  cat("MODEL RUN COMPLETE\n")
  cat("=====================================\n")
  #####################################################
  #### RETURN #########################################
  #####################################################
  list(
    run_timestamp = Sys.time(),
    settings = list(
      discount_rate_cost = input$discount_rate_cost,
      discount_rate_QALY = input$discount_rate_QALY,
      PSA_iterations = input$PSA_Iterations,
      WTP = input$WTP,
      Apply_HCC = input$Apply_HCC
    ),
    intervention = list(
      occupancy = int_occ,
      treatment = int_treatment,
      ae = int_ae,
      health_state = int_health_state,
      validation = int_validation
    ),
    comparator = list(
      occupancy = comp_occ,
      treatment = comp_treatment,
      ae = comp_ae,
      health_state = comp_health_state,
      validation = comp_validation
    ),
    # results list
    results = list(
      base_case_results = base_case_results,
      economically_justifiable_prices = economically_justifiable_price,
      additional_outcomes = additional_outcomes,
      ###################################################
      #### PSA #########################################
      ###################################################
      psa_results = psa_results,
      psa_summary = psa_summary,
      psa_cost_breakdown = psa_cost_breakdown,
      psa_base_case = psa_base_case,
      psa_health_outcomes = psa_health_outcomes,
      psa_uncertainty = psa_uncertainty,
      psa_convergence = psa_convergence,
      psa_evpi = psa_evpi,
      psa_meta = psa_meta,
      economic_outcomes = economic_outcomes,
      treatment_costs = list(
        summary_table = treatment_cost_summary,
        intervention_breakdown = int_breakdown,
        comparator_breakdown = comp_breakdown
      ),
      ae_results = list(summary_table = ae_summary),
      health_state_results = list(summary_table = health_state_summary),
      total_cost_results = list(summary_table = total_cost_summary),
      model_summary = model_summary
    )
  )
})

#########################################################
#### Run Watcher: surfaces success/failure clearly ######
#########################################################
observeEvent(input$run_model, {
  tryCatch({
    model_object()
    analysis_status("completed")
  }, error = function(e) {
    analysis_status("error")
    shinyjs::enable("run_model")
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Model Run Failed",
      text = conditionMessage(e),
      type = "error"
    )
  })
}, priority = -1)

# write.csv(model_object()$results$base_case_results,
#           "Base_Case_Results.csv",
#           row.names = FALSE)

output$model_run_status <- renderUI({
  switch(
    analysis_status(),
    ready = div(
      icon("circle"),
      span(" Ready to run"),
      style = "color:#1976D2;font-weight:600;"
    ),
    running = div(
      icon("spinner", class = "fa-spin"),
      span(" Running analysis..."),
      style = "color:#F57C00;font-weight:600;"
    ),
    completed = div(
      icon("check-circle"),
      span(" Analysis completed"),
      style = "color:#2E7D32;font-weight:600;"
    ),
    error = div(
      icon("circle-exclamation"),
      span(" Analysis failed - see message"),
      style = "color:#C62828;font-weight:600;"
    )
  )
})

#########################################################
#### Export PSA Summary #################################
#########################################################
observeEvent(model_object(),{
  req(model_object()$results$psa_summary)
  write.csv(
    model_object()$results$psa_summary,
    "PSA_Summary.csv",
    row.names = FALSE
  )
})

#########################################################
#### Export Files #######################################
#########################################################
observeEvent(model_object(), {
  write.csv(model_object()$intervention$validation,"intervention_validation.csv", row.names = FALSE)
  write.csv(model_object()$comparator$validation, "comparator_validation.csv", row.names = FALSE)
  write.csv(model_object()$results$treatment_costs$summary_table, "Treatment_Cost_Summary.csv", row.names = FALSE)
  write.csv(model_object()$intervention$health_state$cycle_results,"intervention_health_state_cycles.csv", row.names = FALSE)
  write.csv(model_object()$comparator$health_state$cycle_results, "comparator_health_state_cycles.csv", row.names = FALSE)
  cat("\nAll exports completed successfully\n")
})

observeEvent(model_object(), {
  econ <- model_object()$results$economic_outcomes
  cat("\n")
  cat("-------------------------------------\n")
  cat("Economic Outcomes\n")
  cat("-------------------------------------\n")
  cat("Incremental Cost :", round(econ$incremental_cost,2), "\n")
  cat("Incremental LY   :", round(econ$incremental_ly,4), "\n")
  cat("Incremental QALY :", round(econ$incremental_qaly,4), "\n")
  cat("ICER             :", round(econ$icer,2), "\n")
  cat("Intervention NMB :", round(econ$intervention_nmb,2), "\n")
  cat("Comparator NMB   :", round(econ$comparator_nmb,2), "\n")
  cat("Incremental NMB  :", round(econ$incremental_nmb,2), "\n")
  cat("Cost % Change    :", round(econ$cost_pct_change,2), "%\n")
  cat("LY % Change      :", round(econ$ly_pct_change,2), "%\n")
  cat("QALY % Change    :", round(econ$qaly_pct_change,2), "%\n")
  cat("Dominance        :", econ$dominance, "\n")
})

observeEvent(model_object(), {
  write.csv(model_object()$results$base_case_results, "Base_Case_Results.csv", row.names = FALSE)
})

#########################################################
#### Diagnostics ########################################
#########################################################
observeEvent(model_object(), {
  cat("\n=====================================\n")
  cat("MODEL OBJECT CHECK\n")
  cat("=====================================\n")
  print(model_object()$results$model_summary)
})

observeEvent(model_object(), {
  write.csv(model_object()$results$model_summary, "Model_Object_Check.csv", row.names = FALSE)
})


