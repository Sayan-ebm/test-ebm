run_psa <- function(
    n_iterations,
    seed,
    int_occ,
    comp_occ,
    int_therapy_settings_list,
    comp_therapy_settings_list,
    ae_object,
    health_state_settings,
    discount_rate_cost,
    discount_rate_qaly,
    wtp
){
  
  set.seed(seed)
  
  cat("\n=====================================\n")
  cat("STARTING PSA\n")
  cat("=====================================\n")
  
  psa_iterations <- vector("list", n_iterations)

    #########################################################
  #### RUN PSA ITERATIONS
  #########################################################
  
  for(i in seq_len(n_iterations)){
    
    cat("\nIteration", i, "of", n_iterations, "\n")
    
    sampled_parameters <- sample_psa_parameters(
      health_state_settings,
      ae_object,
      int_therapy_settings_list,
      comp_therapy_settings_list
    )
    
    out <- run_single_iteration(
      sampled_parameters,
      int_occ,
      comp_occ,
      int_therapy_settings_list,
      comp_therapy_settings_list,
      ae_object,
      health_state_settings,
      discount_rate_cost,
      discount_rate_qaly,
      wtp
    )
    
    out$outcomes$Iteration <- i
    
    psa_iterations[[i]] <- out
  }
  
  #########################################################
  #### REMOVE FAILED ITERATIONS
  #########################################################
  
  psa_iterations <- Filter(Negate(is.null), psa_iterations)
  
  if(length(psa_iterations)==0){
    stop("All PSA iterations failed.")
  }
  
  #########################################################
  #### EXTRACT RESULTS
  #########################################################
  
  psa_results <-
    dplyr::bind_rows(
      lapply(psa_iterations, function(x) x$outcomes)
    )
  
  #########################################################
  #### KEEP FIRST TABLES
  #########################################################
  
  psa_base_case <-
    psa_iterations[[1]]$base_case_results
  
  psa_cost_table <-
    psa_iterations[[1]]$cost_breakdown
  
  #########################################################
  #### SAFETY CHECK
  #########################################################
  
  required_cols <- c(
    
    "total_cost_intervention",
    "total_cost_comparator",
    
    "qaly_intervention",
    "qaly_comparator",
    
    "ly_intervention",
    "ly_comparator",
    
    "incremental_cost",
    "incremental_qaly",
    "incremental_ly",
    
    "icer",
    "evly_intervention",
    "evly_comparator",
    
    "hyt_intervention",
    "hyt_comparator",
    
    "incremental_evly",
    "incremental_hyt",
    
    "nmb_intervention",
    "nmb_comparator",
    "incremental_nmb",
    "nmb",
    # 🔥 ADD THIS BLOCK
    "ae_disutility_intervention",
    "ae_disutility_comparator",
    "incremental_ae_disutility"
  )
  
  optional_numeric <- c(
    
    "SE_cost",
    
    "SE_prob_intervention",
    "LCI_prob_intervention",
    "UCI_prob_intervention",
    
    "SE_prob_comparator",
    "LCI_prob_comparator",
    "UCI_prob_comparator",
    
    "SE_disutility",
    "LCI_disutility",
    "UCI_disutility"
    
  )
  
  missing_cols <- setdiff(required_cols, names(psa_results))
  
  if(length(missing_cols)>0){
    
    stop(
      "PSA missing required columns: ",
      paste(missing_cols, collapse=", ")
    )
    
  }
  
  #########################################################
  #### BUILD OUTPUTS
  #########################################################
  
  psa_summary <- summarise_psa_results(psa_results)
  
  psa_health_outcomes <- compute_health_outcomes(psa_results)
  
  psa_uncertainty <- compute_uncertainty(psa_results)
  
  psa_convergence <- compute_convergence(psa_results)
  
  psa_evpi <- compute_evpi(
    psa_results,
    wtp
  )
  
  #########################################################
  #### META
  #########################################################
  
  psa_meta <- list(
    
    n_iterations_requested = n_iterations,
    
    n_iterations_successful = nrow(psa_results),
    
    n_iterations_failed =
      n_iterations - nrow(psa_results)
    
  )
  
  cat("\nPSA COMPLETE\n")
  
  #########################################################
  #### RETURN
  #########################################################
  
  list(
    
    iterations = psa_results,
    
    summary = psa_summary,
    
    cost_breakdown = psa_cost_table,
    
    base_case = psa_base_case,
    
    health_outcomes = psa_health_outcomes,
    
    uncertainty = psa_uncertainty,
    
    convergence = psa_convergence,
    
    evpi = psa_evpi,
    
    meta = psa_meta
    
  )
  
}

#########################################################
# COST BREAKDOWN
#########################################################

compute_cost_breakdown <- function(psa_results){
  
  data.frame(
    
    Cost_Component = c(
      "Treatment",
      "Adverse Events",
      "Health States",
      "Total"
    ),
    
    Intervention = c(
      format_psa(psa_results$treatment_cost_intervention, prefix = "€ "),
      format_psa(psa_results$ae_cost_intervention, prefix = "€ "),
      format_psa(psa_results$health_state_cost_intervention, prefix = "€ "),
      format_psa(psa_results$total_cost_intervention, prefix = "€ ")
    ),
    
    Comparator = c(
      format_psa(psa_results$treatment_cost_comparator, prefix = "€ "),
      format_psa(psa_results$ae_cost_comparator, prefix = "€ "),
      format_psa(psa_results$health_state_cost_comparator, prefix = "€ "),
      format_psa(psa_results$total_cost_comparator, prefix = "€ ")
    ),
    
    Incremental = c(
      format_psa(
        psa_results$treatment_cost_intervention -
          psa_results$treatment_cost_comparator,
        prefix = "€ "
      ),
      
      format_psa(
        psa_results$ae_cost_intervention -
          psa_results$ae_cost_comparator,
        prefix = "€ "
      ),
      
      format_psa(
        psa_results$health_state_cost_intervention -
          psa_results$health_state_cost_comparator,
        prefix = "€ "
      ),
      
      format_psa(
        psa_results$incremental_cost,
        prefix = "€ "
      )
    ),
    
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
}

#########################################################
# HEALTH OUTCOMES
#########################################################
compute_health_outcomes <- function(psa_results){
  data.frame(
    intervention_qaly = mean(psa_results$qaly_intervention, na.rm = TRUE),
    comparator_qaly   = mean(psa_results$qaly_comparator, na.rm = TRUE),
    incremental_qaly  = mean(psa_results$incremental_qaly, na.rm = TRUE),
    intervention_ly = mean(psa_results$ly_intervention, na.rm = TRUE),
    comparator_ly   = mean(psa_results$ly_comparator, na.rm = TRUE),
    incremental_ly  = mean(psa_results$incremental_ly, na.rm = TRUE),
    intervention_evly = mean(psa_results$evly_intervention, na.rm = TRUE),
    comparator_evly   = mean(psa_results$evly_comparator, na.rm = TRUE),
    incremental_evly  = mean(psa_results$incremental_evly, na.rm = TRUE),
    intervention_hyt = mean(psa_results$hyt_intervention, na.rm = TRUE),
    comparator_hyt   = mean(psa_results$hyt_comparator, na.rm = TRUE),
    incremental_hyt  = mean(psa_results$incremental_hyt, na.rm = TRUE)
  )
}

#########################################################
# BASE CASE (FIXED + CONSISTENT)
#########################################################

#########################################################
# BASE CASE
#########################################################
extract_base_case <- function(psa_results){
  if(nrow(psa_results)==0){    return(NULL)
  }
  idx <- which.min(
    abs(
      psa_results$incremental_cost -
        median(
          psa_results$incremental_cost,
          na.rm = TRUE
        )
    )
  )
  psa_results$base_case_results[[idx]]
}

#########################################################
# UNCERTAINTY (FIXED)
#########################################################
compute_uncertainty <- function(psa_results) {
  numeric_cols <- names(psa_results)[sapply(psa_results, is.numeric)]
  dplyr::bind_rows(lapply(numeric_cols, function(col) {
    x <- psa_results[[col]]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0) {
      return(data.frame(
        Outcome = col,
        Mean = NA,
        SD = NA,
        P2.5 = NA,
        P97.5 = NA
      ))
    }
    data.frame(
      Outcome = col,
      Mean = mean(x),
      SD = sd(x),
      P2.5 = quantile(x, 0.025),
      P97.5 = quantile(x, 0.975)
    )
  }))
}

#########################################################
# CONVERGENCE
#########################################################
compute_convergence <- function(psa_results) {
  n <- nrow(psa_results)
  inc_cost <- psa_results$incremental_cost
  inc_qaly <- psa_results$incremental_qaly
  cum_cost <- cumsum(inc_cost) / seq_len(n)
  cum_qaly <- cumsum(inc_qaly) / seq_len(n)
  data.frame(
    iteration = seq_len(n),
    cum_mean_cost = cum_cost,
    cum_mean_qaly = cum_qaly,
    cum_icer = cum_cost / cum_qaly
  )
}

#########################################################
# EVPI (FIXED)
#########################################################
compute_evpi <- function(psa_results, wtp) {
  if (is.null(wtp)) {
    return(data.frame(error = "WTP not provided"))
  }
  nmb <- psa_results$incremental_qaly * wtp - psa_results$incremental_cost
  ev_perfect <- mean(pmax(nmb, 0), na.rm = TRUE)
  ev_current <- max(mean(nmb, na.rm = TRUE), 0)
  data.frame(
    evpi = ev_perfect - ev_current
  )
}
#########################################################
# PLACEHOLDER (ONLY IF YOU DON'T HAVE A FULL ONE YET)
#########################################################
summarise_psa_results <- function(psa_results){
  if (is.null(psa_results) || nrow(psa_results) == 0) {
    return(NULL)
  }
  exclude_cols <- c("Iteration", "cost_breakdown")
  numeric_columns <- setdiff(
    names(psa_results)[sapply(psa_results, is.numeric)],
    exclude_cols
  )
  summary_list <- lapply(numeric_columns, function(var){
    x <- psa_results[[var]]
    x <- x[!is.na(x)]
    data.frame(
      Outcome = var,
      Mean = mean(x),
      Median = median(x),
      SD = sd(x),
      Minimum = min(x),
      Maximum = max(x),
      Percentile2.5 = quantile(x, 0.025),
      Percentile97.5 = quantile(x, 0.975),
      Iterations = length(x)
    )
  })
  summary_table <- dplyr::bind_rows(summary_list)
  # classification
  summary_table$Type <- dplyr::case_when(
    grepl("icer", summary_table$Outcome, ignore.case = TRUE) ~ "Ratio",
    grepl("incremental", summary_table$Outcome) ~ "Incremental",
    grepl("cost", summary_table$Outcome, ignore.case = TRUE) ~ "Cost",
    grepl("qaly|ly", summary_table$Outcome, ignore.case = TRUE) ~ "Health outcome",
    grepl("nmb", summary_table$Outcome, ignore.case = TRUE) ~ "Economic outcome",
    TRUE ~ "Other"
  )
  summary_table$ZeroVariance <- summary_table$SD == 0
  summary_table
}








