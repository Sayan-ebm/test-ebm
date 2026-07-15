summarise_psa_results <- function(psa_results){
  if (is.null(psa_results) || nrow(psa_results) == 0) {
    return(NULL)
  }
  exclude_cols <- c("Iteration")
  numeric_columns <- setdiff(
    names(psa_results)[sapply(psa_results, is.numeric)],
    exclude_cols
  )
  summary_list <- lapply(numeric_columns, function(variable){
    x <- psa_results[[variable]]
    x <- x[!is.na(x)]
    n <- length(x)
    mean_x   <- mean(x)
    median_x <- median(x)
    sd_x     <- sd(x)
    min_x    <- min(x)
    max_x    <- max(x)
    q025 <- quantile(x, 0.025, na.rm = TRUE)
    q975 <- quantile(x, 0.975, na.rm = TRUE)
    se_x <- ifelse(n > 1, sd_x / sqrt(n), NA_real_)
    ci_lower <- ifelse(!is.na(se_x), mean_x - 1.96 * se_x, NA_real_)
    ci_upper <- ifelse(!is.na(se_x), mean_x + 1.96 * se_x, NA_real_)
    data.frame(
      Outcome = variable,
      Mean = mean_x,
      Median = median_x,
      SD = sd_x,
      Minimum = min_x,
      Maximum = max_x,
      Percentile2.5 = q025,
      Percentile97.5 = q975,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Iterations = n,
      stringsAsFactors = FALSE
    )
  })
  summary_table <- dplyr::bind_rows(summary_list)
  #########################################################
  #### ROUNDING ###########################################
  #########################################################
  num_cols <- names(summary_table)[sapply(summary_table, is.numeric)]
  summary_table[num_cols] <- lapply(summary_table[num_cols], round, 4)
  #########################################################
  #### CLASSIFY ###########################################
  #########################################################
  summary_table$Type <- dplyr::case_when(
    grepl("ICER", summary_table$Outcome) ~ "Ratio (DO NOT MEAN)",
    grepl("incremental", summary_table$Outcome) ~ "Incremental",
    grepl("cost", summary_table$Outcome) ~ "Cost",
    grepl("qaly|ly|evly|hyt", summary_table$Outcome, ignore.case = TRUE) ~ "Health outcome",
    grepl("nmb", summary_table$Outcome, ignore.case = TRUE) ~ "Economic outcome",
    TRUE ~ "Other"
  )
  summary_table$ZeroVariance <- summary_table$SD == 0
  #########################################################
  #### PRINT ##############################################
  #########################################################
  cat("\n=====================================\n")
  cat("PSA SUMMARY (MEAN + MEDIAN + CI)\n")
  cat("=====================================\n")
  print(summary_table)
  cat("\nNOTE:\n")
  cat("- ICER should be interpreted via distribution, not mean\n")
  cat("- NMB is preferred decision metric\n")
  cat("- Percentiles are more reliable than CI for PSA\n")
  return(summary_table)
}


