#########################################################
#### Generate Administration Schedule ###################
#########################################################
generate_administration_schedule <- function(
    therapy_settings,
    horizon_months
){
  avg_days_per_month <- 365.25 / 12
  if(therapy_settings$max_cycles_flag == "Yes"){
    n_admins <- therapy_settings$max_cycles
  } else {
    horizon_days  <- horizon_months * avg_days_per_month
    horizon_weeks <- horizon_days / 7
    n_admins <-
      floor(
        horizon_weeks /
          therapy_settings$cycle_length_weeks
      ) + 1
  }
  administration_weeks <-
    seq(
      from = 0,
      by = therapy_settings$cycle_length_weeks,
      length.out = n_admins
    )
  administration_days <- administration_weeks * 7
  model_month <- floor(administration_days / avg_days_per_month)
  schedule <- data.frame(
    administration_number = seq_along(administration_weeks),
    administration_week = administration_weeks,
    administration_day = administration_days,
    model_month = model_month
  )
  schedule <-
    schedule[
      schedule$model_month < horizon_months,
    ]
  rownames(schedule) <- NULL
  schedule
}

#########################################################
#### Assign Administration Costs ########################
#########################################################
assign_administration_costs <- function(
    administration_schedule,
    therapy_settings
){
  #######################################################
  #### SAMPLE COST IF PSA ###############################
  #######################################################
  if(
    !is.null(therapy_settings$cost$sample)
  ){
    base_cost <- therapy_settings$cost$sample
  } else {
    base_cost <- therapy_settings$cost_per_cycle
  }
  #######################################################
  #### START WITH BASE COST #############################
  #######################################################
  administration_schedule$cost <- base_cost
  #######################################################
  #### COST CHANGE ######################################
  #######################################################
  if(therapy_settings$cost_change_flag == "Yes"){
    if(
      !is.null(
        therapy_settings$cost_after_change_psa
      )
    ){
      changed_cost <-
        therapy_settings$cost_after_change_psa
    } else {
      changed_cost <-
        therapy_settings$cost_after_change
    }
    administration_schedule$cost[
      administration_schedule$administration_number >=
        therapy_settings$cost_change_cycle
    ] <- changed_cost
  }
  #######################################################
  #### COMMERCIAL DISCOUNT ##############################
  #######################################################
  administration_schedule$cost <- administration_schedule$cost * (1 - therapy_settings$discount/100)
  administration_schedule
}