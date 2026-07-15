#########################################################
#### Generate Partitioned Survival States ###############
#########################################################
generate_partitioned_survival_states <- function(
    os_curve,
    pfs_curve = NULL,
    start_age = NULL,
    mortality_table = NULL,
    apply_bgm = FALSE,
    male_proportion = NULL
){
  #######################################################
  #### INPUT CHECKS #####################################
  #######################################################
  if(is.null(os_curve)){
    stop("OS curve is NULL")
  }
  cat("\n====================\n")
  cat("BACKGROUND MORTALITY CHECK\n")
  cat("====================\n")
  cat("Starting age:\n")
  print(start_age)
  #######################################################
  #### DEFAULT DIAGNOSTICS ##############################
  #######################################################
  bg_diagnostics <- NULL
  bg_life_table <- NULL
  if(!is.null(mortality_table)){
    cat("Mortality table rows:\n")
    print(nrow(mortality_table))
  } else {
    cat("Mortality table: NULL\n")
  }
  #######################################################
  #### APPLY BACKGROUND MORTALITY #######################
  #######################################################
  if(
    isTRUE(apply_bgm) &&
    !is.null(start_age) &&
    !is.null(mortality_table)
  ){
    bg_result <- apply_background_mortality(
      os_curve = os_curve,
      start_age = start_age,
      mortality_table = mortality_table,
      apply_bgm = apply_bgm,
      male_proportion = male_proportion
    )
    os_curve <- bg_result$os_curve
    bg_diagnostics <- bg_result$diagnostics
    bg_life_table <- bg_result$life_table
  }
  #######################################################
  #### OS ONLY MODEL ####################################
  #######################################################
  if(is.null(pfs_curve)){
    alive <- pmax(
      pmin(os_curve$surv, 1),
      0
    )
    dead <- 1 - alive
    return(
      list(states = data.frame(
          time = os_curve$time,
          Alive = alive,
          Dead = dead
        ),
        background_mortality = bg_diagnostics,
        background_life_table = bg_life_table)
    )
  }
  #######################################################
  #### VALIDATE PFS #####################################
  #######################################################
  if(nrow(pfs_curve) == 0){
    stop("PFS curve is empty")
  }
  #######################################################
  #### ALIGN CURVES #####################################
  #######################################################
  if(
    length(os_curve$surv) != length(pfs_curve$surv)
  ){
    stop("OS and PFS curves have different lengths")
  }
  #######################################################
  #### PARTITIONED SURVIVAL #############################
  #######################################################
  alive <- os_curve$surv
  pf <- pmax(pmin(pfs_curve$surv, alive), 0)
  pd <- alive - pf
  dead <- 1 - alive
  #######################################################
  #### FINAL SAFETY #####################################
  #######################################################
  pf <- pmax(pmin(pf,1),0)
  pd <- pmax(pmin(pd,1),0)
  dead <- pmax(pmin(dead,1),0)
  #######################################################
  #### RETURN ###########################################
  #######################################################
  list(
    states = data.frame(
      time = os_curve$time,
      PF = pf,
      PD = pd,
      Dead = dead),
    background_mortality = bg_diagnostics,
    background_life_table = bg_life_table
  )
}

#########################################################
#### Validate Occupancy #################################
#########################################################
validate_occupancy <- function(
    occupancy_df,
    tolerance = 1e-6
){
  states <- setdiff(names(occupancy_df), "time")
  totals <- rowSums(occupancy_df[, states, drop = FALSE])
  all(
    abs(totals - 1) < tolerance
  ) &&
    all(occupancy_df[, states] >= 0)
}
#########################################################
#### Get State Names ####################################
#########################################################
get_state_names <- function(
    occupancy_df
){
  setdiff(names(occupancy_df), "time")
}
#########################################################
#### Number Of States ###################################
#########################################################
get_number_of_states <- function(
    occupancy_df
){
  length(get_state_names(occupancy_df))
}
#########################################################
#### Convert To Long Format #############################
#########################################################
get_occupancy_long <- function(
    occupancy_df
){
  tidyr::pivot_longer(
    occupancy_df,
    cols = -time,
    names_to = "state",
    values_to = "occupancy"
  )
}
#########################################################
#### Occupancy Summary ##################################
#########################################################
summarise_occupancy <- function(
    occupancy_df
){
  list(rows = nrow(occupancy_df),
    states = get_state_names(occupancy_df),
    valid = validate_occupancy(occupancy_df))
}
#########################################################
#### Build Occupancy Object #############################
#########################################################
build_occupancy_object <- function(
    os_curve,
    pfs_curve = NULL,
    start_age = NULL,
    mortality_table = NULL,
    apply_bgm = FALSE,
    male_proportion = NULL
){
  validate(need(!is.null(os_curve), "OS curve is NULL"))
  if(
    !is.null(pfs_curve) && nrow(pfs_curve) == 0
  ){
    stop("PFS curve is empty")
  }
  occupancy_result <- generate_partitioned_survival_states(
    os_curve = os_curve,
    pfs_curve = pfs_curve,
    start_age = start_age,
    mortality_table = mortality_table,
    apply_bgm = apply_bgm,
    male_proportion = male_proportion
  )
  states <- occupancy_result$states
  list(
    states = states,
    summary = summarise_occupancy(states),
    valid = validate_occupancy(states),
    background_mortality = occupancy_result$background_mortality,
    background_life_table = occupancy_result$background_life_table
  )
}
#########################################################
#### Extract States #####################################
#########################################################
get_states <- function(
    occupancy_object
){
  occupancy_object$states
}

get_background_mortality <- function(
    occupancy_object
){
  occupancy_object$background_mortality
}
#########################################################
#### Extract Background Life Table ######################
#########################################################
get_background_life_table <- function(
    occupancy_object
){
  occupancy_object$background_life_table
}
#########################################################
#### Extract Summary ####################################
#########################################################
get_occupancy_summary <- function(
    occupancy_object
){
  occupancy_object$summary
}
#########################################################
#### Plot Data ##########################################
#########################################################
get_occupancy_plot_data <- function(
    occupancy_object
){
  get_occupancy_long(occupancy_object$states)
}
#########################################################
#### State Cost Hook ####################################
#########################################################
apply_state_costs <- function(
    occupancy_object,
    state_costs
){
  NULL
}
#########################################################
#### Utility Hook #######################################
#########################################################
apply_state_utilities <- function(
    occupancy_object,
    utilities
){
  NULL
}

#########################################################
#### Print Occupancy Summary ############################
#########################################################
print_occupancy_summary <- function(
    occupancy_object
){
  cat("\n====================\n")
  cat("OCCUPANCY SUMMARY\n")
  cat("====================\n")
  print(occupancy_object$summary)
  cat("\nVALID:", occupancy_object$valid, "\n")
  cat("\n====================\n")
}
print_occupancy_debug <- function(
    occupancy_object,
    n = 10
){
  cat("\n====================\n")
  cat("STATE OCCUPANCY\n")
  cat("====================\n")
  print(head(occupancy_object$states, n))
  cat("\nVALID:",occupancy_object$valid, "\n")
  cat("\nMIN VALUES\n")
  print(sapply(occupancy_object$states[, -1, drop = FALSE], min))
  cat("\nMAX VALUES\n")
  print(sapply(occupancy_object$states[, -1, drop = FALSE], max))
  
}

