#########################################################
#### Calculate Economically Justifiable Prices ##########
#########################################################
calculate_ejp_table <- function(
    int_treatment,
    comp_treatment,
    incremental_cost,
    incremental_qaly,
    wtp
){
  #######################################################
  #### Initialise #######################################
  #######################################################
  ejp_rows <- list()
  #######################################################
  #### Internal function ################################
  #######################################################
  calculate_arm <- function(
    therapy_list,
    arm = c("Intervention","Comparator")
  ){
    arm <- match.arg(arm)
    rows <- list()
    for(i in seq_along(therapy_list)){
      therapy <- therapy_list[[i]]
      ###################################################
      #### Skip empty therapies #########################
      ###################################################
      if(nrow(therapy$administration_schedule)==0){
        next
      }
      ###################################################
      #### Discount factor ##############################
      ###################################################
      discount_factor <-
        1 -
        therapy$settings$discount/100
      ###################################################
      #### Mean discounted administration cost ##########
      ###################################################
      mean_admin_cost <-
        mean(
          therapy$administration_schedule$cost,
          na.rm = TRUE
        )
      ###################################################
      #### Effective lifetime administrations ###########
      ###################################################
      effective_cycles <-
        therapy$total_cost_discounted /
        mean_admin_cost
      ###################################################
      #### Other incremental costs ######################
      ###################################################
      if(arm=="Intervention"){
        other_costs <-
          incremental_cost -
          therapy$total_cost_discounted
        discounted_price <-
          (
            (wtp*incremental_qaly) -
              other_costs
          )/
          effective_cycles
      }else{
        other_costs <-
          incremental_cost +
          therapy$total_cost_discounted
        discounted_price <-
          (
            other_costs -
              (wtp*incremental_qaly)
          )/
          effective_cycles
      }
      ###################################################
      #### Convert back to list price ###################
      ###################################################
      ejp_price <-
        discounted_price /
        discount_factor
      ###################################################
      #### Negative prices ##############################
      ###################################################
      if(
        is.na(ejp_price) ||
        is.infinite(ejp_price) ||
        ejp_price<0
      ){
        ejp_price <- NA_real_
      }
      ###################################################
      #### Difference ###################################
      ###################################################
      current_price <-
        therapy$settings$cost_per_cycle
      difference <-
        ifelse(
          is.na(ejp_price),
          NA_real_,
          ejp_price-current_price
        )
      ###################################################
      #### Store ########################################
      ###################################################
      rows[[length(rows)+1]] <-
        data.frame(
          Arm = arm,
          Therapy =
            therapy$settings$therapy_name,
          Current_Price_Per_Cycle =
            current_price,
          Economically_Justifiable_Price =
            round(ejp_price,0),
          Difference =
            round(difference,0),
          stringsAsFactors = FALSE
        )
    }
    do.call(rbind,rows)
  }
  #######################################################
  #### Intervention #####################################
  #######################################################
  int_table <-
    calculate_arm(
      int_treatment$therapies,
      "Intervention"
    )
  #######################################################
  #### Comparator #######################################
  #######################################################
  comp_table <-
    calculate_arm(
      comp_treatment$therapies,
      "Comparator"
    )
  #######################################################
  #### Return ###########################################
  #######################################################
  rbind(
    int_table,
    comp_table
  )
}

#########################################################
#### Additional Economic Outcomes #######################
#########################################################
calculate_additional_outcomes <- function(
    int_occ,
    comp_occ,
    int_health_state,
    comp_health_state,
    int_total_cost,
    comp_total_cost
){
  #######################################################
  #### Base Outcomes ####################################
  #######################################################
  cost_int  <- int_total_cost
  cost_comp <- comp_total_cost
  qaly_int  <- int_health_state$qaly$discounted
  qaly_comp <- comp_health_state$qaly$discounted
  ly_int    <- int_health_state$lys$discounted
  ly_comp   <- comp_health_state$lys$discounted
  #######################################################
  #### Incrementals #####################################
  #######################################################
  inc_cost <- cost_int - cost_comp
  inc_qaly <- qaly_int - qaly_comp
  inc_ly   <- ly_int - ly_comp
  #######################################################
  #### Equal Value Life Years (evLY) ####################
  #######################################################
  evly_comp <- qaly_comp
  evly_int  <- evly_comp + 0.851 * inc_ly
  inc_evly  <- evly_int - evly_comp
  #######################################################
  #### Healthy Years Total (HYT) ########################
  #######################################################
  hyt_comp <- qaly_comp
  hyt_int <- qaly_int + inc_evly
  inc_hyt <- hyt_int - hyt_comp
  #######################################################
  #### Cost per evLY ####################################
  #######################################################
  cost_per_evly <-
    ifelse(
      abs(inc_evly) < 1e-10,
      NA_real_,
      inc_cost / inc_evly
    )
  #######################################################
  #### Cost per HYT #####################################
  #######################################################
  cost_per_hyt <-
    ifelse(
      abs(inc_hyt) < 1e-10,
      NA_real_,
      inc_cost / inc_hyt
    )
  #######################################################
  #### Return ###########################################
  #######################################################
  data.frame(
    Component = c(
      "Equal Value Life Years (evLY)",
      "Healthy Years Total (HYT)",
      "Cost per evLY Gained",
      "Cost per HYT Gained"
    ),
    Intervention = c(
      evly_int,
      hyt_int,
      NA_real_,
      NA_real_
    ),
    Comparator = c(
      evly_comp,
      hyt_comp,
      NA_real_,
      NA_real_
    ),
    Incremental = c(
      inc_evly,
      inc_hyt,
      cost_per_evly,
      cost_per_hyt
    ),
    stringsAsFactors = FALSE
  )
}
















