#########################################################
#### Discounting Cost & Utility Settings ################
#########################################################
apply_discount_vector <- function(
    values,
    rate,
    cycle_length_months = 1
){
  n <- length(values)
  t <- 0:(n - 1)
  t_years <- t * (cycle_length_months / 12)
  discount_factor <- 1 / ((1 + rate)^t_years)
  values * discount_factor
}

#########################################################
#### Health State Cost & Utility Settings ###############
#########################################################
build_arm_health_state_object <- function(
    occupancy_df,
    health_state_settings,
    ae_monthly_disutility = NULL,
    discount_rate_cost,
    discount_rate_qaly,
    cycle_length_months = 1
){
  df <- occupancy_df
  #######################################################
  #### UTILITIES ########################################
  #######################################################
  utility_pf <- health_state_settings$utilities$PF$mean
  utility_pd <- health_state_settings$utilities$PD$mean
  #######################################################
  #### COSTS ############################################
  #######################################################
  cost_pf_first <- health_state_settings$costs$PF_First$mean
  cost_pf <- health_state_settings$costs$PF$mean
  cost_pd <- health_state_settings$costs$PD$mean
  #######################################################
  #### Debug ############################################
  #######################################################
  cat("\n=====================================\n")
  cat("HEALTH STATE SETTINGS USED\n")
  cat("=====================================\n")
  cat("PF Utility      :", utility_pf, "\n")
  cat("PD Utility      :", utility_pd, "\n")
  cat("PF First Cost   :", cost_pf_first, "\n")
  cat("PF Cost         :", cost_pf, "\n")
  cat("PD Cost         :", cost_pd, "\n")
  #######################################################
  #### HEALTH STATE COSTS ###############################
  #######################################################
  df$PF_First_Cost <- rep(0, nrow(df))
  df$PF_Cost       <- rep(0, nrow(df))
  df$PD_Cost       <- rep(0, nrow(df))
  #######################################################
  #### First Cycle PF Cost ##############################
  #######################################################
  if(
    nrow(df) > 0 &&
    !is.null(cost_pf_first)
  ){
    df$PF_First_Cost[1] <-
      df$PF[1] * cost_pf_first
  }
  #######################################################
  #### Ongoing PF Cost ##################################
  #######################################################
  if(
    nrow(df) > 1 &&
    !is.null(cost_pf)
  ){
    df$PF_Cost[2:nrow(df)] <-
      df$PF[2:nrow(df)] *
      cost_pf
  }
  #######################################################
  #### PD Cost ##########################################
  #######################################################
  if(!is.null(cost_pd)){
    df$PD_Cost <-
      df$PD *
      cost_pd
  }
  #######################################################
  #### Total Health State Cost ##########################
  #######################################################
  df$Total_State_Cost <- df$PF_First_Cost + df$PF_Cost + df$PD_Cost
  #######################################################
  #### LIFE YEARS #######################################
  #######################################################
  df$Alive <- df$PF + df$PD
  df$LYs <- df$Alive *(cycle_length_months / 12)
  #######################################################
  #### HEALTH STATE QALYS ###############################
  #######################################################
  df$PF_QALY <-
    df$PF *
    utility_pf *
    (cycle_length_months / 12)
  df$PD_QALY <-
    df$PD *
    utility_pd *
    (cycle_length_months / 12)
  #######################################################
  #### AE DISUTILITY ####################################
  #######################################################
  if(is.null(ae_monthly_disutility)){
    ae_monthly_disutility <-
      rep(
        0,
        nrow(df)
      )
  }
  if(length(ae_monthly_disutility) < nrow(df)){
    ae_monthly_disutility <-
      c(
        ae_monthly_disutility,
        rep(
          0,
          nrow(df) -
            length(ae_monthly_disutility)
        )
      )
  }
  ae_monthly_disutility <-
    ae_monthly_disutility[
      seq_len(nrow(df))
    ]
  df$AE_Disutility <-
    ae_monthly_disutility
  #######################################################
  #### TOTAL QALY #######################################
  #######################################################
  df$Total_QALY <-
    df$PF_QALY +
    df$PD_QALY +
    df$AE_Disutility
  #######################################################
  #### DISCOUNTING ######################################
  #######################################################
  cost_disc <-
    apply_discount_vector(
      values = df$Total_State_Cost,
      rate = discount_rate_cost,
      cycle_length_months = cycle_length_months
    )
  qaly_disc <-
    apply_discount_vector(
      values = df$Total_QALY,
      rate = discount_rate_qaly,
      cycle_length_months = cycle_length_months
    )
  lys_disc <-
    apply_discount_vector(
      values = df$LYs,
      rate = discount_rate_qaly,
      cycle_length_months = cycle_length_months
    )
  ae_disc <-
    apply_discount_vector(
      values = df$AE_Disutility,
      rate = discount_rate_qaly,
      cycle_length_months = cycle_length_months
    )
  #######################################################
  #### STORE DISCOUNTED VALUES ##########################
  #######################################################
  df$Cost_Discounted <- cost_disc
  df$QALY_Discounted <- qaly_disc
  df$LY_Discounted <- lys_disc
  df$AE_Disutility_Discounted <- ae_disc
  #######################################################
  #### Debug ############################################
  #######################################################
  cat("-------------------------------------\n")
  cat("Health State Summary\n")
  cat("-------------------------------------\n")
  cat("Undiscounted Cost :", round(sum(df$Total_State_Cost),2), "\n")
  cat("Discounted Cost   :", round(sum(cost_disc),2), "\n")
  cat("Undiscounted LY   :", round(sum(df$LYs),4), "\n")
  cat("Discounted LY     :", round(sum(lys_disc),4), "\n")
  cat("Undiscounted QALY :", round(sum(df$Total_QALY),4), "\n")
  cat("Discounted QALY   :", round(sum(qaly_disc),4), "\n")
  #######################################################
  #### RETURN ###########################################
  #######################################################
  # return(
  #   list(
  #     cycle_results = df,
  #     ###################################################
  #     #### COSTS ########################################
  #     ###################################################
  #     cost = list(
  #       undiscounted =
  #         sum(df$Total_State_Cost, na.rm = TRUE),
  #       discounted =
  #         sum(cost_disc, na.rm = TRUE)
  #     ),
  #     ###################################################
  #     #### QALYS ########################################
  #     ###################################################
  #     qaly = list(
  #       undiscounted =
  #         sum(df$Total_QALY, na.rm = TRUE),
  #       discounted =
  #         sum(qaly_disc, na.rm = TRUE),
  #       ae_undiscounted =
  #         sum(df$AE_Disutility, na.rm = TRUE),
  #       ae_discounted =
  #         sum(df$AE_Disutility_Discounted, na.rm = TRUE)
  #     ),
  #     ###################################################
  #     #### LIFE YEARS ###################################
  #     ###################################################
  #     lys = list(
  #       undiscounted =
  #         sum(df$LYs, na.rm = TRUE),
  #       discounted =
  #         sum(lys_disc, na.rm = TRUE)
  #     )
  #   )
  # )
  #######################################################
  #### RETURN ###########################################
  #######################################################
  return(
    list(
      
      ###################################################
      #### Cycle Results #################################
      ###################################################
      cycle_results = df,
      
      ###################################################
      #### Costs #########################################
      ###################################################
      cost = list(
        
        undiscounted =
          sum(df$Total_State_Cost, na.rm = TRUE),
        
        discounted =
          sum(cost_disc, na.rm = TRUE)
      ),
      
      ###################################################
      #### QALYs #########################################
      ###################################################
      qaly = list(
        
        #################################################
        #### Health-State Only ###########################
        #################################################
        
        health_state_undiscounted =
          sum(df$PF_QALY + df$PD_QALY, na.rm = TRUE),
        
        health_state_discounted =
          sum(
            apply_discount_vector(
              values =
                df$PF_QALY +
                df$PD_QALY,
              rate = discount_rate_qaly,
              cycle_length_months = cycle_length_months
            ),
            na.rm = TRUE
          ),
        
        #################################################
        #### AE Disutility ###############################
        #################################################
        
        ae_undiscounted =
          sum(df$AE_Disutility, na.rm = TRUE),
        
        ae_discounted =
          sum(df$AE_Disutility_Discounted, na.rm = TRUE),
        
        #################################################
        #### Total QALYs #################################
        #################################################
        
        undiscounted =
          sum(df$Total_QALY, na.rm = TRUE),
        
        discounted =
          sum(qaly_disc, na.rm = TRUE)
      ),
      
      ###################################################
      #### Life Years ####################################
      ###################################################
      lys = list(
        
        undiscounted =
          sum(df$LYs, na.rm = TRUE),
        
        discounted =
          sum(lys_disc, na.rm = TRUE)
      ),
      
      ###################################################
      #### Convenience Outputs ###########################
      ###################################################
      
      total_health_state_qaly =
        sum(
          apply_discount_vector(
            values =
              df$PF_QALY +
              df$PD_QALY,
            rate = discount_rate_qaly,
            cycle_length_months = cycle_length_months
          ),
          na.rm = TRUE
        ),
      
      total_ae_disutility =
        sum(df$AE_Disutility_Discounted, na.rm = TRUE),
      
      total_qaly =
        sum(qaly_disc, na.rm = TRUE)
    )
  )
}



