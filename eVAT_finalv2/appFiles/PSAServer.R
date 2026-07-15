#########################################################
#### PSA SERVER MODULE ##################################
#########################################################

#########################################################
#### REQUIRED SAFE HELPERS ##############################
#########################################################

fmt_num <- function(x, digits = 2){
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

#########################################################
#### PROFESSIONAL PSA SUMMARY CARD ######################
#########################################################
create_psa_card <- function(
    title,
    icon_name,
    value,
    lower = NULL,
    upper = NULL,
    footer = NULL,
    colour = "#E67E22",
    background = "#FFFFFF"
){
  div(
    style=paste0(
      "background:",background,";
       border-radius:14px;
       border:1px solid #E5E7EB;
       box-shadow:0 2px 8px rgba(0,0,0,.06);
       padding:18px;
       min-height:205px;
       position:relative;"
    ),
    div(
      style=paste0(
        "position:absolute;
         left:0;
         top:0;
         bottom:0;
         width:6px;
         border-radius:14px 0 0 14px;
         background:",colour,";"
      )
    ),
    div(
      style="margin-left:8px;",
      div(
        style="display:flex;
               align-items:center;
               gap:10px;
               margin-bottom:12px;",
        icon(
          icon_name,
          style=paste0(
            "color:",colour,
            ";font-size:20px;"
          )
        ),
        tags$span(
          style="font-size:17px;
                 font-weight:600;
                 color:#2C3E50;",
          title
        )
      ),
      div(
        style="font-size:12px;
               color:#7F8C8D;
               font-weight:600;",
        "Mean"
      ),
      div(
        style=paste0(
          "font-size:28px;
           font-weight:700;
           color:",colour,
          ";
           margin-bottom:18px;"
        ),
        value
      ),
      if(!is.null(lower)){
        tagList(
          div(
            style="font-size:12px;
                   color:#7F8C8D;
                   font-weight:600;",
            "95% Confidence Interval"
          ),
                    div(
            style="font-size:15px;
                   font-weight:600;
                   color:#34495E;
                   margin-top:4px;",
            paste0(lower," to ",upper)
          )
        )
      },
      if(!is.null(footer))
        div(
          style="margin-top:16px;
                 color:#7F8C8D;
                 font-size:12px;",
          footer
        )
    )
  )
}

#########################################################
#### PSA DATA REACTIVES #################################
#########################################################
psa_summary <- reactive({
  req(model_object(), model_object()$results$psa_summary)
  model_object()$results$psa_summary
})

psa_iterations <- reactive({
  req(model_object(), model_object()$results$psa$iterations)
  model_object()$results$psa$iterations
})



#########################################################
#### SAFE ROW EXTRACTOR #################################
#########################################################
get_psa_row <- function(outcome){
  s <- psa_summary()
  row <- s[s$Outcome == outcome, ]
  validate(
    need(nrow(row) == 1, paste("Missing PSA outcome:", outcome))
  )
  row
}

#########################################################
#### SUMMARY CARD 1: INCREMENTAL COST ###################
#########################################################
output$cardIncCost <- renderUI({
  s <- get_psa_row("incremental_cost")
  create_psa_card(
    title = "Incremental Cost",
    icon_name = "sack-dollar",
    value = paste0(currency_symbol(), fmt_num(s$Mean)),
    lower = paste0(currency_symbol(), fmt_num(s$Percentile2.5)),
    upper = paste0(currency_symbol(), fmt_num(s$Percentile97.5)),
    colour = "#E67E22",
    background = "#FFF4E6"
  )
})

#########################################################
#### SUMMARY CARD 2: INCREMENTAL QALY ###################
#########################################################
output$cardIncQALY <- renderUI({
  s <- get_psa_row("incremental_qaly")
  create_psa_card(
    title = "Incremental QALY",
    icon_name = "heartbeat",
    value = fmt_num(s$Mean, 2),
    lower = fmt_num(s$Percentile2.5, 2),
    upper = fmt_num(s$Percentile97.5, 2),
    colour = "#27AE60",
    background = "#ECFFF4"
  )
})

#########################################################
#### SUMMARY CARD 3: ICER ###############################
#########################################################
output$cardICER <- renderUI({
  inc_cost <- get_psa_row("incremental_cost")$Mean
  inc_qaly <- get_psa_row("incremental_qaly")$Mean
  icer_mean <- if(abs(inc_qaly) < 1e-10) {
    NA_real_
  } else {
    inc_cost / inc_qaly
  }
  create_psa_card(
    title = "ICER",
    icon_name = "chart-line",
    value = if(is.na(icer_mean)) {
      "Not estimable"
    } else {
      paste0(currency_symbol(), fmt_num(icer_mean))
    },
    colour = "#2980B9",
    background = "#EDF6FD",
    footer = "Calculated as Mean Incremental Cost ÷ Mean Incremental QALY"
  )
})

#########################################################
#### SUMMARY CARD: PROBABILITY COST-EFFECTIVE ###########
#########################################################
output$cardProbCE <- renderUI({
  req(model_object())
  #######################################################
  #### SAME DATA USED BY CEAC ###########################
  #######################################################
  iter <- model_object()$results$psa_results
  validate(
    need(!is.null(iter), "No PSA iterations available")
  )
  iter$incremental_cost <- as.numeric(iter$incremental_cost)
  iter$incremental_qaly <- as.numeric(iter$incremental_qaly)
  iter <- iter[
    is.finite(iter$incremental_cost) &
      is.finite(iter$incremental_qaly),
  ]
  #######################################################
  #### WTP ##############################################
  #######################################################
  wtp <- input$WTP
  if(is.null(wtp) || is.na(wtp))
    wtp <- 30000
  #######################################################
  #### Probability ######################################
  #######################################################
  prob_ce <- mean(
    wtp * iter$incremental_qaly -
      iter$incremental_cost > 0,
    na.rm = TRUE
  )
  #######################################################
  #### Card #############################################
  #######################################################
  create_psa_card(
    title = "Probability Cost-Effective",
    icon_name = "percent",
    value = paste0(sprintf("%.2f", 100 * prob_ce), "%"),
    footer = paste0(
      "WTP = ",
      currency_symbol(),
      format(round(wtp), big.mark = ",")
    ),
    colour = "#8E44AD",
    background = "#FAF5FF"
  )
})

#########################################################
#### BASE CASE TABLE ####################################
#########################################################
summary_cell <- function(df, outcome){
  row <- df[df$Outcome == outcome, ]
  if(nrow(row) == 0)
    return("")
  paste0(
    fmt_num(row$Mean),
    " (",
    fmt_num(row$Percentile2.5),
    ", ",
    fmt_num(row$Percentile97.5),
    ")"
  )
}

#########################################################
#### PSA ICER FROM MEAN OUTCOMES ########################
#########################################################
# psa_mean_icer <- function(summary_df){
#   inc_cost <- get_summary_row(summary_df, "incremental_cost")$Mean
#   inc_qaly <- get_summary_row(summary_df, "incremental_qaly")$Mean
#   if(abs(inc_qaly) < 1e-10){
#     return("Not estimable")
#   }
#   paste0(currency_symbol(), fmt_num(inc_cost / inc_qaly))
# }

psa_mean_icer <- function(summary_df){
  inc_cost <- get_summary_row(summary_df, "incremental_cost")$Mean
  inc_qaly <- get_summary_row(summary_df, "incremental_qaly")$Mean
  if(abs(inc_qaly) < 1e-10){
    return("Not estimable")
  }
  fmt_num(inc_cost / inc_qaly)
}

# #########################################################
# #### BASE CASE DATA #####################################
# #########################################################
# base_case_table <- reactive({
#   s <- psa_summary()
#   data.frame(
#     Treatment = c(
#       "Comparator",
#       "Intervention"
#     ),
#     Total_Cost = c(
#       summary_cell(s, "total_cost_comparator"),
#       summary_cell(s, "total_cost_intervention")
#     ),
#     Total_LY = c(
#       summary_cell(s, "ly_comparator"),
#       summary_cell(s, "ly_intervention")
#     ),
#     Total_QALY = c(
#       summary_cell(s, "qaly_comparator"),
#       summary_cell(s, "qaly_intervention")
#     ),
#     Incremental_Cost = c(
#       "",
#       summary_cell(s, "incremental_cost")
#     ),
#     Incremental_LY = c(
#       "",
#       summary_cell(s, "incremental_ly")
#     ),
#     Incremental_QALY = c(
#       "",
#       summary_cell(s, "incremental_qaly")
#     ),
#     ICER = c(
#       "",
#       psa_mean_icer(s)
#     ),
#     NMB = c(
#       "",
#       summary_cell(s, "nmb")
#     ),
#     check.names = FALSE,
#     stringsAsFactors = FALSE
#   )
# })
# 
# 
# output$psaBaseCase <- DT::renderDT({
#   DT::datatable(
#     base_case_table(),
#     escape = FALSE,
#     rownames = FALSE,
#     class = "compact stripe hover",
#     options = list(
#       dom = "t",
#       paging = FALSE,
#       ordering = FALSE,
#       searching = FALSE,
#       info = FALSE,
#       autoWidth = FALSE,   # IMPORTANT CHANGE
#       scrollX = TRUE,
#       columnDefs = list(
#         list(className = "dt-center", targets = "_all"),
#         list(className = "dt-left", targets = 0)
#       )
#     )
#   )
# })
# 
# 
# #########################################################
# #### DOWNLOAD BASE CASE #################################
# #########################################################
# output$download_psa_summary <- downloadHandler(
#   filename = function(){
#     paste0("PSA_Base_Case_", Sys.Date(), ".csv")
#   },
#   content = function(file){
#     write.csv(
#       base_case_table(),
#       file,
#       row.names = FALSE
#     )
#   }
# )

#########################################################
#### BASE CASE DATA #####################################
#########################################################
base_case_table <- reactive({
  s <- psa_summary()
  ly_comp <- get_summary_row(s, "ly_comparator")
  ly_int  <- get_summary_row(s, "ly_intervention")
  data.frame(
    Treatment = c("Comparator", "Intervention"),
    Total_Cost = c(
      summary_cell(s, "total_cost_comparator"),
      summary_cell(s, "total_cost_intervention")
    ),
    Total_LY = c(
      fmt_num(ly_comp$Mean, 2),
      fmt_num(ly_int$Mean, 2)
    ),
    Total_QALY = c(
      summary_cell(s, "qaly_comparator"),
      summary_cell(s, "qaly_intervention")
    ),
    Incremental_Cost = c(
      "NA",
      summary_cell(s, "incremental_cost")
    ),
    Incremental_LY = c(
      "NA",
      fmt_num(get_summary_row(s, "incremental_ly")$Mean, 2)
    ),
    Incremental_QALY = c(
      "NA",
      summary_cell(s, "incremental_qaly")
    ),
    ICER = c(
      "NA",
      psa_mean_icer(s)
    ),
    NMB = c(
      "NA",
      summary_cell(s, "nmb")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})

#########################################################
#### BASE CASE TABLE ####################################
#########################################################
output$psaBaseCase <- DT::renderDT({
  DT::datatable(
    base_case_table(),
    escape = FALSE,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      info = FALSE,
      autoWidth = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = "_all"),
        list(className = "dt-left", targets = 0)
      )
    )
  )
})

# #########################################################
# #### LY NOTE ############################################
# #########################################################
# output$psaLYNote <- renderUI({
#   tags$div(
#     style = "margin-top:10px;
#              font-size:13px;
#              color:#666666;
#              font-style:italic;",
#     HTML(
#       "<strong>Note:</strong> Confidence intervals are not reported for life-years (LYs) because uncertainty in the survival curves was not propagated during the probabilistic sensitivity analysis. Consequently, LY estimates reflect deterministic survival projections, and only the mean values are presented."
#     )
#   )
# })

#########################################################
#### DOWNLOAD BASE CASE #################################
#########################################################
output$download_psa_summary <- downloadHandler(
  filename = function(){
    paste0("PSA_Base_Case_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(
      base_case_table(),
      file,
      row.names = FALSE
    )
  }
)

#########################################################
#### COST BREAKDOWN HELPERS #############################
#########################################################
get_summary_row <- function(df, outcome){
  row <- df[df$Outcome == outcome, ]
  if(nrow(row) == 0)
    return(NULL)
  row
}

pct_change <- function(s, int_name, comp_name){
  int <- get_summary_row(s, int_name)$Mean
  comp <- get_summary_row(s, comp_name)$Mean
  if(is.na(comp) || abs(comp) < 1e-10){
    return(NA)
  }
  round(100 * (int - comp) / comp, 2)
}

#########################################################
#### MEAN (95% CrI) CELL ###############################
#########################################################
summary_cell_html <- function(df, outcome){
  row <- get_summary_row(df, outcome)
  if(is.null(row))
    return("")
  paste0(
    fmt_num(row$Mean),
    " (",
    fmt_num(row$Percentile2.5),
    ", ",
    fmt_num(row$Percentile97.5),
    ")"
  )
}

#########################################################
#### INCREMENTAL CELL ##################################
#########################################################
incremental_cell_html <- function(df,
                                  intervention,
                                  comparator){
  int  <- get_summary_row(df, intervention)
  comp <- get_summary_row(df, comparator)
  if(is.null(int) || is.null(comp))
    return("")
  mean <- int$Mean - comp$Mean
  lci  <- int$Percentile2.5 - comp$Percentile2.5
  uci  <- int$Percentile97.5 - comp$Percentile97.5
  paste0(
    fmt_num(mean),
    " (",
    fmt_num(lci),
    ", ",
    fmt_num(uci),
    ")"
  )
}

#########################################################
#### COST BREAKDOWN DATA ###############################
#########################################################
cost_breakdown_table <- reactive({
  s <- psa_summary()
  data.frame(
    
    Cost_Component = c(
      "Treatment Cost",
      "Adverse Event Cost",
      "Health State Cost",
      "Total Cost"
    ),
    Intervention = c(
      summary_cell_html(s, "treatment_cost_intervention"),
      summary_cell_html(s, "ae_cost_intervention"),
      summary_cell_html(s, "health_state_cost_intervention"),
      summary_cell_html(s, "total_cost_intervention")
    ),
    Comparator = c(
      summary_cell_html(s, "treatment_cost_comparator"),
      summary_cell_html(s, "ae_cost_comparator"),
      summary_cell_html(s, "health_state_cost_comparator"),
      summary_cell_html(s, "total_cost_comparator")
    ),
    Incremental_Cost = c(
      incremental_cell_html(s,
                            "treatment_cost_intervention",
                            "treatment_cost_comparator"),
      incremental_cell_html(s,
                            "ae_cost_intervention",
                            "ae_cost_comparator"),
      incremental_cell_html(s,
                            "health_state_cost_intervention",
                            "health_state_cost_comparator"),
      incremental_cell_html(s,
                            "total_cost_intervention",
                            "total_cost_comparator")
    ),
    
    Percent_Change = c(
      pct_change(s, "treatment_cost_intervention","treatment_cost_comparator"),
      pct_change(s, "ae_cost_intervention","ae_cost_comparator"),
      pct_change(s, "health_state_cost_intervention","health_state_cost_comparator"),
      pct_change(s, "total_cost_intervention","total_cost_comparator")
    ),
    
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
})

output$psaCostBreakdown <- DT::renderDT({
  DT::datatable(
    cost_breakdown_table(),
    escape = FALSE,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      info = FALSE,
      autoWidth = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = 0),
        list(className = "dt-left", targets = c(1,2,3,4))
      )
    )
  )
})

#########################################################
#### DOWNLOAD COST TABLE ###############################
#########################################################
output$download_psa_costs <- downloadHandler(
  filename = function(){
    paste0("PSA_Cost_Breakdown_", Sys.Date(), ".csv")
  },
  content = function(file){
    s <- psa_summary()
        pct_change <- function(int_name, comp_name){
      int <- get_summary_row(s, int_name)$Mean
      comp <- get_summary_row(s, comp_name)$Mean
      if(is.na(comp) || abs(comp) < 1e-10){
        return(NA)
      }
      round(100 * (int - comp) / comp, 2)
    }
    write.csv(
      data.frame(
        Cost_Component = c(
          "Treatment Cost",
          "Adverse Event Cost",
          "Health State Cost",
          "Total Cost"
        ),
        Intervention = c(
          fmt_num(get_summary_row(s,"treatment_cost_intervention")$Mean),
          fmt_num(get_summary_row(s,"ae_cost_intervention")$Mean),
          fmt_num(get_summary_row(s,"health_state_cost_intervention")$Mean),
          fmt_num(get_summary_row(s,"total_cost_intervention")$Mean)
        ),
        Comparator = c(
          fmt_num(get_summary_row(s,"treatment_cost_comparator")$Mean),
          fmt_num(get_summary_row(s,"ae_cost_comparator")$Mean),
          fmt_num(get_summary_row(s,"health_state_cost_comparator")$Mean),
          fmt_num(get_summary_row(s,"total_cost_comparator")$Mean)
        ),
        Incremental = c(
          fmt_num(
            get_summary_row(s,"treatment_cost_intervention")$Mean -
              get_summary_row(s,"treatment_cost_comparator")$Mean
          ),
          fmt_num(
            get_summary_row(s,"ae_cost_intervention")$Mean -
              get_summary_row(s,"ae_cost_comparator")$Mean
          ),
          fmt_num(
            get_summary_row(s,"health_state_cost_intervention")$Mean -
              get_summary_row(s,"health_state_cost_comparator")$Mean
          ),
          fmt_num(
            get_summary_row(s,"total_cost_intervention")$Mean -
              get_summary_row(s,"total_cost_comparator")$Mean
          )
        ),
        Percent_Change = c(
          pct_change("treatment_cost_intervention","treatment_cost_comparator"),
          pct_change("ae_cost_intervention","ae_cost_comparator"),
          pct_change("health_state_cost_intervention","health_state_cost_comparator"),
          pct_change("total_cost_intervention","total_cost_comparator")
        )
      ),
      file,
      row.names = FALSE
    )
  }
)


#########################################################
#### QALY / evLY / HYT BREAKDOWN ########################
#########################################################
output$psaQALYBreakdown <- DT::renderDT({
  s <- psa_summary()
  #######################################################
  #### Helper ###########################################
  #######################################################
  fmt_ci <- function(mean, lci, uci){
    paste0(
      fmt_num(mean, 2),
      " (",
      fmt_num(lci, 2),
      ", ",
      fmt_num(uci, 2),
      ")"
    )
  }
  #######################################################
  #### QALYs ############################################
  #######################################################
  q_int  <- get_summary_row(s, "qaly_intervention")
  q_comp <- get_summary_row(s, "qaly_comparator")
  q_inc  <- get_summary_row(s, "incremental_qaly")
  #######################################################
  #### evLY #############################################
  #######################################################
  ev_int  <- get_summary_row(s, "evly_intervention")
  ev_comp <- get_summary_row(s, "evly_comparator")
  ev_inc  <- get_summary_row(s, "incremental_evly")
  #######################################################
  #### HYT ##############################################
  #######################################################
  hyt_int  <- get_summary_row(s, "hyt_intervention")
  hyt_comp <- get_summary_row(s, "hyt_comparator")
  hyt_inc  <- get_summary_row(s, "incremental_hyt")
  #######################################################
  #### Table ############################################
  #######################################################
  qaly_table <- data.frame(
    Component = c(
      "Total QALYs",
      "Total evLYs",
      "Total HYTs"
    ),
    Intervention = c(
      fmt_ci(q_int$Mean, q_int$Percentile2.5, q_int$Percentile97.5),
      fmt_ci(ev_int$Mean, ev_int$Percentile2.5, ev_int$Percentile97.5),
      fmt_ci(hyt_int$Mean, hyt_int$Percentile2.5, hyt_int$Percentile97.5)
    ),
    Comparator = c(
      fmt_ci(q_comp$Mean, q_comp$Percentile2.5, q_comp$Percentile97.5),
      fmt_ci(ev_comp$Mean, ev_comp$Percentile2.5, ev_comp$Percentile97.5),
      fmt_ci(hyt_comp$Mean, hyt_comp$Percentile2.5, hyt_comp$Percentile97.5)
    ),
    Incremental = c(
      fmt_ci(q_inc$Mean, q_inc$Percentile2.5, q_inc$Percentile97.5),
      fmt_ci(ev_inc$Mean, ev_inc$Percentile2.5, ev_inc$Percentile97.5),
      fmt_ci(hyt_inc$Mean, hyt_inc$Percentile2.5, hyt_inc$Percentile97.5)
    ),
    Percent_Change = c(
      round(100 * q_inc$Mean / q_comp$Mean, 2),
      round(100 * ev_inc$Mean / ev_comp$Mean, 2),
      round(100 * hyt_inc$Mean / hyt_comp$Mean, 2)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  DT::datatable(
    qaly_table,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      autoWidth = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  )
})

# #########################################################
# #### DOWNLOAD QALY SUMMARY ##############################
# #########################################################
output$download_psa_qaly <- downloadHandler(
  filename = function(){
    paste0("PSA_Health_Outcomes_", Sys.Date(), ".csv")
  },
  content = function(file){
    s <- psa_summary()
    fmt_ci <- function(mean, lci, uci){
      paste0(
        fmt_num(mean, 2),
        " (",
        fmt_num(lci, 2),
        ", ",
        fmt_num(uci, 2),
        ")"
      )
    }
    rows <- list(
      c("qaly_intervention","qaly_comparator","incremental_qaly","Total QALYs"),
      c("evly_intervention","evly_comparator","incremental_evly","Total evLYs"),
      c("hyt_intervention","hyt_comparator","incremental_hyt","Total HYTs")
    )
    export <- do.call(rbind, lapply(rows, function(x){
      int  <- get_summary_row(s, x[1])
      comp <- get_summary_row(s, x[2])
      inc  <- get_summary_row(s, x[3])
      data.frame(
        Component = x[4],
        Intervention = fmt_ci(int$Mean,int$Percentile2.5,int$Percentile97.5),
        Comparator = fmt_ci(comp$Mean,comp$Percentile2.5,comp$Percentile97.5),
        Incremental = fmt_ci(inc$Mean,inc$Percentile2.5,inc$Percentile97.5),
        Percent_Change = round(100 * inc$Mean / comp$Mean, 2),
        stringsAsFactors = FALSE
      )
    }))
    write.csv(export, file, row.names = FALSE)
  }
)

#########################################################
#### SYNCHRONIZED CE DATASET ############################
#########################################################
ce_plane_data <- reactive({
  # PSA iterations (full simulation cloud)
  iter <- psa_iterations()
  # PSA summary (used for validation only)
  summ <- psa_summary()
  validate(
    need(!is.null(iter), "PSA iterations not available"),
    need(!is.null(summ), "PSA summary not available")
  )
  # -----------------------------
  # FORCE NUMERIC SAFETY
  # -----------------------------
  iter$incremental_qaly <- as.numeric(iter$incremental_qaly)
  iter$incremental_cost <- as.numeric(iter$incremental_cost)
  # -----------------------------
  # REMOVE INVALID ROWS
  # -----------------------------
  iter <- iter[
    is.finite(iter$incremental_qaly) &
      is.finite(iter$incremental_cost) &
      !is.na(iter$incremental_qaly) &
      !is.na(iter$incremental_cost),
  ]
  # -----------------------------
  # ALIGNMENT CHECK (IMPORTANT)
  # -----------------------------
  if (nrow(iter) != nrow(summ$psa_results)) {
    warning("PSA iteration mismatch with summary object — check run_psa output alignment.")
  }
  # -----------------------------
  # RETURN CLEAN CE DATASET
  # -----------------------------
  return(iter)
})

bcea_object <- reactive({
  req(model_object())
  iter <- model_object()$results$psa_results
  validate(
    need(!is.null(iter), "PSA iterations not available")
  )
  # Ensure numeric safety
  iter$incremental_qaly <- as.numeric(iter$incremental_qaly)
  iter$incremental_cost <- as.numeric(iter$incremental_cost)
  # BCEA expects: rows = simulations, columns = interventions
  e <- cbind(iter$incremental_qaly, 0)
  c <- cbind(iter$incremental_cost, 0)
  BCEA::bcea(
    e = e,
    c = c,
    ref = 2,
    interventions = c("Intervention", "Comparator"),
    k = seq(0, 200000, by = 1000)
  )
})

bcea_ce_plane <- reactive({
  iter <- model_object()$results$psa_results
  validate(need(!is.null(iter), "No PSA data"))
  iter$incremental_qaly <- as.numeric(iter$incremental_qaly)
  iter$incremental_cost <- as.numeric(iter$incremental_cost)
  data.frame(
    incremental_qaly = iter$incremental_qaly,
    incremental_cost = iter$incremental_cost
  )
})


output$cePlane <- renderPlotly({
  df <- bcea_ce_plane()
  req(nrow(df) > 0)
  #################################################
  #### Average PSA Point ##########################
  #################################################
  avg_qaly <- mean(df$incremental_qaly, na.rm = TRUE)
  avg_cost <- mean(df$incremental_cost, na.rm = TRUE)
  #################################################
  #### Base-case ##################################
  #################################################
  base_results <- model_object()$results$base_case_results
  base_qaly <- base_results$Incremental_QALY[
    base_results$Treatment == "Intervention"
  ]
  base_cost <- base_results$Incremental_Cost[
    base_results$Treatment == "Intervention"
  ]
  #################################################
  #### WTP ########################################
  #################################################
  wtp <- input$WTP
  if (is.null(wtp) || is.na(wtp))
    wtp <- 30000
  #################################################
  #### Plot limits ################################
  #################################################
  x_lim <- max(abs(c(df$incremental_qaly, avg_qaly, base_qaly)), na.rm = TRUE)
  y_lim <- max(abs(c(df$incremental_cost, avg_cost, base_cost)), na.rm = TRUE)
  x_min <- -x_lim
  x_max <-  x_lim
  y_min <- -y_lim
  y_max <-  y_lim
  #################################################
  #### Plot #######################################
  #################################################
  plot_ly() %>%
    ## PSA cloud
    add_markers(
      data = df,
      x = ~incremental_qaly,
      y = ~incremental_cost,
      name = "PSA iterations",
      marker = list(
        size = 4,
        opacity = 0.35,
        color = "#6B8FD6"
      ),
      hoverinfo = "none",
      showlegend = TRUE
    ) %>%
    ## Base-case ICER
    add_markers(
      x = base_qaly,
      y = base_cost,
      name = "Base-case ICER",
      marker = list(
        symbol = "triangle-up",
        size = 13,
        color = "#2ECC71",
        line = list(color = "black", width = 1)
      ),
      hoverinfo = "none",
      showlegend = TRUE
    ) %>%
    ## Mean PSA ICER
    add_markers(
      x = avg_qaly,
      y = avg_cost,
      name = "Mean PSA estimate",
      marker = list(
        symbol = "circle-open",
        size = 12,
        color = "red",
        line = list(width = 2)
      ),
      hoverinfo = "none",
      showlegend = TRUE
    ) %>%
    ## WTP label
    add_annotations(
      x = x_max * 0.80,
      y = wtp * x_max * 0.80,
      text = paste0(
        "<b>WTP = ",
        currency_symbol(),
        format(round(wtp), big.mark = ","),
        "</b>"
      ),
      showarrow = FALSE,
      font = list(
        color = "#2563EB",
        size = 12
      )
    ) %>%
    layout(
      margin = list(l = 70, r = 30, t = 20, b = 40),
      xaxis = list(
        title = "Incremental QALYs",
        range = c(x_min, x_max),
        zeroline = FALSE
      ),
      yaxis = list(
        title = paste0(
          "Incremental Cost (",
          currency_symbol(),
          ")"
        ),
        range = c(y_min, y_max),
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.08
      ),
      shapes = list(
        ## Vertical axis
        list(
          type = "line",
          x0 = 0, x1 = 0,
          y0 = y_min, y1 = y_max,
          line = list(color = "black")
        ),
        ## Horizontal axis
        list(
          type = "line",
          x0 = x_min, x1 = x_max,
          y0 = 0, y1 = 0,
          line = list(color = "black")
        ),
        ## WTP line
        list(
          type = "line",
          x0 = x_min,
          y0 = wtp * x_min,
          x1 = x_max,
          y1 = wtp * x_max,
          line = list(
            color = "#2563EB",
            dash = "dash",
            width = 2
          )
        )
      )
    )
})

#########################################################
#### CEAC DATA ##########################################
#########################################################
ceac_data <- reactive({
  req(model_object())
  iter <- model_object()$results$psa_results
  validate(
    need(!is.null(iter), "No PSA iterations available")
  )
  iter$incremental_cost <- as.numeric(iter$incremental_cost)
  iter$incremental_qaly <- as.numeric(iter$incremental_qaly)
  iter <- iter[
    is.finite(iter$incremental_cost) &
      is.finite(iter$incremental_qaly),
  ]
  v_wtp <- seq(0, 200000, by = 1000)
  ceac_intervention <- sapply(v_wtp, function(k){
    mean(k * iter$incremental_qaly -
           iter$incremental_cost > 0)
    
  })
  ceac_comparator <- 1 - ceac_intervention
  data.frame(
    WTP = v_wtp,
    Intervention = ceac_intervention,
    Comparator = ceac_comparator
  )
})

output$ceac <- renderPlotly({
  df <- ceac_data()
  req(nrow(df) > 0)
  plot_ly(df) %>%
    add_trace(
      x = ~WTP,
      y = ~Intervention,
      type = "scatter",
      mode = "lines+markers",
      name = "Intervention",
      line = list(
        color = "#16A34A",
        width = 2.5
      ),
      marker = list(
        symbol = "circle",
        size = 5,
        color = "#16A34A"
      )
    ) %>%
    add_trace(
      x = ~WTP,
      y = ~Comparator,
      type = "scatter",
      mode = "lines+markers",
      name = "Comparator",
      line = list(
        color = "#2563EB",
        width = 2.5
      ),
      marker = list(
        symbol = "circle",
        size = 5,
        color = "#2563EB"
      )
    ) %>%
    layout(
      xaxis = list(
        title = paste0(
          "Cost-effectiveness threshold (",
          currency_symbol(),
          " thousands)"
        ),
        tickvals = seq(10000, 190000, 20000),
        ticktext = seq(10, 190, 20),
        range = c(0, 200000)
      ),
      yaxis = list(
        title = "Probability cost-effective",
        range = c(0, 1)
      ),
      legend = list(
        orientation = "h",
        x = 0.3,
        y = 1.08
      )
    )
})

#########################################################
#### For treatment cost correction ######################
#########################################################
currency_symbol <- reactive({
  switch(
    input$currency,
    "USD" = "$",
    "EUR" = "€",
    "GBP" = "£",
    # "CHF" = "CHF",
    # "SEK" = "SEK",
    # "NOK" = "NOK",
    # "DKK" = "DKK",
    "£"
  )
})

output$cost_currency_note <- renderUI({
  tags$div(
    style = "
      background:#F8F9FA;
      border-left:4px solid #3C8DBC;
      padding:10px 15px;
      border-radius:5px;
      color:#555;
      font-size:13px;
    ",
    icon("circle-info"),
    HTML(sprintf(
      "<b>Note:</b> All cost results presented throughout this analysis are reported in <b>%s</b>.",
      currency_symbol()
    ))
  )
})


