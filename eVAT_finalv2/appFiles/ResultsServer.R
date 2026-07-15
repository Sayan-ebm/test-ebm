#########################################################
#### Base Case Results Table ############################
#########################################################
output$basecase_table <- DT::renderDT({
  #######################################################
  #### Get Results ######################################
  #######################################################
  req(model_object())
  df <- model_object()$results$base_case_results
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(
    df[num_cols],
    function(x) {
      ifelse(
        is.na(x),
        "NA",
        sprintf("%.2f", round(x, 2))
      )
    }
  )
  #######################################################
  #### Display Table ####################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  )
})

#########################################################
#### Download ###########################################
#########################################################
output$download_basecase <- downloadHandler(
  filename = function() {
    "Base_Case_Results.csv"
  },
  content = function(file) {
    write.csv(model_object()$results$base_case_results, file, row.names = FALSE)
  }
)

#########################################################
#### Economically Justifiable Price Table ###############
#########################################################
output$ejp_table <- DT::renderDT({
  req(model_object())
  df <- model_object()$results$economically_justifiable_price
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  df$Current_Price_Per_Cycle <- round(df$Current_Price_Per_Cycle, 2)
  df$Economically_Justifiable_Price <-
    ifelse(
      is.na(df$Economically_Justifiable_Price),
      "NA",
      sprintf("%.2f", df$Economically_Justifiable_Price)
    )
  df$Difference <-
    ifelse(
      is.na(df$Difference),
      "NA",
      sprintf("%.2f", df$Difference)
    )
  #######################################################
  #### Table ############################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  )
})

#########################################################
#### EJP Note ###########################################
#########################################################
output$ejp_note <- renderUI({
  req(model_object())
  df <- model_object()$results$economically_justifiable_price
  #######################################################
  #### Only show if any EJP is NA #######################
  #######################################################
  if(any(is.na(df$Economically_Justifiable_Price))){
    div(
      style = "
        margin-top:10px;
        padding:10px 12px;
        background:#FCF8E3;
        border-left:4px solid #F0AD4E;
        border-radius:4px;
        color:#8A6D3B;
        font-size:13px;
      ",
      tags$b("Note: "),
      "NA indicates that the calculated economically justifiable price is negative and therefore not applicable."
    )
  } else {
    NULL
  }
})

#########################################################
#### Download EJP #######################################
#########################################################
output$download_ejp <- downloadHandler(
  filename = function() {
    paste0(
      "Economically_Justifiable_Prices_",
      Sys.Date(),
      ".csv"
    )
  },
  content = function(file) {
    write.csv(
      model_object()$results$economically_justifiable_price,
      file,
      row.names = FALSE,
      na = ""
    )
  }
)

#########################################################
#### Additional Outcomes Table ##########################
#########################################################
output$additional_outcomes_table <- DT::renderDT({
  #######################################################
  #### Get Results ######################################
  #######################################################
  req(model_object())
  df <- model_object()$results$additional_outcomes
  req(!is.null(df))
  req(nrow(df) > 0)
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  numeric_cols <- c("Intervention", "Comparator", "Incremental")
  df[numeric_cols] <- lapply(
    df[numeric_cols],
    function(x) fmt_num(x, 2)
  )
  #######################################################
  #### Display Table ####################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
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
        list(className = "dt-left", targets = 1:3)
      )
    )
  )
  
})


#########################################################
#### Download Additional Outcomes #######################
#########################################################
output$download_additional_outcomes <- downloadHandler(
  filename = function() {
    paste0(
      "Additional_Economic_Outcomes_",
      Sys.Date(),
      ".csv"
    )
  },
  content = function(file) {
    req(model_object())
    write.csv(
      model_object()$results$additional_outcomes,
      file,
      row.names = FALSE
    )
  }
)

#########################################################
#### Cost Breakdown Data ###############################
#########################################################
cost_breakdown_df <- reactive({
  req(model_object())
  int_breakdown <- model_object()$results$treatment_costs$intervention_breakdown
  comp_breakdown <- model_object()$results$treatment_costs$comparator_breakdown
  cost_change_pct <- function(int_val, comp_val) {
    if (is.na(comp_val) || abs(comp_val) < 1e-10) {
      return(NA_real_)
    }
    100 * (int_val - comp_val) / comp_val
  }
  therapy_names <- unique(c(int_breakdown$Cost_Category, comp_breakdown$Cost_Category))
  therapy_table <- data.frame(
    Cost_Component = therapy_names,
    stringsAsFactors = FALSE
  )
  
  therapy_table$Intervention <- sapply(
    therapy_names,
    function(x) {
      val <- int_breakdown$Intervention[
        int_breakdown$Cost_Category == x
      ]
      if(length(val) == 0) 0 else val
    }
  )
  therapy_table$Comparator <- sapply(
    therapy_names,
    function(x) {
      val <- comp_breakdown$Comparator[
        comp_breakdown$Cost_Category == x
      ]
      if(length(val) == 0) 0 else val
    }
  )
  therapy_table$Incremental_Cost <- therapy_table$Intervention - therapy_table$Comparator
  therapy_table$Percent_Change <-
    mapply(
      cost_change_pct,
      therapy_table$Intervention,
      therapy_table$Comparator
    )
  #########################################################
  #### Summary ############################################
  #########################################################
  summary_table <- data.frame(
    Cost_Component = c(
      "Treatment Cost Total",
      "AE Cost",
      "Health State Cost",
      "Total Cost"
    ),
    Intervention = c(
      sum(therapy_table$Intervention),
      model_object()$intervention$ae$total_cost_discounted,
      model_object()$intervention$health_state$cost$discounted,
      sum(therapy_table$Intervention) +
        model_object()$intervention$ae$total_cost_discounted +
        model_object()$intervention$health_state$cost$discounted
    ),
    Comparator = c(
      sum(therapy_table$Comparator),
      model_object()$comparator$ae$total_cost_discounted,
      model_object()$comparator$health_state$cost$discounted,
      sum(therapy_table$Comparator) +
        model_object()$comparator$ae$total_cost_discounted +
        model_object()$comparator$health_state$cost$discounted
    ),
    stringsAsFactors = FALSE
  )
  summary_table$Incremental_Cost <- summary_table$Intervention - summary_table$Comparator
  summary_table$Percent_Change <-
    mapply(
      cost_change_pct,
      summary_table$Intervention,
      summary_table$Comparator
    )
  #########################################################
  #### Final ##############################################
  #########################################################
  final_table <- rbind(
    therapy_table,
    summary_table
  )
  final_table
})

#########################################################
#### Cost Breakdown Table ###############################
#########################################################
output$cost_breakdown_table <- DT::renderDT({
  req(cost_breakdown_df())
  df <- cost_breakdown_df()
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(
    df[num_cols],
    function(x) sprintf("%.2f", round(x, 2))
  )
  #######################################################
  #### Table ############################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  ) %>%
    DT::formatStyle(
      "Cost_Component",
      target = "row",
      fontWeight = DT::styleEqual(
        c("Treatment Cost Total", "Total Cost"),
        c("bold", "bold")
      ),
      backgroundColor = DT::styleEqual(
        c("Treatment Cost Total", "Total Cost"),
        c("#FFF3CD", "#D9EDF7")
      )
    )
})

output$download_costs <- downloadHandler(
  filename = function() {
    paste0("Disaggregated_Costs_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(cost_breakdown_df(), file, row.names = FALSE)
  }
)

#########################################################
#### Cost Breakdown Plot ###############################
#########################################################
output$cost_breakdown_plot <- renderPlotly({
  req(model_object())
  #######################################################
  #### Treatment Costs ##################################
  #######################################################
  int_breakdown <- model_object()$results$treatment_costs$intervention_breakdown
  comp_breakdown <- model_object()$results$treatment_costs$comparator_breakdown
  therapy_df <- merge(int_breakdown, comp_breakdown, by = "Cost_Category", all = TRUE)
  therapy_df[is.na(therapy_df)] <- 0
  #######################################################
  #### Build Plot Data ##################################
  #######################################################
  plot_df <- data.frame()
  for(i in seq_len(nrow(therapy_df))){
    plot_df <- rbind(
      plot_df,
      data.frame(
        Arm = "Intervention",
        Component = therapy_df$Cost_Category[i],
        Value = therapy_df$Intervention[i]
      ),
      data.frame(
        Arm = "Comparator",
        Component = therapy_df$Cost_Category[i],
        Value = therapy_df$Comparator[i]
      )
    )
  }
  #######################################################
  #### Add AE Costs #####################################
  #######################################################
  plot_df <- rbind(
    plot_df,
    data.frame(
      Arm = c("Intervention","Comparator"),
      Component = "AE Cost",
      Value = c(
        model_object()$intervention$ae$total_cost_discounted,
        model_object()$comparator$ae$total_cost_discounted
      )
    ),
    data.frame(
      Arm = c("Intervention","Comparator"),
      Component = "Health State Cost",
      Value = c(
        model_object()$intervention$health_state$cost$discounted,
        model_object()$comparator$health_state$cost$discounted
      )
    )
  )
  #######################################################
  #### Dynamic Y-Axis ###################################
  #######################################################
  totals <- aggregate(Value ~ Arm, plot_df, sum)
  ymax <- ceiling(max(totals$Value) / 10000) * 10000
  #######################################################
  #### Plot #############################################
  #######################################################
  plotly::plot_ly(
    data = plot_df,
    x = ~Arm,
    y = ~Value,
    color = ~Component,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = paste0("Cost (", currency_symbol(), ")"),
        range = c(0, ymax)
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.20
      )
    )
})

#########################################################
#### LY Breakdown Data ##################################
#########################################################
ly_breakdown_df <- reactive({
  req(model_object())
  int_cycles <- model_object()$intervention$health_state$cycle_results
  comp_cycles <- model_object()$comparator$health_state$cycle_results
  pct_change <- function(int_val, comp_val){
    if(is.na(comp_val) || abs(comp_val) < 1e-10){
      return(NA_real_)
    }
    100 * (int_val - comp_val) / comp_val
  }
  #######################################################
  #### Intervention #####################################
  #######################################################
  int_total_ly <- sum(int_cycles$LY_Discounted, na.rm = TRUE)
  int_pf_weight <- sum(int_cycles$PF, na.rm = TRUE)
  int_pd_weight <- sum(int_cycles$PD, na.rm = TRUE)
  int_pf_ly <- int_total_ly *int_pf_weight / (int_pf_weight + int_pd_weight)
  int_pd_ly <- int_total_ly *int_pd_weight / (int_pf_weight + int_pd_weight)
  #######################################################
  #### Comparator #######################################
  #######################################################
  comp_total_ly <- sum(comp_cycles$LY_Discounted, na.rm = TRUE)
  comp_pf_weight <- sum(comp_cycles$PF, na.rm = TRUE)
  comp_pd_weight <- sum(comp_cycles$PD, na.rm = TRUE)
  comp_pf_ly <- comp_total_ly*comp_pf_weight / (comp_pf_weight + comp_pd_weight)
  comp_pd_ly <- comp_total_ly*comp_pd_weight / (comp_pf_weight + comp_pd_weight)
  #######################################################
  #### Final Table ######################################
  #######################################################
  df <- data.frame(
    Component = c("PF LYs", "PD LYs", "Total LYs"),
    Intervention = c(int_pf_ly, int_pd_ly, int_total_ly),
    Comparator = c(comp_pf_ly, comp_pd_ly, comp_total_ly),
    stringsAsFactors = FALSE
  )
  df$Incremental_LY <- df$Intervention - df$Comparator
  df$Percent_Change <- mapply(pct_change, df$Intervention, df$Comparator)
  df
})

#########################################################
#### LY Breakdown Table #################################
#########################################################
output$ly_breakdown_table <- DT::renderDT({
  req(ly_breakdown_df())
  df <- ly_breakdown_df()
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(
    df[numeric_cols],
    function(x) sprintf("%.2f", round(x, 2))
  )
  #######################################################
  #### Table ############################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  ) %>%
    DT::formatStyle(
      "Component",
      target = "row",
      fontWeight = DT::styleEqual(
        "Total LYs",
        "bold"
      ),
      backgroundColor = DT::styleEqual(
        "Total LYs",
        "#D9EDF7"
      )
    )
})

#########################################################
#### Download LY Breakdown ##############################
#########################################################
output$download_lys <- downloadHandler(
  filename = function(){
    paste0("Disaggregated_LYs_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(ly_breakdown_df(), file, row.names = FALSE)
  }
)

#########################################################
#### LY Breakdown Plot ##################################
#########################################################
output$ly_breakdown_plot <- renderPlotly({
  req(ly_breakdown_df())
  plot_df <- subset(ly_breakdown_df(), Component != "Total LYs")
  plot_df <- rbind(
    data.frame(Arm = "Comparator", Component = plot_df$Component, Value = plot_df$Comparator),
    data.frame(Arm = "Intervention", Component = plot_df$Component, Value = plot_df$Intervention)
  )
  totals <- aggregate(Value ~ Arm, plot_df, sum)
  ymax <- ceiling(max(totals$Value))
  plotly::plot_ly(data = plot_df, x = ~Arm, y = ~Value, color = ~Component, type = "bar") %>%
    plotly::layout(barmode = "stack", xaxis = list(title = ""),
                   yaxis = list(title = "Life Years", range = c(0, ymax)),
                   legend = list(orientation = "h", x = 0, y = -0.20)
    )
})

#########################################################
#### QALY Breakdown Data ################################
#########################################################
qaly_breakdown_df <- reactive({
  req(model_object())
  int_cycles <- model_object()$intervention$health_state$cycle_results
  comp_cycles <- model_object()$comparator$health_state$cycle_results
  pct_change <- function(int_val, comp_val){
    if(is.na(comp_val) || abs(comp_val) < 1e-10){
      return(NA_real_)
    }
    100 * (int_val - comp_val) / comp_val
  }
  #######################################################
  #### Intervention #####################################
  #######################################################
  int_pf_weight <- sum(int_cycles$PF_QALY, na.rm = TRUE)
  int_pd_weight <- sum(int_cycles$PD_QALY, na.rm = TRUE)
  int_ae_disutility <- sum(int_cycles$AE_Disutility_Discounted, na.rm = TRUE)
  int_total_qaly <- sum(int_cycles$QALY_Discounted, na.rm = TRUE)
  int_pf_qaly <- int_total_qaly * int_pf_weight / (int_pf_weight + int_pd_weight)
  int_pd_qaly <- int_total_qaly * int_pd_weight / (int_pf_weight + int_pd_weight)
  #######################################################
  #### Comparator #######################################
  #######################################################
  comp_pf_weight <- sum(comp_cycles$PF_QALY, na.rm = TRUE)
  comp_pd_weight <- sum(comp_cycles$PD_QALY, na.rm = TRUE)
  comp_ae_disutility <- sum(comp_cycles$AE_Disutility_Discounted, na.rm = TRUE)
  comp_total_qaly <- sum(comp_cycles$QALY_Discounted, na.rm = TRUE)
  comp_pf_qaly <- comp_total_qaly * comp_pf_weight / (comp_pf_weight + comp_pd_weight)
  comp_pd_qaly <- comp_total_qaly * comp_pd_weight / (comp_pf_weight + comp_pd_weight)
  #######################################################
  #### Final Table ######################################
  #######################################################
  df <- data.frame(
    Component = c("PF QALYs", "PD QALYs", "AE Disutility", "Total QALYs"),
    Intervention = c(int_pf_qaly, int_pd_qaly, int_ae_disutility, int_total_qaly),
    Comparator = c(comp_pf_qaly, comp_pd_qaly, comp_ae_disutility, comp_total_qaly),
    stringsAsFactors = FALSE
  )
  df$Incremental_QALY <- df$Intervention - df$Comparator
  df$Percent_Change <- mapply(pct_change, df$Intervention, df$Comparator)
  df
})

#########################################################
#### QALY Breakdown Table ###############################
#########################################################
output$qaly_breakdown_table <- DT::renderDT({
  req(qaly_breakdown_df())
  df <- qaly_breakdown_df()
  #######################################################
  #### Format Numeric Columns ###########################
  #######################################################
  numeric_cols <- sapply(df, is.numeric)
  df[numeric_cols] <- lapply(
    df[numeric_cols],
    function(x) sprintf("%.2f", round(x, 2))
  )
  #######################################################
  #### Table ############################################
  #######################################################
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover",
    options = list(
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      info = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-left", targets = "_all")
      )
    )
  ) %>%
    DT::formatStyle(
      "Component",
      target = "row",
      fontWeight = DT::styleEqual(
        "Total QALYs",
        "bold"
      ),
      backgroundColor = DT::styleEqual(
        "Total QALYs",
        "#D9EDF7"
      )
    )
})

#########################################################
#### Download QALY Breakdown ############################
#########################################################
output$download_qalys <- downloadHandler(
  filename = function(){
    paste0("Disaggregated_QALYs_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(qaly_breakdown_df(), file, row.names = FALSE)
  }
)

#########################################################
#### QALY Breakdown Plot ################################
#########################################################
output$qaly_breakdown_plot <- renderPlotly({
  req(qaly_breakdown_df())
  #######################################################
  #### Keep PF and PD only ##############################
  #######################################################
  plot_df <- subset(qaly_breakdown_df(), Component %in% c("PF QALYs", "PD QALYs"))
  #######################################################
  #### Convert to Long Format ###########################
  #######################################################
  plot_df <- rbind(
    data.frame(Arm = "Comparator", Component = plot_df$Component, Value = plot_df$Comparator),
    data.frame(Arm = "Intervention", Component = plot_df$Component, Value = plot_df$Intervention)
  )
  #######################################################
  #### Axis Limit #######################################
  #######################################################
  totals <- aggregate(
    Value ~ Arm,
    plot_df,
    sum)
  ymax <- ceiling(max(totals$Value))
  #######################################################
  #### Plot #############################################
  #######################################################
  plot_ly(
    data = plot_df,
    x = ~Arm,
    y = ~Value,
    color = ~Component,
    type = "bar"
  ) %>%
    layout(
      barmode = "stack",
      yaxis = list(
        title = "Quality Adjusted Life Years",
        range = c(0, ymax)
      ),
      xaxis = list(
        title = ""
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.2
      )
    )
})


output$currency_note <- renderUI({
  tags$div(
    style = "
      margin-top:10px;
      margin-bottom:15px;
      padding:10px 14px;
      background:#F8F9FA;
      border-left:4px solid #3C8DBC;
      border-radius:4px;
      color:#555;
      font-size:13px;
    ",
    tags$i(
      class = "fa fa-circle-info",
      style = "margin-right:6px;color:#3C8DBC;"
    ),
    HTML(
      paste0(
        "<b>Note:</b> All costs presented in the deterministic analysis are reported in <b>",
        currency_symbol()
      )
    )
  )
})