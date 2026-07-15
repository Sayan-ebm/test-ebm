#========================================================
# SurvivalintaratorServer.R
#========================================================
#########################################################
#### Median function codes ##############################
#########################################################
model_times <- reactive({
  req(input$time_horizon)
  seq(
    from = 0,
    to   = input$time_horizon,
    by   = 1
  )
})

#########################################################
#### OS EXPONENTIAL CURVE ###############################
#########################################################
median_os_curve <- reactive({
  req(input$int_median_os)
  generate_exponential_survival(
    median_months = input$int_median_os,
    model_times   = model_times(),
    model_name = "Median OS"
  )
})

#########################################################
#### PFS EXPONENTIAL CURVE ##############################
#########################################################
median_pfs_curve <- reactive({
  req(input$int_median_pfs)
  generate_exponential_survival(
    median_months = input$int_median_pfs,
    model_times   = model_times(),
    model_name = "Median PFS"
  )
})

#########################################################
#### OPTIONAL KM DATA ###################################
#########################################################
km_os_data <- reactive({
  req(input$int_os_km_survival)
  km <- read.csv(
    input$int_os_km_survival$datapath
  )
  validate(
    need(
      all(c("time", "survival") %in% names(km)),
      "CSV must contain columns: time and survival"
    )
  )
  km
})

#########################################################
#### OPTIONAL KM PFS DATA ###############################
#########################################################
km_pfs_data <- reactive({
  req(input$int_pfs_km_survival)
  km <- read.csv(
    input$int_pfs_km_survival$datapath
  )
  validate(
    need(
      all(c("time", "survival") %in% names(km)),
      "CSV must contain columns: time and survival"
    )
  )
  km
})

output$download_os_survival_example <- downloadHandler(
  filename = function() {
    req(input$int_os_km_survival)
    input$int_os_km_survival$name
  },
  content = function(file) {
    req(input$int_os_km_survival)
    file.copy(
      from = input$int_os_km_survival$datapath,
      to = file,
      overwrite = TRUE
    )
  }
)
output$download_os_risk_example <- downloadHandler(
  filename = function() {
    req(input$int_os_km_risk)
    input$int_os_km_risk$name
  },
  content = function(file) {
    req(input$int_os_km_risk)
    file.copy(
      from = input$int_os_km_risk$datapath,
      to = file,
      overwrite = TRUE
    )
  }
)
#########################################################
#### OS PLOT ############################################
#########################################################
output$int_median_os_plot <- plotly::renderPlotly({
  req(input$int_os_input_type == "Median OS")
  data <- median_os_curve()
  p <- plotly::plot_ly()
  #------------------------------------------------------
  # EXPONENTIAL CURVE
  #------------------------------------------------------
  p <- p %>%
    plotly::add_lines(
      data = data,
      x = ~time,
      y = ~survival,
      name = "OS exponential distribution",
      line = list(width = 3)
    )
  #------------------------------------------------------
  # MEDIAN LINES
  #------------------------------------------------------
  # Vertical Median Line
  p <- p %>%
    plotly::add_segments(
      x = input$int_median_os,
      xend = input$int_median_os,
      y = 0,
      yend = 0.5,
      line = list(
        dash = "dash",
        width = 2,
        color = "orange"
      ),
      name = "Median OS"
    )
  # Horizontal Median Line
  p <- p %>%
    plotly::add_segments(
      x = 0,
      xend = input$int_median_os,
      y = 0.5,
      yend = 0.5,
      showlegend = FALSE,
      line = list(
        dash = "dash",
        width = 2,
        color = "orange"
      )
    )
  #------------------------------------------------------
  # OPTIONAL KM CURVE
  #------------------------------------------------------
  if (!is.null(input$int_os_km_survival)) {
    km <- km_os_data()
    p <- p %>%
      plotly::add_lines(
        data = km,
        x = ~time,
        y = ~survival,
        name = "Observed KM curve",
        line = list(dash = "dot")
      )
  }
  #------------------------------------------------------
  # LAYOUT
  #------------------------------------------------------
  p %>%
    plotly::layout(
      title = list(
        text = "Overall Survival (OS)",
        x = 0.05
      ),
      xaxis = list(
        title = "Time (Months)",
        range = c(0, input$time_horizon),
        showgrid = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Survival Probability",
        range = c(0, 1),
        showgrid = TRUE,
        zeroline = FALSE
      ),
      legend = list(
        x = 0.72,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.6)"
      ),
      hovermode = "x unified"
    )
})
#########################################################
#### PFS PLOT ###########################################
#########################################################
output$int_median_pfs_plot <- plotly::renderPlotly({
  req(input$int_pfs_input_type == "Median PFS")
  data <- median_pfs_curve()
  p <- plotly::plot_ly()
  #------------------------------------------------------
  # EXPONENTIAL CURVE
  #------------------------------------------------------
  p <- p %>%
    plotly::add_lines(
      data = data,
      x = ~time,
      y = ~survival,
      name = "PFS exponential distribution",
      line = list(width = 3)
    )
  #------------------------------------------------------
  # MEDIAN LINES
  #------------------------------------------------------
  # Vertical Median Line
  p <- p %>%
    plotly::add_segments(
      x = input$int_median_pfs,
      xend = input$int_median_pfs,
      y = 0,
      yend = 0.5,
      line = list(
        dash = "dash",
        width = 2,
        color = "orange"
      ),
      name = "Median PFS"
    )
  # Horizontal Median Line
  p <- p %>%
    plotly::add_segments(
      x = 0,
      xend = input$int_median_pfs,
      y = 0.5,
      yend = 0.5,
      showlegend = FALSE,
      line = list(
        dash = "dash",
        width = 2,
        color = "orange"
      )
    )
  #------------------------------------------------------
  # OPTIONAL KM CURVE
  #------------------------------------------------------
  if (!is.null(input$int_pfs_km_survival)) {
    km <- km_pfs_data()
    p <- p %>%
      plotly::add_lines(
        data = km,
        x = ~time,
        y = ~survival,
        name = "Observed KM curve",
        line = list(dash = "dot")
      )
  }
  #------------------------------------------------------
  # LAYOUT
  #------------------------------------------------------
  p %>%
    plotly::layout(
      title = list(
        text = "Progression-Free Survival (PFS)",
        x = 0.05
      ),
      xaxis = list(
        title = "Time (Months)",
        range = c(0, input$time_horizon),
        showgrid = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Survival Probability",
        range = c(0, 1),
        showgrid = TRUE,
        zeroline = FALSE
      ),
      legend = list(
        x = 0.72,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.6)"
      ),
      hovermode = "x unified"
    )
})
#########################################################
#### DEBUGGING ##########################################
#########################################################
observe({
  cat("\n====================================\n")
  cat("TIME HORIZON YEARS:", input$time_horizon, "\n")
  cat("TIME HORIZON MONTHS:", max(model_times()), "\n")
  cat("======================================\n")
})

#########################################################
#### OS Survival IPD data ###############################
#########################################################

#########################################################
#### IPD data ###########################################
#########################################################
read_ipd_data <- function(file){
  ipd <- read.csv(file$datapath)
  names(ipd) <- tolower(names(ipd))
  # if("event" %in% names(ipd)){
  #   ipd$status <- ipd$event
  # }
  ipd <- ipd[, c("time","status")]
  ipd
}

output$download_os_ipd_example <- downloadHandler(
  filename = function() {paste0("OS_IPD_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(
      input$int_os_input_type == "Survival Data",
      input$int_os_survival_type == "IPD"
    )
    write.csv(int_os_ipd(),file,row.names = FALSE)
  }
)

#########################################################
#### OS IPD input #######################################
#########################################################
int_os_ipd <- reactive({
  req(input$int_os_input_type == "Survival Data")
  req(input$int_os_survival_type)
  if (input$int_os_survival_type == "IPD") {
    req(input$int_os_ipd)
    read_ipd_data(input$int_os_ipd)
  } else {
    req(input$int_os_km_survival)
    req(input$int_os_km_risk)
    km_data <- read.csv(input$int_os_km_survival$datapath)
    risk_data <- read.csv(input$int_os_km_risk$datapath)
    reconstruct_ipd(
      n_at_risk = risk_data,
      p_survival = km_data
    )
  }
})

#########################################################
#### OS IPD Previews ####################################
#########################################################
# output$int_os_ipd_preview <- DT::renderDT({
#   req(
#     input$int_os_input_type == "Survival Data",
#     input$int_os_survival_type == "IPD"
#   )
#   DT::datatable(
#     int_os_ipd(),
#     rownames = FALSE,
#     options = list(
#       dom = "t",
#       pageLength = 10,
#       scrollY = "300px",
#       scrollCollapse = TRUE
#     )
#   )
# })

# output$int_os_reconstructed_ipd_preview <- DT::renderDT({
#   req(
#     input$int_os_input_type == "Survival Data",
#     input$int_os_survival_type == "Digitized KM Curve"
#   )
#   DT::datatable(
#     int_os_ipd(),
#     rownames = FALSE,
#     options = list(
#       dom = "t",
#       pageLength = 10,
#       scrollY = "300px",
#       scrollCollapse = TRUE
#     )
#   )
# })

#########################################################
#### OS IPD Downloads ###################################
#########################################################
observe({
  req(
    input$int_os_input_type == "Survival Data",
    input$int_os_survival_type == "IPD"
  )
  ipd <- int_os_ipd()
  cat("\n=========================\n")
  cat("OS IPD UPLOADED\n")
  cat("=========================\n")
  cat("Rows:", nrow(ipd), "\n")
  cat("Events:",
      sum(ipd$status == 1),
      "\n")
  cat("Censored:",
      sum(ipd$status == 0),
      "\n")
  cat("\nColumns:\n")
  print(names(ipd))
  cat("\nFirst 10 rows:\n")
  print(head(ipd,10))
})

output$download_os_reconstructed_ipd <- downloadHandler(
  filename = function() {
    paste0("reconstructed_OS_IPD_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(int_os_ipd())
    write.csv(int_os_ipd(), file, row.names = FALSE)
  }
)

#########################################################
#### Digitized KM curve codes ###########################
#########################################################

#########################################################
#### OS Parametric Models ###############################
#########################################################
int_os_models <- reactive({
  req(int_os_ipd())
  ipd <- int_os_ipd()
  cat("\n=========================\n")
  cat("OS IPD SUMMARY\n")
  cat("=========================\n")
  cat("Rows:", nrow(ipd), "\n")
  cat("Events:", sum(ipd$status == 1), "\n")
  cat("Censored:", sum(ipd$status == 0), "\n")
  print(head(ipd))
  fit_parametric_models(ipd)
})
#########################################################
#### OS Goodness of Fit Table ###########################
#########################################################
int_os_fit_table <- reactive({
  req(int_os_models())
  create_fit_table(int_os_models())
})

#########################################################
#### OS AIC TABLE #######################################
#########################################################
output$int_os_aic_table <- DT::renderDT({
  req(
    input$int_os_input_type == "Survival Data"
  )
  DT::datatable(
    int_os_fit_table(),
    rownames = FALSE,
    selection = "multiple",
    options = list(
      pageLength = 10,
      dom = "t"
    )
  )
})

output$download_int_os_aic_table <- downloadHandler(
  filename = function() {
    paste0("OS_Goodness_of_Fit_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(input$int_os_input_type == "Survival Data")
    write.csv(int_os_fit_table(), file, row.names = FALSE)
  }
)

output$debug_ipd <- renderPrint({
  ipd <- int_os_ipd()
  cat("Patients:", nrow(ipd), "\n")
  cat("Events:", sum(ipd$status == 1), "\n")
  cat("Censored:", sum(ipd$status == 0), "\n")
})

#########################################################
#### Create KM curve from IPD  ##########################
#########################################################
create_km_from_ipd <- function(ipd){
  stopifnot(all(c("time","status") %in% names(ipd)))
  fit <- survival::survfit(
    survival::Surv(time, status) ~ 1,
    data = ipd
  )
  data.frame(
    time = fit$time,
    surv = fit$surv,
    model = "Kaplan-Meier"
  )
}

#########################################################
#### Survival Predictions  ##############################
#########################################################
int_os_predictions <- reactive({
  req(int_os_models())
  req(input$time_horizon)
  generate_survival_predictions(
    int_os_models(),
    input$time_horizon
  )
})

#########################################################
#### Combined Survival data #############################
#########################################################
int_os_all_survival <- reactive({
  req(int_os_ipd())
  req(int_os_models())
  req(input$time_horizon)
  km <- create_km_from_ipd(int_os_ipd())
  preds <- int_os_predictions()
  dplyr::bind_rows(km, preds)
})

#########################################################
#### Model Selection ####################################
#########################################################
selected_int_os_models <- reactive({
  req(int_os_fit_table())
  tbl <- int_os_fit_table()
  rows <- input$int_os_aic_table_rows_selected
  if(is.null(rows) || length(rows) == 0){
    return(character(0))
  }
  tbl$Model[rows]
})

visible_int_os_survival <- reactive({
  surv <- int_os_all_survival()
  selected <- selected_int_os_models()
  if(length(selected) == 0){
    return(
      surv[surv$model == "Kaplan-Meier", ]
    )
  }
  surv[
    surv$model %in%
      c("Kaplan-Meier", selected),
  ]
})

#########################################################
#### Adding Active model ################################
#########################################################
output$int_os_active_model <- renderText({
  req(input$int_os_input_type)
  selected <- selected_int_os_models()
  if(length(selected) == 0){
    best_model <- int_os_fit_table()$Model[1]
    return(
      paste0("Active model: ",best_model," (Lowest AIC)")
    )
  }
  paste0("Selected models: ",paste(selected, collapse = ", "))
})

#########################################################
#### Plotting function  #################################
#########################################################
plot_survival_models <- function(
    preds,
    selected_model = NULL,
    horizon = NULL,
    validation_data = NULL,
    validation_time = NULL,
    validation_prob = NULL
){
  p <- plotly::plot_ly()
  for(m in unique(preds$model)){
    df <- preds[preds$model == m, ]
    width <- 2
    opacity <- 0.8
    if(!is.null(selected_model) && m == selected_model){
      width <- 5
      opacity <- 1
    }
    if(m == "Kaplan-Meier"){
      p <- p %>%
        plotly::add_lines(
          data = df,
          x = ~time,
          y = ~surv,
          name = m,
          line = list(
            color = "black",
            dash = "dash",
            width = 3
          ),
          opacity = 1
        )
    } else {
      p <- p %>%
        plotly::add_lines(
          data = df,
          x = ~time,
          y = ~surv,
          name = m,
          line = list(
            color = survival_colours[[m]] %||% "#000000",
            width = width
          ),
          opacity = opacity
        )
    }
  }
  #################################################
  #### Uploaded Long-Term Validation Curve ########
  #################################################
  if(!is.null(validation_data) &&
     nrow(validation_data) > 0){
    p <- p %>%
      plotly::add_lines(
        data = validation_data,
        x = ~time,
        y = ~surv,
        name = "Long-Term Validation",
        line = list(
          color = "purple",
          width = 3,
          dash = "dot"
        )
      )
  }
  #################################################
  #### Manual Validation Point ####################
  #################################################
  if(!is.null(validation_time) &&
     !is.null(validation_prob)){
    p <- p %>%
      plotly::add_markers(
        x = validation_time,
        y = validation_prob,
        name = "Validation Point",
        marker = list(
          color = "red",
          size = 12,
          symbol = "x"
        )
      )
  }
  p %>%
    plotly::layout(
      xaxis = list(
        title = "Time (Months)",
        range = c(0, horizon)
      ),
      yaxis = list(
        title = "Survival Probability",
        range = c(0,1)
      )
    )
}

#########################################################
#### Long-Term OS Validation Input ######################
#########################################################
longterm_os_validation <- reactive({
  if(is.null(input$int_longterm_os_file)){
    return(NULL)
  }
  df <- read.csv(
    input$int_longterm_os_file$datapath
  )
  names(df) <- tolower(names(df))
  df
})

#########################################################
#### Long-Term OS Validation Download ###################
#########################################################
output$download_longterm_os_example <- downloadHandler(
  filename = function() {
    paste0("LongTerm_OS_Validation_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(longterm_os_validation())
    write.csv(longterm_os_validation(),file,row.names = FALSE)
  }
)

output$int_os_fit_plot <- plotly::renderPlotly({
  req(int_os_fit_table())
  plot_survival_models(
    preds = visible_int_os_survival(),
    horizon = input$time_horizon,
    validation_data = longterm_os_validation(),
    validation_time = input$int_longterm_os_time,
    validation_prob = input$int_longterm_os_prob
  )
})

#########################################################
#### Extrapolation download  ############################
#########################################################
output$download_int_os_extrapolation <- downloadHandler(
  filename = function(){
    paste0("OS_extrapolation_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(int_os_all_survival(), file,row.names = FALSE)
  }
)

#########################################################
#### Comparator curve for HR only #######################
#########################################################
comp_os_hr_reference_curve <- reactive({
  validate(
    need(
      input$comp_os_input_type == "Survival Data",
      "Hazard Ratio requires Comparator OS to be based on Survival Data."
    )
  )
  selected_models <- selected_comp_os_models()
  if(length(selected_models) == 0){
    model_name <- comp_os_fit_table()$Model[1]
  } else {
    model_name <- selected_models[1]
  }
  get_model_survival_curve(
    all_survival = comp_os_all_survival(),
    selected_model = model_name
  )
})

#########################################################
#### HR OS Curve ########################################
#########################################################
int_hr_os_curve <- reactive({
  req(
    input$int_os_input_type == "Hazard Ratio"
  )
  req(
    input$int_hr_os
  )
  comp_curve <- comp_os_hr_reference_curve()
  hr_curve <- apply_hr_to_survival(
    surv_df = comp_curve,
    hr = input$int_hr_os
  )
  
  cat("\nHR CURVE MONTH 12\n")
  print(
    hr_curve[hr_curve$time == 12, ]
  )
  
  cat("\nOS MONTH 12\n")
  print(
    int_os_curve()[int_os_curve()$time == 12, ]
  )
  
  hr_curve$model <- paste0(
    "HR = ",
    input$int_hr_os
  )
  hr_curve
})

#########################################################
#### Selected Intervention OS Curve #####################
#########################################################
selected_int_os_curve <- reactive({
  req(input$int_os_input_type)
  #######################################################
  #### SURVIVAL DATA ####################################
  #######################################################
  if(input$int_os_input_type == "Survival Data"){
    selected_models <- selected_int_os_models()
    if(length(selected_models) == 0){
      model_name <- int_os_fit_table()$Model[1]
    } else {
      model_name <- selected_models[1]
    }
    return(
      get_model_survival_curve(
        all_survival = int_os_all_survival(),
        selected_model = model_name
      )
    )
  }
  #######################################################
  #### HAZARD RATIO #####################################
  #######################################################
  if(input$int_os_input_type == "Hazard Ratio"){
    return(
      int_hr_os_curve()
    )
  }
})

#########################################################
#### Hazard ratio plot ##################################
#########################################################
output$int_hr_os_plot <- plotly::renderPlotly({
  req(
    input$int_os_input_type == "Hazard Ratio"
  )
  comp_curve <- comp_os_hr_reference_curve()
  int_curve <- int_hr_os_curve()
  active_model <- unique(comp_curve$model %||% "Comparator")
  p <- plotly::plot_ly()
  #######################################################
  #### Comparator #######################################
  #######################################################
  p <- p %>%
    plotly::add_lines(
      data = comp_curve,
      x = ~time,
      y = ~surv,
      name = paste0(
        "Comparator (",
        active_model,
        ")"
      ),
      line = list(width = 3)
    )
  #######################################################
  #### Intervention #####################################
  #######################################################
  p <- p %>%
    plotly::add_lines(
      data = int_curve,
      x = ~time,
      y = ~surv,
      name = paste0(
        "Intervention (HR=",
        input$int_hr_os,
        ")"      ),
      line = list(width = 3)
    )
  #######################################################
  #### Layout ###########################################
  #######################################################
  p %>%
    plotly::layout(
      title = "OS Hazard Ratio Projection",
      xaxis = list(
        title = "Time (Months)"
      ),
      yaxis = list(
        title = "Survival Probability",
        range = c(0,1)
      )
    )
})

#########################################################
#### PFS Survival IPD data ##############################
#########################################################
output$download_pfs_ipd_example <- downloadHandler(
  filename = function() {
    paste0("PFS_IPD_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(
      input$int_pfs_input_type == "Survival Data",
      input$int_pfs_survival_type == "IPD"
    )
    write.csv(int_pfs_ipd(),file,row.names = FALSE)
  }
)

#########################################################
#### PFS IPD Input ######################################
#########################################################
int_pfs_ipd <- reactive({
  req(input$int_pfs_input_type == "Survival Data")
  req(input$int_pfs_survival_type)
  if(input$int_pfs_survival_type == "IPD"){
    req(input$int_pfs_ipd)
    read_ipd_data(input$int_pfs_ipd)
  } else {
    req(input$int_pfs_km_survival)
    req(input$int_pfs_km_risk)
    km_data <- read.csv(input$int_pfs_km_survival$datapath)
    risk_data <- read.csv(input$int_pfs_km_risk$datapath)
    reconstruct_ipd(
      n_at_risk = risk_data,
      p_survival = km_data
    )
  }
})

#########################################################
#### PFS IPD Diagnostics ################################
#########################################################
observe({
  req(
    input$int_pfs_input_type == "Survival Data",
    input$int_pfs_survival_type == "IPD"
  )
  ipd <- int_pfs_ipd()
  cat("\n=========================\n")
  cat("PFS IPD UPLOADED\n")
  cat("=========================\n")
  cat("Rows:", nrow(ipd), "\n")
  cat(
    "Events:",
    sum(ipd$status == 1),
    "\n"
  )
  cat(
    "Censored:",
    sum(ipd$status == 0),
    "\n"
  )
  cat("\nColumns:\n")
  print(names(ipd))
  cat("\nFirst 10 rows:\n")
  print(head(ipd,10))
})

#########################################################
#### PFS Reconstructed IPD Download #####################
#########################################################
output$download_pfs_reconstructed_ipd <- downloadHandler(
  filename = function(){
    paste0("reconstructed_PFS_IPD_", Sys.Date(), ".csv")
  },
  content = function(file){
    req(int_pfs_ipd())
    write.csv(int_pfs_ipd(), file, row.names = FALSE)
  }
)

#########################################################
#### PFS KM Curve downloads #############################
#########################################################
output$download_pfs_prob_survival <- downloadHandler(
  filename = function() {
    paste0("PFS_Probability_Survival_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(input$int_pfs_km_survival)
    km_data <- read.csv(
      input$int_pfs_km_survival$datapath
    )
    write.csv(km_data, file, row.names = FALSE)
  }
)
output$download_pfs_natrisk <- downloadHandler(
  filename = function() {
    paste0("PFS_Number_At_Risk_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(input$int_pfs_km_risk)
    risk_data <- read.csv(input$int_pfs_km_risk$datapath)
    write.csv(risk_data, file, row.names = FALSE)
  }
)

#########################################################
#### PFS MODELS #########################################
#########################################################
int_pfs_models <- reactive({
  req(int_pfs_ipd())
  ipd <- int_pfs_ipd()
  cat("\n=========================\n")
  cat("PFS IPD SUMMARY\n")
  cat("=========================\n")
  cat("Rows:", nrow(ipd), "\n")
  cat("Events:", sum(ipd$status == 1), "\n")
  cat("Censored:", sum(ipd$status == 0), "\n")
  fit_parametric_models(ipd)
})

#########################################################
#### FIT TABLE ##########################################
#########################################################
int_pfs_fit_table <- reactive({
  req(int_pfs_models())
  create_fit_table(int_pfs_models())
})

output$int_pfs_aic_table <- DT::renderDT({
  req(input$int_pfs_input_type == "Survival Data")
  DT::datatable(
    int_pfs_fit_table(),
    rownames = FALSE,
    selection = "multiple",
    options = list(pageLength = 10, dom = "t")
  )
})

#########################################################
#### MODEL SELECTION ####################################
#########################################################
selected_int_pfs_models <- reactive({
  req(int_pfs_fit_table())
  rows <- input$int_pfs_aic_table_rows_selected
  if (is.null(rows) || length(rows) == 0) return(character(0))
  int_pfs_fit_table()$Model[rows]
})

get_primary_pfs_model <- function() {
  selected <- selected_int_pfs_models()
  if (length(selected) > 0) return(selected[1])
  int_pfs_fit_table()$Model[1]
}

#########################################################
#### SURVIVAL COMPONENTS ################################
#########################################################
int_pfs_predictions <- reactive({
  req(int_pfs_models(), input$time_horizon)
  generate_survival_predictions(
    int_pfs_models(),
    input$time_horizon
  )
})

int_pfs_all_survival <- reactive({
  req(int_pfs_ipd(), int_pfs_models(), input$time_horizon)
  km <- create_km_from_ipd(int_pfs_ipd())
  preds <- int_pfs_predictions()
  dplyr::bind_rows(km, preds)
})

visible_int_pfs_survival <- reactive({
  surv <- int_pfs_all_survival()
  selected <- selected_int_pfs_models()
  if (length(selected) == 0) {
    return(surv[surv$model == "Kaplan-Meier", ])
  }
  surv[surv$model %in% c("Kaplan-Meier", selected), ]
})

#########################################################
#### COMP MODEL #########################################
#########################################################
get_comp_pfs_model <- function() {
  selected <- selected_comp_pfs_models()
  if (length(selected) > 0) return(selected[1])
  comp_pfs_fit_table()$Model[1]
}

# get_comp_pfs_curve <- reactive({
#   req(input$comp_pfs_input_type == "Survival Data")
#   model_name <- get_comp_pfs_model()
#   df <- get_model_survival_curve(
#     all_survival = comp_pfs_all_survival(),
#     selected_model = model_name
#   )
#   normalize_survival_curve(df)
# })

#########################################################
#### Median PFS MODEL ###################################
#########################################################
get_active_comp_pfs_curve <- reactive({
  cat("\n===== ENTERING get_active_comp_pfs_curve =====\n")
  if (input$comp_pfs_input_type == "Median PFS") {
    cat("Using Median PFS comparator\n")
    obj <- comp_pfs_object()
    req(obj)
    return(
      get_active_curve(
        survival_object = obj
      )
    )
  }
  if (input$comp_pfs_input_type == "Survival Data") {
    cat("Using Survival Data comparator\n")
    return(
      get_comp_pfs_curve()
    )
  }
})

#########################################################
#### Survival PFS MODEL #################################
#########################################################
get_comp_pfs_curve <- reactive({
  cat("\n===== ENTERING get_comp_pfs_curve =====\n")
  cat("comp input type:\n")
  print(input$comp_pfs_input_type)
  req(input$comp_pfs_input_type == "Survival Data")
  cat("PASSED INPUT TYPE CHECK\n")
  cat("Getting model...\n")
  model_name <- get_comp_pfs_model()
  print(model_name)
  cat("Getting all survival...\n")
  surv <- comp_pfs_all_survival()
  cat("Rows:\n")
  print(nrow(surv))
  df <- get_model_survival_curve(
    all_survival = surv,
    selected_model = model_name
  )
  cat("Curve rows:\n")
  print(nrow(df))
  normalize_survival_curve(df)
})



#########################################################
#### INTERVENTION MODEL LOGIC (UNIFIED CORE) ###########
#########################################################
get_int_pfs_model <- function() {
  # Median case
  if (input$int_pfs_input_type == "Median PFS") {
    return(list(
      type = "median",
      model = "Median PFS"
    ))
  }
  # Survival / HR / Parametric case
  selected <- selected_int_pfs_models()
  model_name <- if (length(selected) > 0) {
    selected[1]
  } else {
    get_primary_pfs_model()
  }
  list(
    type = "parametric",
    model = model_name
  )
}



#########################################################
#### FINAL CURVE WRAPPER ###############################
#########################################################
selected_int_pfs_curve <- reactive({
  req(input$int_pfs_input_type)
  get_int_pfs_curve()
})

#########################################################
#### PFS ACTIVE MODEL TEXT ##############################
#########################################################
output$int_pfs_active_model <- renderText({
  req(input$int_pfs_input_type)
  if (input$int_pfs_input_type == "Median PFS") {
    return("Active model: Median PFS")
  }
  selected <- selected_int_pfs_models()
  if (length(selected) == 0) {
    return(paste0(
      "Active model: ",
      get_primary_pfs_model(),
      " (Lowest AIC)"
    ))
  }
  paste0("Selected models: ", paste(selected, collapse = ", "))
})


#########################################################
#### PFS PLOT ###########################################
#########################################################
output$int_pfs_fit_plot <- plotly::renderPlotly({
  req(int_pfs_fit_table())
  plot_survival_models(
    preds = visible_int_pfs_survival(),
    horizon = input$time_horizon,
    validation_data = longterm_pfs_validation(),
    validation_time = input$int_longterm_pfs_time,
    validation_prob = input$int_longterm_pfs_prob
  )
})





int_pfs_curve <- reactive({
  cat("\n====================\n")
  cat("ENTERING int_pfs_curve\n")
  cat("====================\n")
  cat("input$int_pfs_input_type = ")
  print(input$int_pfs_input_type)
  cat("input$int_hr_pfs = ")
  print(input$int_hr_pfs)
  if (input$int_pfs_input_type == "Hazard Ratio") {
    cat("\n===== HR TRANSFORM BLOCK ENTERED =====\n")
    #  req(input$comp_pfs_input_type == "Survival Data")
    
    cat("Comparator type:\n")
    print(input$comp_pfs_input_type)
    validate(
      need(
        input$comp_pfs_input_type %in% c(
          "Survival Data",
          "Median PFS"
        ),
        paste(
          "Comparator type is",
          input$comp_pfs_input_type
        )
      )
    )
    
    #  comp_curve <- isolate(get_comp_pfs_curve())
    cat("Before get_comp_pfs_curve()\n")
    comp_curve <- tryCatch(
      get_active_comp_pfs_curve(),
      error = function(e) {
        cat("ERROR INSIDE get_comp_pfs_curve()\n")
        print(e)
        return(NULL)
      }
    )
    cat("After get_comp_pfs_curve()\n")
    print(class(comp_curve))
    # if (!is.null(comp_curve)) {
    #   print(head(comp_curve))
    # }
    
    req(!is.null(comp_curve))
    req(nrow(comp_curve) > 0)
    hr_curve <- apply_hr_to_survival(
      surv_df = comp_curve[, c("time","surv")],
      hr = as.numeric(input$int_hr_pfs)
    )
    hr_curve$model <- paste0("HR=", input$int_hr_pfs)
    return(hr_curve)
  }
  obj <- int_pfs_object()
  if (obj$type == "median") {
    return(
      get_active_curve(
        survival_object = obj$curve
      )
    )
  }
  curve <- get_active_curve(
    survival_object = obj,
    selected_model = int_pfs_selected_model()
  )
  curve
})

observe({
  cat("\n====================\n")
  cat("INT PFS CURVE TEST\n")
  cat("====================\n")
  x <- tryCatch(
    int_pfs_curve(),
    error = function(e) {
      cat("\nERROR CLASS:\n")
      print(class(e))
      cat("\nERROR MESSAGE:\n")
      print(conditionMessage(e))
      return(e)
    }
  )
  print(x)
})


#########################################################
#### HR PFS PLOT ########################################
#########################################################
output$int_hr_pfs_plot <- plotly::renderPlotly({
  cat("\n================ HR PFS PLOT CALLED ================\n")
  cat("input$int_pfs_input_type:", input$int_pfs_input_type, "\n")
  cat("HR value:", input$int_hr_pfs, "\n")
  
  cat("comp input type:", input$comp_pfs_input_type, "\n")
  req(input$int_pfs_input_type == "Hazard Ratio")
  comp_curve <- isolate(get_active_comp_pfs_curve())
  cat("\n--- COMP CURVE DEBUG ---\n")
  print(str(comp_curve))
  cat("\n===== COMP CURVE SANITY CHECK =====\n")
  
  cat("N rows:", nrow(comp_curve), "\n")
  cat("NA surv:", sum(is.na(comp_curve$surv)), "\n")
  cat("Min surv:", min(comp_curve$surv, na.rm = TRUE), "\n")
  cat("Max surv:", max(comp_curve$surv, na.rm = TRUE), "\n")
  cat("Rows:", nrow(comp_curve), "\n")
  cat("Columns:", paste(names(comp_curve), collapse = ", "), "\n")
  cat("Model:", unique(comp_curve$model), "\n")
  cat("\n--- INT CURVE DEBUG (RAW FUNCTION CALL) ---\n")
  tmp <- tryCatch(
    isolate(int_pfs_curve()),
    error = function(e) e
  )
  print(tmp)
  if (inherits(tmp, "try-error")) {
    cat("ERROR in int_pfs_curve()\n")
  }
  # int_curve  <- isolate(int_pfs_curve())
  
  int_curve  <- int_pfs_curve()
  
  str(int_curve)
  cat("Rows:", nrow(int_curve), "\n")
  active_model <- unique(comp_curve$model %||% "Comparator")
  plot_ly() %>%
    add_lines(
      data = comp_curve,
      x = ~time,
      y = ~surv,
      name = paste0("Comparator (", active_model, ")"),
      line = list(width = 3)
    ) %>%
    add_lines(
      data = int_curve,
      x = ~time,
      y = ~surv,
      name = paste0("Intervention (HR=", input$int_hr_pfs, ")"),
      line = list(width = 3)
    ) %>%
    layout(
      title = "PFS Hazard Ratio Projection",
      xaxis = list(title = "Time (Months)"),
      yaxis = list(title = "Survival Probability", range = c(0, 1))
    )
})


#########################################################
#### PFS Extrapolation download #########################
#########################################################
output$download_int_pfs_extrapolation <- downloadHandler(
  filename = function(){
    paste0("PFS_extrapolation_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(int_pfs_all_survival(), file, row.names = FALSE)
  }
)

#########################################################
#### Long-Term PFS Validation Input #####################
#########################################################
longterm_pfs_validation <- reactive({
  if(is.null(input$int_longterm_pfs_file)){
    return(NULL)
  }
  df <- read.csv(
    input$int_longterm_pfs_file$datapath
  )
  names(df) <- tolower(names(df))
  df
})

#########################################################
#### Long-Term PFS Validation Download ##################
#########################################################
output$download_longterm_pfs_example <- downloadHandler(
  filename = function() {
    paste0("LongTerm_PFS_Validation_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(longterm_pfs_validation())
    write.csv(longterm_pfs_validation(),file,row.names = FALSE)
  }
)

###########################################################################

#########################################################
#### OS Survival Object #################################
#########################################################
int_os_object <- reactive({
  if(input$int_os_input_type == "Median OS"){
    build_survival_object(
      input_type = "Median",
      median_value = input$int_median_os,
      horizon = input$time_horizon,
      curve_name = "Median OS"
    )
  } else {
    build_survival_object(
      input_type = "Survival",
      ipd = int_os_ipd(),
      horizon = input$time_horizon
    )
  }
})

#########################################################
#### PFS Survival Object ################################
#########################################################
int_pfs_object <- reactive({
  if (input$int_pfs_input_type == "Median PFS") {
    req(input$int_median_pfs)
    validate(
      need(input$int_median_pfs > 0, "Median PFS must be > 0")
    )
    return(list(
      type = "median",
      curve = build_survival_object(
        input_type = "Median",
        median_value = input$int_median_pfs,
        horizon = input$time_horizon,
        curve_name = "Median PFS"
      )
    ))
  }
  # Survival case
  req(int_pfs_ipd())
  return(list(
    type = "survival",
    curve = build_survival_object(
      input_type = "Survival",
      ipd = int_pfs_ipd(),
      horizon = input$time_horizon
    )
  ))
})

observe({
  req(input$int_median_pfs)
  cat("\n=====================\n")
  cat("MEDIAN PFS INPUT\n")
  cat("=====================\n")
  cat("Median =", input$int_median_pfs, "\n")
  cat("Horizon =", input$time_horizon, "\n")
})

#########################################################
#### Add Debugging ######################################
#########################################################
observe({
  req(int_os_object())
  cat("\n=====================\n")
  cat("INTERVENTION OS OBJECT\n")
  cat("=====================\n")
  print(int_os_object()$type)
})

observe({
  req(int_pfs_object())
  cat("\n=====================\n")
  cat("INTERVENTION PFS OBJECT\n")
  cat("=====================\n")
  print(int_pfs_object()$type)
})

#########################################################
#### Print Package Summary ##############################
#########################################################
observe({
  req(int_os_object())
  print_survival_summary(int_os_object())
})

#########################################################
#### Selected OS Model ##################################
#########################################################
int_os_selected_model <- reactive({
  req(int_os_object())
  if(int_os_object()$type == "median"){
    return(NULL)
  }
  select_active_model(
    fit_table = int_os_object()$package$fit_table,
    selected_rows = input$int_os_aic_table_rows_selected
  )
})

#########################################################
#### Selected PFS Model #################################
#########################################################
int_pfs_selected_model <- reactive({
  req(int_pfs_object())
  if(int_pfs_object()$type == "median"){
    return(NULL)
  }
  select_active_model(
    fit_table = int_pfs_object()$package$fit_table,
    selected_rows = input$int_pfs_aic_table_rows_selected
  )
})

#########################################################
#### Selected OS Debug ##################################
#########################################################
observe({
  req(int_os_selected_model())
  cat("\n====================\n")
  cat("INTERVENTION SELECTED OS MODEL\n")
  cat("====================\n")
  print(int_os_selected_model())
})

#########################################################
#### Selected PFS Debug #################################
#########################################################
observe({
  req(int_pfs_selected_model())
  cat("\n====================\n")
  cat("INTERVENTION SELECTED PFS MODEL\n")
  cat("====================\n")
  print(int_pfs_selected_model())
})

#########################################################
#### Active OS Curve ####################################
#########################################################
int_os_curve <- reactive({
  req(int_os_object())
  if(int_os_object()$type == "median"){
    return(int_os_object()$curve)
  }
  get_active_curve(
    survival_object = int_os_object(),
    selected_model = int_os_selected_model()
  )
})

#########################################################
#### Active PFS Curve ###################################
#########################################################
# int_pfs_curve <- reactive({
#   req(int_pfs_object())
#   
#   if(int_pfs_object()$type == "median"){
#     return(
#       int_pfs_object()$curve$curve
#     )
#   }
#   cat("\n[PFS DEBUG]\n")
#  # cat("type:", obj$type, "\n")
#   cat("selected model:", int_pfs_selected_model(), "\n")
# #  cat("curve NULL?", is.null(curve), "\n")
#   if (!is.null(curve)) print(head(curve))
#   get_active_curve(
#     survival_object = int_pfs_object(),
#     selected_model = int_pfs_selected_model()
#   )
# })

observe({
  req(int_os_curve())
  cat("\nINTERVENTION OS CURVE MODEL\n")
  print(unique(int_os_curve()$model))
})

observe({
  req(int_pfs_curve())
  cat("\nINTERVENTION PFS CURVE MODEL\n")
  print(unique(int_pfs_curve()$model))
})


#########################################################
#### Intervention Occupancy #############################
#########################################################
int_occupancy_object <- reactive({
  cat("\n=====================\n")
  cat("BUILDING INTERVENTION OCCUPANCY\n")
  cat("=====================\n")
  os <- tryCatch(
    int_os_curve(),
    error = function(e) e
  )
  pfs <- tryCatch(
    int_pfs_curve(),
    error = function(e) e
  )
  cat("\nOS OBJECT:\n")
  print(class(os))
  cat("\nPFS OBJECT:\n")
  print(class(pfs))
  if (inherits(os, "error")) {
    cat("\nOS ERROR:\n")
    print(os$message)
    return(NULL)
  }
  if (inherits(pfs, "error")) {
    cat("\nPFS ERROR:\n")
    print(pfs$message)
    return(NULL)
  }
  cat("\nOS rows:\n")
  print(nrow(os))
  cat("\nPFS rows:\n")
  print(nrow(pfs))
  build_occupancy_object(os, pfs)
})

observe({
  req(int_occupancy_object())
  cat("\n=====================\n")
  cat("INTERVENTION OCCUPANCY\n")
  cat("=====================\n")
  print(head(int_occupancy_object()$states))
})

observe({
  req(int_os_curve())
  req(int_pfs_curve())
  cat("\n=====================\n")
  cat("OCCUPANCY INPUTS\n")
  cat("=====================\n")
  cat("OS model:\n")
  print(unique(int_os_curve()$model))
  cat("OS month 12:\n")
  print(
    int_os_curve()[
      int_os_curve()$time == 12,
    ]
  )
  cat("PFS model:\n")
  print(unique(int_pfs_curve()$model))
  cat("PFS month 12:\n")
  print(
    int_pfs_curve()[
      int_pfs_curve()$time == 12,
    ]
  )
})

observe({
  req(int_occupancy_object())
  write.csv(int_occupancy_object()$states,"intervention_occupancy_debug.csv",row.names = FALSE)
  cat("\nINTERVENTION CSV WRITTEN\n")
})


