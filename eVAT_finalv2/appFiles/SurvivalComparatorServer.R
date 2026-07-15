#========================================================
# SurvivalComparatorServer.R
#========================================================
#########################################################
#### Guard: auto-correct blank/invalid manual entries ###
#########################################################
guardNumericInput(input, session, "starting_age", default = 40, min = 0, max = 150, label = "Starting Age")
guardNumericInput(input, session, "male_proportion", default = 1, min = 0, max = 1, label = "Male Proportion")
guardNumericInput(input, session, "comp_median_os", default = 30, min = 0, label = "Comparator Median OS (Months)")
guardNumericInput(input, session, "comp_median_pfs", default = 18, min = 0, label = "Comparator Median PFS (Months)")
guardNumericInput(input, session, "comp_longterm_os_prob", default = 0.05, min = 0, max = 1, label = "Comparator OS Survival Probability")
guardNumericInput(input, session, "comp_longterm_os_time", default = 60, min = 0, label = "Comparator OS Long-Term Time (Months)")
guardNumericInput(input, session, "comp_longterm_pfs_prob", default = 0.02, min = 0, max = 1, label = "Comparator PFS Survival Probability")
guardNumericInput(input, session, "comp_longterm_pfs_time", default = 60, min = 0, label = "Comparator PFS Long-Term Time (Months)")
guardNumericInput(input, session, "time_horizon", default = 240, min = 1, label = "Time Horizon (Months)")

#########################################################
#### Median function codes ##############################
#########################################################
model_times <- reactive({
  req(input$time_horizon)
  validate(
    need(input$time_horizon > 0, "Time Horizon must be > 0")
  )
  seq(
    from = 0,
    to   = input$time_horizon,
    by   = 1
  )
})
#########################################################
#### RESET TO DEFAULT (COMPARATOR SURVIVAL DATA) ########
#########################################################
# shinyjs::reset() only clears the widget cosmetically; the
# Shiny fileInput binding does not reliably emit a NULL value
# back to the server when reset that way, so the previously
# uploaded file's data can persist. Instead we track an
# explicit "use default" flag per dataset. The wrapper
# reactives below (e.g. comp_os_ipd_file()) return NULL
# whenever that flag is set, which downstream reactives treat
# as "use the bundled default dataset". The flag clears
# automatically the moment the user uploads a genuinely new file.
comp_reset_flags <- reactiveValues(
  comp_os_ipd  = FALSE,
  comp_os_km   = FALSE,
  comp_pfs_ipd = FALSE,
  comp_pfs_km  = FALSE,
  comp_background_mortality = FALSE
)

comp_os_ipd_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_os_ipd)) return(NULL)
  input$comp_os_ipd
})
comp_os_km_survival_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_os_km)) return(NULL)
  input$comp_os_km_survival
})
comp_os_km_risk_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_os_km)) return(NULL)
  input$comp_os_km_risk
})
comp_pfs_ipd_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_pfs_ipd)) return(NULL)
  input$comp_pfs_ipd
})
comp_pfs_km_survival_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_pfs_km)) return(NULL)
  input$comp_pfs_km_survival
})
comp_pfs_km_risk_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_pfs_km)) return(NULL)
  input$comp_pfs_km_risk
})
comp_background_mortality_file <- reactive({
  if (isTRUE(comp_reset_flags$comp_background_mortality)) return(NULL)
  input$comp_background_mortality
})

observeEvent(input$comp_os_ipd, { comp_reset_flags$comp_os_ipd <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_os_km_survival, { comp_reset_flags$comp_os_km <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_os_km_risk, { comp_reset_flags$comp_os_km <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_pfs_ipd, { comp_reset_flags$comp_pfs_ipd <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_pfs_km_survival, { comp_reset_flags$comp_pfs_km <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_pfs_km_risk, { comp_reset_flags$comp_pfs_km <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)
observeEvent(input$comp_background_mortality, { comp_reset_flags$comp_background_mortality <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent(input$reset_comp_os_ipd, {
  comp_reset_flags$comp_os_ipd <- TRUE
  shinyjs::reset("comp_os_ipd")
  showNotification("Comparator OS IPD reset to default dataset.", type = "message")
})

observeEvent(input$reset_comp_os_km, {
  comp_reset_flags$comp_os_km <- TRUE
  shinyjs::reset("comp_os_km_survival")
  shinyjs::reset("comp_os_km_risk")
  showNotification("Comparator OS digitized KM data reset to default dataset.", type = "message")
})

observeEvent(input$reset_comp_pfs_ipd, {
  comp_reset_flags$comp_pfs_ipd <- TRUE
  shinyjs::reset("comp_pfs_ipd")
  showNotification("Comparator PFS IPD reset to default dataset.", type = "message")
})

observeEvent(input$reset_comp_background_mortality, {
  comp_reset_flags$comp_background_mortality <- TRUE
  shinyjs::reset("comp_background_mortality")
  showNotification("Background Mortality reset to default dataset.", type = "message")
})

observeEvent(input$reset_comp_pfs_km, {
  comp_reset_flags$comp_pfs_km <- TRUE
  shinyjs::reset("comp_pfs_km_survival")
  shinyjs::reset("comp_pfs_km_risk")
  showNotification("Comparator PFS digitized KM data reset to default dataset.", type = "message")
})

#########################################################
#### OS EXPONENTIAL CURVE ###############################
#########################################################
comp_median_os_curve <- reactive({
  req(input$comp_median_os)
  generate_exponential_survival(
    median_months = input$comp_median_os,
    model_times   = model_times(),
    model_name = "Median OS"
  )
})

output$download_comp_median_os_predictions <- downloadHandler(
  filename = function() {
    paste0("Comparator_Median_OS_Predictions_", Sys.Date(), ".csv")
  },
  content = function(file) {
    pred <- comp_median_os_curve()
    write.csv(pred, file, row.names = FALSE)
  }
)

#########################################################
#### PFS EXPONENTIAL CURVE ##############################
#########################################################
comp_median_pfs_curve <- reactive({
  req(input$comp_median_pfs)
  generate_exponential_survival(
    median_months = input$comp_median_pfs,
    model_times   = model_times(),
    model_name = "Median PFS"
  )
})

#########################################################
#### Download Median PFS Predictions ####################
#########################################################
output$download_comp_median_pfs_predictions <- downloadHandler(
  filename = function() {
    paste0("Comparator_Median_PFS_Predictions_", Sys.Date(), ".csv")
  },
  content = function(file) {
    pred <- comp_median_pfs_curve()
    write.csv(pred, file, row.names = FALSE)
  }
)

#########################################################
#### OPTIONAL KM DATA ###################################
#########################################################
km_os_data <- reactive({
  cat("\n===== ENTERING km_os_data() =====\n")
  km <- if(is.null(comp_os_km_survival_file())) {
    cat("Using default KM file\n")
    default_os_survival
  } else {
    cat("Using uploaded KM file\n")
    read.csv(
      comp_os_km_survival_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  cat("Raw dimensions:\n")
  print(dim(km))
  cat("Raw names:\n")
  print(names(km))
  names(km) <- c("Time","Surv")
  cat("Renamed successfully\n")
  km
})

output$comp_os_survival_source <- renderText({
  if(is.null(comp_os_km_survival_file())){
    "Currently using: Default OS Survival dataset"
  } else {
    paste(
      "Currently using uploaded file",
      comp_os_km_survival_file()$name
    )
  }
})

#########################################################
#### OPTIONAL RISK DATA #################################
#########################################################
risk_os_data <- reactive({
  cat("\n===== ENTERING risk_os_data() =====\n")
  risk <- if(is.null(comp_os_km_risk_file())) {
    default_os_risk
  } else {
    read.csv(
      comp_os_km_risk_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  print(dim(risk))
  print(names(risk))
  names(risk) <- c("Time","n_at_risk")
  risk
})

output$comp_os_risk_source <- renderText({
  if(is.null(comp_os_km_risk_file())){
    "Currently using: Default OS Risk dataset"
  } else {
    paste(
      "Currently using uploaded file",
      comp_os_km_risk_file()$name
    )
  }
})


#########################################################
#### OPTIONAL KM PFS DATA ###############################
#########################################################
km_pfs_data <- reactive({
  req(comp_pfs_km_survival_file())
  km <- read.csv(
    comp_pfs_km_survival_file()$datapath
  )
  validate(
    need(
      all(c("time", "survival") %in% names(km)),
      "CSV must contain columns: time and survival"
    )
  )
  km
})

output$download_comp_os_survival_example <- downloadHandler(
  filename = function() {
    "Example_OS_Survival.csv"
  },
  content = function(file) {
    write.csv(
      default_os_survival,
      file,
      row.names = FALSE
    )
  }
)

output$download_comp_os_risk_example <- downloadHandler(
  filename = function() {
    "Example_OS_Risk.csv"
  },
  content = function(file) {
    write.csv(
      default_os_risk,
      file,
      row.names = FALSE
    )
  }
)

# #########################################################
# #### OPTIONAL KM OS DATA ###############################
# #########################################################
# output$comp_os_survival_preview <- DT::renderDT({
#   DT::datatable(
#     head(km_os_data(), 5),
#     rownames = FALSE,
#     options = list(
#       pageLength = 5,
#       searching = FALSE,
#       lengthChange = FALSE,
#       dom = "t"
#     )
#   )
# })
# 
# output$comp_os_risk_preview <- DT::renderDT({
#   DT::datatable(
#     head(risk_os_data(), 5),
#     rownames = FALSE,
#     options = list(
#       pageLength = 5,
#       searching = FALSE,
#       lengthChange = FALSE,
#       dom = "t"
#     )
#   )
# })

#########################################################
#### OS PLOT ############################################
#########################################################
output$comp_median_os_plot <- plotly::renderPlotly({
  req(input$comp_os_input_type == "Median OS")
  data <- comp_median_os_curve()
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
      x = input$comp_median_os,
      xend = input$comp_median_os,
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
      xend = input$comp_median_os,
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
  if (!is.null(comp_os_km_survival_file())) {
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
        fixedrange = TRUE,
        tickmode = "array",
        tickvals = c(0, 0.25, 0.5, 0.75, 1),
        ticktext = c("0.00", "0.25", "0.50", "0.75", "1.00"),
        nticks = 5,
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
output$comp_median_pfs_plot <- plotly::renderPlotly({
  req(input$comp_pfs_input_type == "Median PFS")
  data <- comp_median_pfs_curve()
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
      x = input$comp_median_pfs,
      xend = input$comp_median_pfs,
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
      xend = input$comp_median_pfs,
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
  if (!is.null(comp_pfs_km_survival_file())) {
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
        fixedrange = TRUE,
        tickmode = "array",
        tickvals = c(0, 0.25, 0.5, 0.75, 1),
        ticktext = c("0.00", "0.25", "0.50", "0.75", "1.00"),
        nticks = 5,
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

output$download_comp_os_ipd_example <- downloadHandler(
  filename = function() {paste0("OS_IPD_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(
      input$comp_os_input_type == "Survival Data",
      input$comp_os_survival_type == "IPD"
    )
    write.csv(comp_os_ipd(),file,row.names = FALSE)
  }
)

observe({
  
  req(input$comp_os_survival_type == "Digitized KM Curve")
  
  ipd <- comp_os_ipd()
  
  cat("\n========================\n")
  cat("RECONSTRUCTED IPD\n")
  cat("========================\n")
  
  if(is.null(ipd)){
    cat("NULL RESULT\n")
  } else {
    cat("Rows:", nrow(ipd), "\n")
    print(head(ipd))
  }
  
})


#########################################################
#### OS IPD INPUT #######################################
#########################################################
# comp_os_ipd <- reactive({
#   cat("\n=================================\n")
#   cat("ENTERING comp_os_ipd()\n")
#   cat("=================================\n")
#   
#   req(input$comp_os_input_type == "Survival Data")
#   req(input$comp_os_survival_type)
#   cat("survival type:",input$comp_os_survival_type, "\n")
#   #######################################################
#   #### IPD ROUTE ########################################
#   #######################################################
#   if (input$comp_os_survival_type == "IPD") {
#     if (is.null(input$comp_os_ipd)) {
#       cat("ROUTE = IPD\n")
#       ipd <- default_os_ipd
#     } else {
#       cat("ROUTE = IPD\n")
#       ipd <- read_ipd_data(
#         input$comp_os_ipd
#       )
#     }
#     return(ipd)
#   }
#   #######################################################
#   #### KM + N AT RISK ROUTE #############################
#   #######################################################
#   # km_data <- if (is.null(input$comp_os_km_survival)) {
#   #   default_os_survival
#   # } else {
#   #   read.csv(
#   #     input$comp_os_km_survival$datapath
#   #   )
#   # }
#   # risk_data <- if (is.null(input$comp_os_km_risk)) {
#   #   default_os_risk
#   # } else {
#   #   read.csv(
#   #     input$comp_os_km_risk$datapath
#   #   )
#   # }
#   cat("\nKM DATA\n")
#   print(dim(km_os_data()))
#   print(names(km_os_data()))
#   print(head(km_os_data()))
#   
#   cat("\nRISK DATA\n")
#   print(dim(risk_os_data()))
#   print(names(risk_os_data()))
#   print(head(risk_os_data()))
#   #######################################################
#   #### KM + N AT RISK ROUTE #############################
#   #######################################################
#   result <- tryCatch({
#     # 
#     reconstruct_ipd(
#       n_at_risk = risk_os_data(),
#       p_survival = km_os_data()
#     )
#     
#   }, error = function(e){
#     
#     cat("\nRECONSTRUCTION FAILED\n")
#     cat(e$message, "\n")
#     
#     NULL
#   })
#   
#   cat("\nRECONSTRUCTION RESULT\n")
#   print(class(result))
#   
#   if(!is.null(result)){
#     cat("Rows:", nrow(result), "\n")
#     print(head(result))
#   }
#   
#   result
# })

#########################################################
#### OS IPD INPUT #######################################
#########################################################
comp_os_ipd <- reactive({
  cat("\n=================================\n")
  cat("ENTERING comp_os_ipd()\n")
  cat("=================================\n")
  req(input$comp_os_input_type == "Survival Data")
  req(input$comp_os_survival_type)
  cat(
    "survival type:",
    input$comp_os_survival_type,
    "\n"
  )
  #######################################################
  #### IPD ROUTE ########################################
  #######################################################
  if (input$comp_os_survival_type == "IPD") {
    cat("ROUTE = IPD\n")
    if (is.null(comp_os_ipd_file())) {
      ipd <- default_os_ipd
    } else {
      ipd <- read_ipd_data(
        comp_os_ipd_file()
      )
    }
    cat("\nIPD SUMMARY\n")
    cat("Rows:", nrow(ipd), "\n")
    cat("Events:", sum(ipd$status == 1), "\n")
    cat("Censored:", sum(ipd$status == 0), "\n")
    return(ipd)
  }
  #######################################################
  #### DIGITIZED KM CURVE ROUTE #########################
  #######################################################
  cat("ROUTE = DIGITIZED KM CURVE\n")
  km_data <- if (is.null(comp_os_km_survival_file())) {
    cat("Using default KM survival file\n")
    default_os_survival
  } else {
    cat("Using uploaded KM survival file\n")
    read.csv(
      comp_os_km_survival_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  risk_data <- if (is.null(comp_os_km_risk_file())) {
    cat("Using default risk table\n")
    default_os_risk
  } else {
    cat("Using uploaded risk table\n")
    read.csv(
      comp_os_km_risk_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  #######################################################
  #### DEBUG KM DATA ####################################
  #######################################################
  cat("\nKM DATA\n")
  print(dim(km_data))
  print(names(km_data))
  print(head(km_data))
  cat("\nRISK DATA\n")
  print(dim(risk_data))
  print(names(risk_data))
  print(head(risk_data))
  #######################################################
  #### RECONSTRUCT IPD ##################################
  #######################################################
  result <- tryCatch({
    reconstruct_ipd(
      n_at_risk = risk_data,
      p_survival = km_data
    )
  }, error = function(e){
    cat("\nRECONSTRUCTION FAILED\n")
    cat(e$message, "\n")
    NULL
  })
  #######################################################
  #### DEBUG RECONSTRUCTION #############################
  #######################################################
  cat("\n========================\n")
  cat("RECONSTRUCTION RESULT\n")
  cat("========================\n")
  if (is.null(result)) {
    cat("NULL RESULT\n")
  } else {
    cat("Rows:", nrow(result), "\n")
    if ("status" %in% names(result)) {
      cat(
        "Events:",
        sum(result$status == 1),
        "\n"
      )
      cat(
        "Censored:",
        sum(result$status == 0),
        "\n"
      )
    }
    cat("\nColumns:\n")
    print(names(result))
    cat("\nFirst 10 rows:\n")
    print(head(result, 10))
  }
  result
})

output$comp_os_ipd_source <- renderText({
  if(is.null(comp_os_ipd_file())){
    "Currently using: Default OS IPD dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      comp_os_ipd_file()$name
    )
  }
})

output$comp_os_ipd_preview <- DT::renderDT({
  ipd <- comp_os_ipd()
  DT::datatable(
    head(ipd, 5),
    rownames = FALSE,
    options = list(
      pageLength = 5,
      searching = FALSE,
      lengthChange = FALSE,
      dom = "t"
    )
  )
})

#########################################################
#### OS IPD Downloads ###################################
#########################################################
observe({
  req(
    input$comp_os_input_type == "Survival Data",
    input$comp_os_survival_type == "IPD"
  )
  ipd <- comp_os_ipd()
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

output$download_comp_os_reconstructed_ipd <- downloadHandler(
  filename = function() {
    paste0("reconstructed_OS_IPD_", Sys.Date(), ".csv")
  },
  # content = function(file) {
  #   req(comp_os_ipd())
  #   write.csv(comp_os_ipd(), file, row.names = FALSE)
  # }
  content = function(file) {
    ipd <- comp_os_ipd()
    cat("\nDOWNLOAD BUTTON CLICKED\n")
    if(is.null(ipd)){
      cat("IPD IS NULL\n")
      stop("IPD reconstruction failed")
    }
    cat("ROWS:", nrow(ipd), "\n")
    write.csv(
      ipd,
      file,
      row.names = FALSE
    )
  }
)

#########################################################
#### Digitized KM curve codes ###########################
#########################################################

#########################################################
#### OS Parametric Models ###############################
#########################################################
# comp_os_models <- reactive({
#   req(comp_os_ipd())
#   ipd <- comp_os_ipd()
#   cat("\n=========================\n")
#   cat("OS IPD SUMMARY\n")
#   cat("=========================\n")
#   cat("Rows:", nrow(ipd), "\n")
#   cat("Events:", sum(ipd$status == 1), "\n")
#   cat("Censored:", sum(ipd$status == 0), "\n")
#   print(head(ipd))
#   fit_parametric_models(ipd)
# })

comp_os_models <- reactive({
  req(comp_os_ipd())
  ipd <- comp_os_ipd()
  models <- fit_parametric_models(ipd)
  cat("\n=========================\n")
  cat("PARAMETERS\n")
  cat("=========================\n")
  for(m in names(models)){
    cat("\nMODEL:", m, "\n")
    print(models[[m]]$res)
  }
  models
})

#########################################################
#### Selected OS Parameters #############################
#########################################################
comp_os_parameters <- reactive({
  req(comp_os_object())
  req(
    comp_os_object()$type == "survival"
  )
  selected_model <- comp_os_selected_model()
  req(selected_model)
  fit <- comp_os_object()$package$models[[selected_model]]
  extract_selected_model_parameters(
    fit = fit,
    model_name = selected_model
  )
})

observe({
  req(comp_os_object())
  req(comp_os_object()$type == "survival")
  wb <- openxlsx::createWorkbook()
  models <- comp_os_models()
  for(model_name in names(models)){
    fit <- models[[model_name]]
    params <- extract_selected_model_parameters(fit, model_name)
    openxlsx::addWorksheet(wb, model_name)
    openxlsx::writeData(wb, model_name, params)
  }
  openxlsx::saveWorkbook(wb, "os_model_parameters.xlsx", overwrite = TRUE)
})

#########################################################
#### Goodness of Fit Table Information ##################
#########################################################
observeEvent(input$info_os_gof, {
  showModal(
    modalDialog(
      title = "Goodness of Fit Table",
      tagList(
        tags$p(
          "This table summarises the goodness-of-fit statistics for all fitted ",
          "parametric survival distributions using the uploaded survival dataset"
        ),
        tags$ul(
          tags$li(
            tags$b("AIC (Akaike Information Criterion)"),
            " and ",
            tags$b("BIC (Bayesian Information Criterion)"),
            " are reported for each distribution"
          ),
          tags$li(
            "The distribution shown in the first row has the lowest AIC value ",
            "and therefore provides the best statistical fit to the observed data"
          ),
          tags$li(
            "Click a distribution in the table to display its corresponding ",
            "survival curve in the adjacent plot"
          ),
          tags$li(
            "Click a selected distribution again to remove its curve from the plot"
          ),
          tags$li(
            "Multiple distributions may be selected for visual comparison"
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Important:")
        ),
        tags$ul(
          tags$li(
            "Only one distribution should be selected when generating model results"
          ),
          tags$li(
            "If multiple distributions are selected, survival extrapolation, ",
            "state occupancy calculations, and downstream model outputs will not be generated"
          )
        )
      ),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton("Close")
      )
    )
  )
})

#########################################################
#### OS Goodness of Fit Table ###########################
#########################################################
comp_os_fit_table <- reactive({
  req(comp_os_models())
  create_fit_table(comp_os_models())
})

#########################################################
#### OS AIC TABLE #######################################
#########################################################
output$comp_os_aic_table <- DT::renderDT({
  req(input$comp_os_input_type == "Survival Data")
  DT::datatable(
    comp_os_fit_table(),
    rownames = FALSE,
    selection = "multiple",
    options = list(
      pageLength = 10,
      dom = "t"
    )
  )
})

output$download_comp_os_aic_table <- downloadHandler(
  filename = function() {
    paste0("OS_Goodness_of_Fit_", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(input$comp_os_input_type == "Survival Data")
    write.csv(comp_os_fit_table(), file, row.names = FALSE)
  }
)

# proper toggle solution
selected_comp_os_rows <- reactiveVal(integer(0))
observeEvent(selected_comp_os_rows(), {
  clicked <- selected_comp_os_rows()
  current <- selected_comp_os_rows()
  for(r in clicked){
    if(r %in% current){
      current <- setdiff(current, r)
    } else {
      current <- c(current, r)
    }
  }
  selected_comp_os_rows(sort(unique(current)))
})

output$debug_ipd <- renderPrint({
  ipd <- comp_os_ipd()
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
comp_os_predictions <- reactive({
  req(comp_os_models())
  req(input$time_horizon)
  generate_survival_predictions(
    comp_os_models(),
    input$time_horizon
  )
})

output$download_comp_os_model_output <- downloadHandler(
  filename = function() {
    paste0("OS_Model_Output_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    wb <- openxlsx::createWorkbook()
    #################################################
    #### Parameters
    #################################################
    params <- extract_model_parameters(comp_os_models())
    openxlsx::addWorksheet(wb, "Parameters")
    openxlsx::writeData(wb, "Parameters", params)
    #################################################
    #### Predictions
    #################################################
    openxlsx::addWorksheet(wb, "Predictions")
    openxlsx::writeData(wb, "Predictions", comp_os_predictions())
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)

#########################################################
#### Combined Survival data #############################
#########################################################
comp_os_all_survival <- reactive({
  req(comp_os_ipd())
  req(comp_os_models())
  req(input$time_horizon)
  km <- create_km_from_ipd(comp_os_ipd())
  preds <- comp_os_predictions()
  dplyr::bind_rows(km, preds)
})

#########################################################
#### Model Selection ####################################
#########################################################
# function to select multiple curves
selected_comp_os_models <- reactive({
  req(comp_os_fit_table())
  tbl <- comp_os_fit_table()
  rows <- input$comp_os_aic_table_rows_selected
  if(is.null(rows) || length(rows)==0){
    return(character(0))
  }
  tbl$Model[rows]
})


visible_comp_os_survival <- reactive({
  surv <- comp_os_all_survival()
  selected <- selected_comp_os_models()
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
#### Plotting function  #################################
#########################################################
plot_survival_models <- function(
    preds,
    # selected_model = NULL,
    horizon = NULL,
    validation_data = NULL,
    validation_time = NULL,
    validation_prob = NULL
){
  p <- plotly::plot_ly()
  for(m in unique(preds$model)){
    df <- preds[preds$model == m, ]
    # width <- 2
    # opacity <- 0.8
    # if(!is.null(selected_model) && m == selected_model){
    #   width <- 5
    #   opacity <- 1
    # }
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
          #   opacity = opacity
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
        range = c(0, 1),
        fixedrange = TRUE,
        tickmode = "array",
        tickvals = c(0, 0.25, 0.5, 0.75, 1),
        ticktext = c("0.00", "0.25", "0.50", "0.75", "1.00"),
        nticks = 5,
        showgrid = TRUE,
        zeroline = FALSE
      )
    )
}


observe({
  cat("\n====================\n")
  cat("LONG TERM DATA\n")
  cat("====================\n")
  print(head(longterm_os_validation_comp()))
})

#########################################################
#### Long-Term OS Validation Input ######################
#########################################################
longterm_os_validation_comp <- reactive({
  if (is.null(input$comp_longterm_os_file))
    return(NULL)
  df <- read.csv(
    input$comp_longterm_os_file$datapath
  )
  validate(
    need(
      all(c("time","surv") %in% names(df)),
      "Long-term validation file must contain columns time and surv"
    )
  )
  names(df) <- tolower(names(df))
  df
})

#########################################################
#### Long-Term OS Validation Download ###################
#########################################################
# output$download_comp_longterm_os_example <- downloadHandler(
#   filename = function() {
#     paste0("LongTerm_OS_Validation_",Sys.Date(),".csv")
#   },
#   content = function(file) {
#     req(longterm_os_validation_comp())
#     write.csv(longterm_os_validation_comp(),file,row.names = FALSE)
#   }
# )
#########################################################
#### Long-Term OS Validation Download ###################
#########################################################
output$download_comp_longterm_os_example <- downloadHandler(
  filename = function() {
    paste0("LongTerm_OS_Validation_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(longterm_os_comp, file, row.names = FALSE)
  }
)



output$comp_os_fit_plot <- plotly::renderPlotly({
  plot_survival_models(
    preds = visible_comp_os_survival(),
    selected_model = NULL,
    horizon = input$time_horizon,
    validation_data =
      longterm_os_validation_comp(),
    validation_time =
      input$comp_longterm_os_time,
    validation_prob =
      input$comp_longterm_os_prob
  )
})

#########################################################
#### Extrapolation download  ############################
#########################################################
output$download_comp_os_extrapolation <- downloadHandler(
  filename = function(){
    paste0("OS_extrapolation_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(comp_os_all_survival(), file,row.names = FALSE)
  }
)

#########################################################
#### Comparator OS selected model reactive ##############
#########################################################
selected_comp_os_curve <- reactive({
  req(input$comp_os_input_type)
  #######################################################
  #### MEDIAN OS ########################################
  #######################################################
  if(input$comp_os_input_type == "Median OS"){
    curve <- comp_median_os_curve()
    names(curve)[
      names(curve) == "survival"
    ] <- "surv"
    curve$model <- "Median OS"
    return(curve)
  }
  #######################################################
  #### SURVIVAL DATA ####################################
  #######################################################
  if(input$comp_os_input_type == "Survival Data"){
    req(comp_os_all_survival())
    selected_models <- selected_comp_os_models()
    req(length(selected_models) > 0)
    model_name <- selected_models[1]
    return(
      get_model_survival_curve(
        all_survival = comp_os_all_survival(),
        selected_model = model_name
      )
    )
  }
})

#########################################################
#### Comparator Plot active model #######################
#########################################################
output$comp_os_active_model <- renderText({
  req(input$comp_os_input_type)
  if(input$comp_os_input_type == "Median OS"){
    return("Active model: Median OS")
  }
  selected <- selected_comp_os_models()
  if(length(selected) == 0){
    best_model <- comp_os_fit_table()$Model[1]
    return(
      paste0("Active model: ",best_model," (Lowest AIC)")
    )
  }
  paste0("Selected models: ",paste(selected, collapse = ", "))
})

# output$comp_os_reconstructed_ipd_preview <- DT::renderDT({   
#   req(input$comp_os_survival_type == "Digitized KM Curve")  
#   ipd <- comp_os_ipd()
#   
#   validate(
#     need(
#       !is.null(ipd),
#       "IPD reconstruction failed"
#     )
#   )
#   
#   DT::datatable(
#     head(ipd, 20),
#     rownames = FALSE,
#     options = list(
#       pageLength = 20,
#       scrollX = TRUE
#     )
#   )
# })



#########################################################
#### PFS Survival IPD data ##############################
#########################################################
output$download_comp_pfs_ipd_example <- downloadHandler(
  filename = function() {
    paste0("PFS_IPD_",Sys.Date(),".csv")
  },
  content = function(file) {
    req(
      input$comp_pfs_input_type == "Survival Data",
      input$comp_pfs_survival_type == "IPD"
    )
    write.csv(comp_pfs_ipd(),file,row.names = FALSE)
  }
)

#########################################################
#### PFS IPD Input ######################################
#########################################################
# comp_pfs_ipd <- reactive({
#   req(input$comp_pfs_input_type == "Survival Data")
#   req(input$comp_pfs_survival_type)
#   if(input$comp_pfs_survival_type == "IPD"){
#     req(input$comp_pfs_ipd)
#     read_ipd_data(input$comp_pfs_ipd)
#   } else {
#     req(input$comp_pfs_km_survival)
#     req(input$comp_pfs_km_risk)
#     km_data <- read.csv(input$comp_pfs_km_survival$datapath)
#     risk_data <- read.csv(input$comp_pfs_km_risk$datapath)
#     reconstruct_ipd(
#       n_at_risk = risk_data,
#       p_survival = km_data
#     )
#   }
# })

#########################################################
#### PFS IPD INPUT ######################################
#########################################################
comp_pfs_ipd <- reactive({
  cat("\n=================================\n")
  cat("ENTERING comp_pfs_ipd()\n")
  cat("=================================\n")
  req(input$comp_pfs_input_type == "Survival Data")
  req(input$comp_pfs_survival_type)
  cat(
    "survival type:",
    input$comp_pfs_survival_type,
    "\n"
  )
  #######################################################
  #### IPD ROUTE ########################################
  #######################################################
  if (input$comp_pfs_survival_type == "IPD") {
    cat("ROUTE = IPD\n")
    if (is.null(comp_pfs_ipd_file())) {
      cat("Using default PFS IPD file\n")
      ipd <- default_pfs_ipd
    } else {
      cat("Using uploaded PFS IPD file\n")
      ipd <- read_ipd_data(
        comp_pfs_ipd_file()
      )
    }
    cat("\nIPD SUMMARY\n")
    cat("Rows:", nrow(ipd), "\n")
    cat("Events:", sum(ipd$status == 1), "\n")
    cat("Censored:", sum(ipd$status == 0), "\n")
    return(ipd)
  }
  #######################################################
  #### DIGITIZED KM CURVE ROUTE #########################
  #######################################################
  cat("ROUTE = DIGITIZED KM CURVE\n")
  km_data <- if (is.null(comp_pfs_km_survival_file())) {
    cat("Using default PFS survival file\n")
    default_pfs_survival
  } else {
    cat("Using uploaded PFS survival file\n")
    read.csv(
      comp_pfs_km_survival_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  risk_data <- if (is.null(comp_pfs_km_risk_file())) {
    cat("Using default PFS risk file\n")
    default_pfs_risk
  } else {
    cat("Using uploaded PFS risk file\n")
    read.csv(
      comp_pfs_km_risk_file()$datapath,
      stringsAsFactors = FALSE
    )
  }
  #######################################################
  #### DEBUG ############################################
  #######################################################
  cat("\nPFS KM DATA\n")
  print(dim(km_data))
  print(names(km_data))
  print(head(km_data))
  cat("\nPFS RISK DATA\n")
  print(dim(risk_data))
  print(names(risk_data))
  print(head(risk_data))
  #######################################################
  #### RECONSTRUCT ######################################
  #######################################################
  result <- tryCatch({
    reconstruct_ipd(
      n_at_risk = risk_data,
      p_survival = km_data
    )
  }, error = function(e){
    cat("\nRECONSTRUCTION FAILED\n")
    cat(e$message, "\n")
    NULL
  })
  #######################################################
  #### DEBUG RESULT #####################################
  #######################################################
  cat("\n========================\n")
  cat("RECONSTRUCTION RESULT\n")
  cat("========================\n")
  if (is.null(result)) {
    cat("NULL RESULT\n")
  } else {
    cat("Rows:", nrow(result), "\n")
    cat("Events:", sum(result$status == 1), "\n")
    cat("Censored:", sum(result$status == 0), "\n")
    print(head(result, 10))
  }
  result
})

output$comp_pfs_ipd_source <- renderText({
  if(is.null(comp_pfs_ipd_file())){
    "Currently using: Default PFS IPD dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      comp_pfs_ipd_file()$name
    )
  }
})

output$comp_pfs_survival_source <- renderText({
  if(is.null(comp_pfs_km_survival_file())){
    "Currently using: Default PFS Survival dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      comp_pfs_km_survival_file()$name
    )
  }
})

output$comp_pfs_risk_source <- renderText({
  if(is.null(comp_pfs_km_risk_file())){
    "Currently using: Default PFS Risk dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      comp_pfs_km_risk_file()$name
    )
  }
})

#########################################################
#### PFS IPD Diagnostics ################################
#########################################################
observe({
  req(
    input$comp_pfs_input_type == "Survival Data",
    input$comp_pfs_survival_type == "IPD"
  )
  ipd <- comp_pfs_ipd()
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
output$download_comp_pfs_reconstructed_ipd <- downloadHandler(
  filename = function(){
    paste0("reconstructed_PFS_IPD_", Sys.Date(), ".csv")
  },
  content = function(file){
    req(comp_pfs_ipd())
    write.csv(comp_pfs_ipd(), file, row.names = FALSE)
  }
)

#########################################################
#### PFS KM Curve downloads #############################
#########################################################
output$download_comp_pfs_prob_survival <- downloadHandler(
  filename = function() {
    "Example_PFS_Survival.csv"
  },
  content = function(file) {
    write.csv(
      default_pfs_survival,
      file,
      row.names = FALSE
    )
  }
)

output$download_comp_pfs_natrisk <- downloadHandler(
  filename = function() {
    "Example_PFS_Risk.csv"
  },
  content = function(file) {
    write.csv(
      default_pfs_risk,
      file,
      row.names = FALSE
    )
  }
)

#########################################################
#### PFS Models #########################################
#########################################################
comp_pfs_models <- reactive({
  req(comp_pfs_ipd())
  ipd <- comp_pfs_ipd()
  cat("\n=========================\n")
  cat("PFS IPD SUMMARY\n")
  cat("=========================\n")
  cat("Rows:", nrow(ipd), "\n")
  cat("Events:", sum(ipd$status == 1), "\n")
  cat("Censored:", sum(ipd$status == 0), "\n")
  cat("\nStatus Table:\n")
  print(table(ipd$status))
  cat("\nTime Summary:\n")
  print(summary(ipd$time))
  cat("\nFirst 10 rows:\n")
  print(head(ipd, 10))
  fit_parametric_models(ipd)
})

#########################################################
#### PFS Goodness of Fit Table ##########################
#########################################################
comp_pfs_fit_table <- reactive({
  req(comp_pfs_models())
  create_fit_table(
    comp_pfs_models()
  )
})

output$comp_pfs_aic_table <- DT::renderDT({
  req(
    input$comp_pfs_input_type ==
      "Survival Data"
  )
  DT::datatable(
    comp_pfs_fit_table(),
    rownames = FALSE,
    selection = "multiple",
    options = list(
      pageLength = 10,
      dom = "t"
    )
  )
})

#########################################################
#### PFS Goodness of Fit Table Information ##############
#########################################################
observeEvent(input$info_pfs_gof, {
  showModal(
    modalDialog(
      title = "Goodness of Fit Table",
      tagList(
        tags$p(
          "This table summarises the goodness-of-fit statistics for all fitted ",
          "parametric survival distributions using the uploaded survival dataset"
        ),
        tags$ul(
          tags$li(
            tags$b("AIC (Akaike Information Criterion)"),
            " and ",
            tags$b("BIC (Bayesian Information Criterion)"),
            " are reported for each distribution"
          ),
          tags$li(
            "The distribution shown in the first row has the lowest AIC value ",
            "and therefore provides the best statistical fit to the observed data"
          ),
          tags$li(
            "Click a distribution in the table to display its corresponding ",
            "survival curve in the adjacent plot"
          ),
          tags$li(
            "Click a selected distribution again to remove its curve from the plot"
          ),
          tags$li(
            "Multiple distributions may be selected for visual comparison"
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Important:")
        ),
        tags$ul(
          tags$li(
            "Only one distribution should be selected when generating model results"
          ),
          tags$li(
            "If multiple distributions are selected, survival extrapolation, ",
            "state occupancy calculations, and downstream model outputs will not be generated"
          )
        )
      ),
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton("Close")
      )
    )
  )
})

output$download_comp_pfs_aic <- downloadHandler(
  filename = function() {
    paste0("PFS_Goodness_of_Fit_",Sys.Date(),".csv")
  },
  content = function(file) {
    write.csv(comp_pfs_fit_table(), file, row.names = FALSE)
  }
)

#########################################################
#### PFS Debugging input ################################
#########################################################
output$debug_pfs_ipd <- renderPrint({
  ipd <- comp_pfs_ipd()
  cat("Rows:", nrow(ipd), "\n")
  cat("Events:", sum(ipd$status == 1), "\n")
  cat("Censored:", sum(ipd$status == 0), "\n")
  print(summary(ipd$time))
})

#########################################################
#### PFS Predictions ####################################
#########################################################
comp_pfs_predictions <- reactive({
  req(comp_pfs_models())
  req(input$time_horizon)
  generate_survival_predictions(
    comp_pfs_models(),
    input$time_horizon
  )
})

#########################################################
#### PFS Combined Survival ##############################
#########################################################
comp_pfs_all_survival <- reactive({
  req(comp_pfs_ipd())
  req(comp_pfs_models())
  req(input$time_horizon)
  km <- create_km_from_ipd(
    comp_pfs_ipd()
  )
  preds <- comp_pfs_predictions()
  dplyr::bind_rows(
    km,
    preds
  )
})

#########################################################
#### PFS Active model input #############################
#########################################################
output$comp_pfs_active_model <- renderText({
  selected <- selected_comp_pfs_models()
  if(length(selected) == 0){
    best_model <- comp_pfs_fit_table()$Model[1]
    return(
      paste0("Active model: ",best_model," (Lowest AIC)"))
  }
  paste0("Selected models: ",paste(selected, collapse = ", "))
})

#########################################################
#### PFS Selected Models ###############################
#########################################################
selected_comp_pfs_models <- reactive({
  req(comp_pfs_fit_table())
  tbl <- comp_pfs_fit_table()
  rows <- input$comp_pfs_aic_table_rows_selected
  if(is.null(rows) || length(rows) == 0){
    return(character(0))
  }
  tbl$Model[rows]
})

visible_comp_pfs_survival <- reactive({
  surv <- comp_pfs_all_survival()
  selected <- selected_comp_pfs_models()
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
#### PFS Plot  ##########################################
#########################################################
output$comp_pfs_fit_plot <- plotly::renderPlotly({
  req(comp_pfs_fit_table())
  plot_survival_models(
    preds = visible_comp_pfs_survival(),
    selected_model = NULL,
    horizon = input$time_horizon,
    validation_data = longterm_pfs_validation_comp(),
    validation_time = input$comp_longterm_pfs_time,
    validation_prob =input$comp_longterm_pfs_prob
  )
})


#########################################################
#### PFS curve reactive #################################
#########################################################
selected_comp_pfs_curve <- reactive({
  req(input$comp_pfs_input_type)
  #######################################################
  #### MEDIAN PFS #######################################
  #######################################################
  if(input$comp_pfs_input_type == "Median PFS"){
    curve <- comp_median_pfs_curve()
    names(curve)[
      names(curve) == "survival"
    ] <- "surv"
    curve$model <- "Median PFS"
    return(curve)
  }
  #######################################################
  #### SURVIVAL DATA ####################################
  #######################################################
  if(input$comp_pfs_input_type == "Survival Data"){
    req(comp_pfs_all_survival())
    selected_models <- selected_comp_pfs_models()
    req(length(selected_models) > 0)
    model_name <- selected_models[1]
    return(
      get_model_survival_curve(
        all_survival = comp_pfs_all_survival(),
        selected_model = model_name
      )
    )
  }
})

#########################################################
#### PFS Extrapolation download #########################
#########################################################
output$download_comp_pfs_extrapolation <- downloadHandler(
  filename = function(){
    paste0("PFS_extrapolation_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(comp_pfs_all_survival(), file, row.names = FALSE)
  }
)

output$download_comp_pfs_model_output <- downloadHandler(
  filename = function() {
    paste0("PFS_Model_Output_", Sys.Date(), ".xlsx")
  },
  content = function(file) {
    wb <- openxlsx::createWorkbook()
    #################################################
    #### Parameters
    #################################################
    params <- extract_model_parameters(comp_pfs_models())
    openxlsx::addWorksheet(wb, "Parameters")
    openxlsx::writeData(wb, "Parameters", params)
    #################################################
    #### Predictions
    #################################################
    openxlsx::addWorksheet(wb, "Predictions")
    openxlsx::writeData(wb, "Predictions", comp_pfs_predictions())
    #################################################
    #### Save Workbook
    #################################################
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)

#########################################################
#### Population Settings Info ###########################
#########################################################
observeEvent(input$info_population, {
  showModal(
    modalDialog(
      title = "Population Settings",
      HTML(
        paste0(
          "<p><b>Starting Age</b> represents the age at model entry.</p>",
          "<p><b>Background Mortality</b> should contain age-specific mortality ",
          "rates (i.e., age, male_qx and female_qx columns) used to apply general population ",
          "mortality within the model.</p>",
          "<p>These inputs are used when estimating long-term survival and ",
          "state occupancy.</p>"
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "m"
    )
  )
})


#########################################################
#### Background Mortality Input #########################
#########################################################
############## create reactive ##########################
apply_background_mortality_flag <- reactive({
  isTRUE(input$apply_background_mortality)})

############## update the status #########################
output$background_mortality_source <- renderText({
  source_text <- if(is.null(comp_background_mortality_file())) {
    "Default Background Mortality dataset loaded"
  } else {
    paste("Uploaded file:", comp_background_mortality_file()$name)
  }
  status_text <- if(isTRUE(input$apply_background_mortality)) {
    "Status: Background Mortality Applied"
  } else {
    "Status: Background Mortality Not Applied"
  }
  paste(source_text, "\n", status_text)
})


background_mortality <- reactive({
  bg <- if(
    is.null(comp_background_mortality_file())
  ){
    default_bgm
  } else {
    read.csv(
      comp_background_mortality_file()$datapath)
  }
  names(bg) <- tolower(names(bg))
  validate(
    need(
      all(c("age", "male_qx", "female_qx") %in% names(bg)),
      "Background mortality file must contain columns: age, male_qx and female_qx"
    )
  )
  bg <- bg[order(bg$age), ]
  bg[, c("age", "male_qx", "female_qx")]
})

starting_age <- reactive({
  input$starting_age
})

#########################################################
#### Male Population Proportion #########################
#########################################################
male_proportion <- reactive({
  validate(
    need(
      input$male_proportion >= 0 &&
        input$male_proportion <= 1,
      "Male proportion must be between 0 and 1."
    )
  )
  input$male_proportion
})


output$background_mortality_source <- renderText({
  if(is.null(comp_background_mortality_file())) {
    "Currently using: Default Background Mortality dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      comp_background_mortality_file()$name
    )
  }
})


#########################################################
#### Background Mortality Download ######################
#########################################################
output$download_background_mortality_example <- downloadHandler(
  filename = function() {
    paste0("Background_Mortality_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(default_bgm, file, row.names = FALSE)
  }
)


#########################################################
#### Long-Term PFS Validation Input #####################
#########################################################
longterm_pfs_validation_comp <- reactive({
  if(is.null(input$comp_longterm_pfs_file)){
    return(NULL)
  }
  df <- read.csv(
    input$comp_longterm_pfs_file$datapath
  )
  names(df) <- tolower(names(df))
  df
})

#########################################################
#### Long-Term PFS Validation download_comp #############
#########################################################
# output$download_comp_longterm_pfs_example <- downloadHandler(
#   filename = function() {
#     paste0("LongTerm_PFS_Validation_",Sys.Date(),".csv")
#   },
#   content = function(file) {
#     req(longterm_pfs_validation_comp())
#     write.csv(longterm_pfs_validation_comp(),file,row.names = FALSE)
#   }
# )
#########################################################
#### Long-Term PFS Validation download ##################
#########################################################
output$download_comp_longterm_pfs_example <- downloadHandler(
  filename = function() {
    paste0("LongTerm_PFS_Validation_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(lonterm_pfs_comp, file, row.names = FALSE)
  }
)

###########################################################################
comp_os_object <- reactive({
  cat("\n=====================\n")
  cat("ENTERING comp_os_object()\n")
  cat("=====================\n")
  cat("input type:\n")
  print(input$comp_os_input_type)
})


#########################################################
#### OS Model reactive ##################################
#########################################################
comp_os_object <- reactive({
  if(input$comp_os_input_type == "Median OS"){
    req(input$comp_median_os)
    validate(
      need(input$comp_median_os > 0, "Median OS must be > 0")
    )
    build_survival_object(
      input_type = "Median",
      median_value = input$comp_median_os,
      horizon = input$time_horizon,
      curve_name = "Median OS"
    )
  } else {
    build_survival_object(
      input_type = "Survival",
      ipd = comp_os_ipd(),
      horizon = input$time_horizon
    )
  }
})

observe({
  req(comp_os_object())
  cat("\n=====================\n")
  cat("COMPARATOR OS OBJECT\n")
  cat("=====================\n")
  print(comp_os_object()$type)
})

observe({
  req(comp_os_object())
  print_survival_summary(
    comp_os_object()
  )
})

#########################################################
#### PFS Model reactive #################################
#########################################################
comp_pfs_object <- reactive({
  if(input$comp_pfs_input_type == "Median PFS"){
    req(input$comp_median_pfs)
    validate(
      need(input$comp_median_pfs > 0, "Median PFS must be > 0")
    )
    build_survival_object(
      input_type = "Median",
      median_value = input$comp_median_pfs,
      horizon = input$time_horizon,
      curve_name = "Median PFS"
    )
  } else {
    build_survival_object(
      input_type = "Survival",
      ipd = comp_pfs_ipd(),
      horizon = input$time_horizon
    )
  }
})

#########################################################
#### Selected OS Model ##################################
#########################################################
comp_os_selected_model <- reactive({
  req(comp_os_object())
  if(comp_os_object()$type == "median"){
    return(NULL)
  }
  select_active_model(
    fit_table = comp_os_object()$package$fit_table,
    selected_rows = input$comp_os_aic_table_rows_selected
  )
})

#########################################################
#### Selected PFS Model #################################
#########################################################
comp_pfs_selected_model <- reactive({
  req(comp_pfs_object())
  if(comp_pfs_object()$type == "median"){
    return(NULL)
  }
  select_active_model(
    fit_table = comp_pfs_object()$package$fit_table,
    selected_rows = input$comp_pfs_aic_table_rows_selected
  )
})

observe({
  cat("\n====================\n")
  cat("OS TABLE ROWS\n")
  cat("====================\n")
  print(input$comp_os_aic_table_rows_selected)
})

observe({
  cat("\n====================\n")
  cat("PFS TABLE ROWS\n")
  cat("====================\n")
  print(input$comp_pfs_aic_table_rows_selected)
})

observe({
  req(comp_os_curve())
  cat("\nOS CURVE MODEL\n")
  print(unique(comp_os_curve()$model))
})

observe({
  req(comp_pfs_curve())
  cat("\nPFS CURVE MODEL\n")
  print(unique(comp_pfs_curve()$model))
})

#########################################################
#### Active OS Curve ####################################
#########################################################
comp_os_curve <- reactive({
  req(comp_os_object())
  get_active_curve(
    survival_object =comp_os_object(),
    selected_model = comp_os_selected_model()
  )
})

#########################################################
#### Active PFS Curve ###################################
#########################################################
comp_pfs_curve <- reactive({
  req(comp_pfs_object())
  get_active_curve(
    survival_object = comp_pfs_object(),
    selected_model = comp_pfs_selected_model()
  )
})


comp_occupancy_object <- reactive({
  req(comp_os_curve())
  req(comp_pfs_curve())
  build_occupancy_object(
    os_curve = comp_os_curve(),
    pfs_curve = comp_pfs_curve(),
    start_age = input$starting_age,
    mortality_table = background_mortality(),
    apply_bgm = apply_background_mortality_flag(),
    male_proportion = input$male_proportion
  )
})


observe({
  req(comp_os_selected_model())
  cat("\n====================\n")
  cat("SELECTED OS MODEL\n")
  cat("====================\n")
  print(comp_os_selected_model())
})

observe({
  req(comp_pfs_selected_model())
  cat("\n====================\n")
  cat("SELECTED PFS MODEL\n")
  cat("====================\n")
  print(comp_pfs_selected_model())
})

observe({
  req(comp_os_curve())
  cat("\nOS CURVE MODEL\n")
  print(unique(comp_os_curve()$model))
})

observe({
  req(comp_pfs_curve())
  cat("\nPFS CURVE MODEL\n")
  print(unique(comp_pfs_curve()$model))
})

observe({
  cat("\nOS ROW SELECTED:\n")
  print(
    input$comp_os_fit_table_rows_selected
  )
})

observe({
  cat("\nPFS ROW SELECTED:\n")
  print(
    input$comp_pfs_fit_table_rows_selected
  )
})

# observe({
#   req(comp_occupancy_object())
#   write.csv(comp_occupancy_object()$states,"occupancy_debug.csv",row.names = FALSE)
#   cat("\nCSV WRITTEN\n")
# })

observe({
  req(comp_occupancy_object())
  cat("\n=====================\n")
  cat("COMPARATOR OCCUPANCY\n")
  cat("=====================\n")
  print(head(comp_occupancy_object()$states))
  occ <- comp_occupancy_object()$states
  cat("\nMONTH 12\n")
  print(
    occ[occ$time == 12, ]
  )
})

# Model testing verification
observe({
  req(comp_pfs_curve())
  cat("\nLOGNORMAL MONTH 12\n")
  print(
    comp_pfs_curve()[
      comp_pfs_curve()$time == 12,
    ]
  )
})

observe({
  req(comp_pfs_curve())
  cat("\n====================\n")
  cat("ACTIVE PFS CURVE\n")
  cat("====================\n")
  print(unique(comp_pfs_curve()$model))
  print(
    comp_pfs_curve()[
      comp_pfs_curve()$time == 12,
    ]
  )
})

observe({
  req(comp_os_curve())
  cat("\n====================\n")
  cat("ACTIVE OS CURVE\n")
  cat("====================\n")
  print(unique(comp_os_curve()$model))
  print(
    comp_os_curve()[
      comp_os_curve()$time == 12,
    ]
  )
})

output$download_comp_state_occupancy <- downloadHandler(
  filename = function() {
    paste0(
      "Comparator_State_Occupancy_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".csv"
    )
  },
  content = function(file) {
    req(comp_occupancy_object())
    write.csv(
      comp_occupancy_object()$states,
      file,
      row.names = FALSE
    )
  }
)

observe({
  req(comp_os_object())
  req(comp_os_object()$type == "survival")
  params_df <- extract_extrapolation_parameters(
    comp_os_object()$package$models
  )
  write.csv(
    params_df,
    "os_extrapolation_parameters.csv",
    row.names = FALSE
  )
  cat("\nOS EXTRAPOLATION PARAMETERS WRITTEN\n")
})

observe({
  req(comp_pfs_object())
  req(comp_pfs_object()$type == "survival")
  params_df <- extract_extrapolation_parameters(
    comp_pfs_object()$package$models
  )
  write.csv(
    params_df,
    "pfs_extrapolation_parameters.csv",
    row.names = FALSE
  )
  cat("\nPFS EXTRAPOLATION PARAMETERS WRITTEN\n")
})


observe({
  req(comp_os_object())
  fit <- comp_os_object()$package$models[[1]]
  cat("\n====================\n")
  cat("MODEL STRUCTURE\n")
  cat("====================\n")
  print(names(fit))
  cat("\nRES\n")
  print(fit$res)
  cat("\nCOEF\n")
  print(coef(fit))
})



observe({
  req(comp_os_curve())
  write.csv(
    comp_os_curve(),
    "active_os_curve_debug.csv",
    row.names = FALSE
  )
  cat("\nACTIVE OS CURVE CSV WRITTEN\n")
})


observe({
  req(comp_pfs_curve())
  write.csv(
    comp_pfs_curve(),
    "active_pfs_curve_debug.csv",
    row.names = FALSE
  )
  cat("\nACTIVE PFS CURVE CSV WRITTEN\n")
})

#Long term validation info
observeEvent(input$info_longterm_validation, {
  showModal(
    modalDialog(
      title = "Long-Term Validation",
      tagList(
        tags$p(
          "Long-term validation is used to assess whether the extrapolated survival curves remain clinically plausible beyond the observed trial follow-up period"
        ),
        tags$hr(),
        tags$p(
          tags$b("How does it work?")
        ),
        tags$ul(
          tags$li(
            "Users can specify an expected survival probability at a future time point (e.g. 5-year or 10-year survival)"
          ),
          tags$li(
            "Optional external survival data may also be uploaded from long-term follow-up studies, registries, observational datasets, or published literature"
          ),
          tags$li(
            "The validation point and any uploaded validation data are displayed on the extrapolation plots alongside the fitted parametric survival curves"
          ),
          tags$li(
            "This allows visual comparison of the extrapolated survival projections against external evidence and clinically expected long-term outcomes"
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Why is long-term validation important?")
        ),
        tags$ul(
          tags$li(
            "Clinical trial data are often immature and may only cover a limited follow-up period"
          ),
          tags$li(
            "Different parametric distributions can provide similar fits to the observed data but generate substantially different long-term survival projections"
          ),
          tags$li(
            "Long-term validation helps identify extrapolations that are statistically acceptable but clinically implausible"
          ),
          tags$li(
            "This supports the selection of survival models that are both statistically robust and clinically credible"
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Typical sources of long-term validation evidence include:")
        ),
        tags$ul(
          tags$li("Long-term extension studies"),
          tags$li("Disease registries"),
          tags$li("Real-world evidence datasets"),
          tags$li("Published literature"),
          tags$li("Clinical expert opinion")
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
})