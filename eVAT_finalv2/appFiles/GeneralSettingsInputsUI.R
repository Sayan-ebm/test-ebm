GeneralSettingsInputsUI <- fluidPage(
  
  #====================================================
  # HEADER
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "General Settings and Inputs",
      status = "success",
      solidHeader = TRUE,
      
      p(
        "This tab contains the general model settings and key input parameters ",
        "that define the overall configuration of the economic model."
      )
    )
  ),
  
  #====================================================
  # ANALYSIS SETTINGS
  #====================================================
  fluidRow(
    
    box(
      title = "Analysis Settings",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      
      selectInput(
        "perspective",
        "Perspective",
        choices = c("Healthcare Payer", "Societal")
      ),
      
      numericInput(
        "time_horizon",
        "Time Horizon (Years)",
        value = 30,
        min = 1
      ),
      
      numericInput(
        "discount_rate_cost",
        "Discount Rate - Costs (%)",
        value = 3.5,
        min = 0
      ),
      
      numericInput(
        "discount_rate_QALY",
        "Discount Rate - Health Outcomes (%)",
        value = 3.5,
        min = 0
      ),
      
      numericInput(
        "PSA_Iterations",
        "Number of PSA Iterations",
        value = 1000,
        min = 0
      ),
      
      
      numericInput(
        "WTP",
        "Willingness to Pay Threshold",
        value = 35000,
        min = 0
      )
      
    ),
    
    box(
      title = "Model Settings",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      
      selectInput(
        "Apply_HCC",
        "Apply Half Cycle Correction",
        choices = c("Yes", "No")
      ),
      
      selectInput(
        "Include_Tx_Cost",
        "Include Treatment Acquisition Costs",
        choices = c("Yes", "No")
      ),
      
      selectInput(
        "Include_HS_Cost",
        "Include Health State Costs",
        choices = c("Yes", "No")
      ),
      
      selectInput(
        "Include_AE_Cost",
        "Include Adverse Events Costs",
        choices = c("Yes", "No")
      ),
      
      selectInput(
        "Include_AE_Disutility",
        "Include Adverse Events Related Disutility",
        choices = c("Yes", "No")
      )
    )
    
  ),
  
  #====================================================
  # POPULATION & ECONOMIC SETTINGS
  #====================================================
  fluidRow(
    
    box(
      title = "Population Settings",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      
      numericInput(
        "start_age",
        "Starting Age",
        value = 40
      ),
      
      fileInput(
        inputId = "BGM",
        label = "Upload Background Mortality CSV"
      )
      
    ),
    
    box(
      title = "Economic Settings",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      
      selectInput(
        "currency",
        "Currency",
        choices = c("USD", "EUR", "GBP", "INR")
      ),
      
      numericInput(
        "cost_year",
        "Cost Year",
        value = 2025
      )
      
    )
    
  ),
  
  #====================================================
  # ACTION BUTTONS
  #====================================================
  fluidRow(
    
    column(
      width = 12,
      align = "right",
      
      actionButton(
        "reset_general",
        "Reset",
        icon = icon("rotate-left")
      ),
      
      actionButton(
        "save_general",
        "Save Settings",
        icon = icon("floppy-disk")
      ),
      
      actionButton(
        "run_model",
        "Run Model",
        icon = icon("play"),
        class = "btn-success"
      )
      
    )
    
  )
  
)