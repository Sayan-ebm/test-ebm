ModelsettingsUI <- fluidPage(
  #====================================================
  # HEADER
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "General Settings and Model Configuration",
      status = "success",
      solidHeader = TRUE,
      p(
        icon("sliders"),
        " Define the global assumptions that control model execution, including perspective, discounting, probabilistic sensitivity analysis, and willingness-to-pay thresholds."
      )
    )
  ),
  br(),
  #====================================================
  # MAIN SETTINGS PANELS
  #====================================================
  fluidRow(
    #================================================
    # ANALYSIS SETTINGS
    #================================================
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          min-height:320px;
        ",
        tags$div(
          style = "
            background:#EAF2FF;
            padding:10px;
            border-left:5px solid #3C8DBC;
            border-radius:6px;
            margin-bottom:20px;
            font-weight:600;
            font-size:16px;
          ",
          icon("chart-line"),
          " Analysis Settings"
        ),
        textInput(
          inputId = "country",
          label = tagList(
            icon("globe"),
            " Country"
          ),
          value = "",
          placeholder = "e.g. United Kingdom, United States, Canada"
        ),
        numericInput(
          "discount_rate_cost",
          label = tagList(
            tagList(
              icon("money-bill-wave"),
              " Discount Rate - Costs (%)"
            ),
            actionButton(
              inputId = paste0("reset_", "discount_rate_cost"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 3.5,
          min = 0
        ),
        numericInput(
          "discount_rate_QALY",
          label = tagList(
            tagList(
              icon("heart"),
              " Discount Rate - Health Outcomes (%)"
            ),
            actionButton(
              inputId = paste0("reset_", "discount_rate_QALY"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 3.5,
          min = 0
        ),
        numericInput(
          "WTP",
          label = tagList(
            tagList(
              icon("gauge-high"),
              " Willingness-to-Pay Threshold"
            ),
            actionButton(
              inputId = paste0("reset_", "WTP"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 35000,
          min = 0
        )
      )
    ),
    #================================================
    # PSA SETTINGS
    #================================================
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          min-height:320px;
        ",
        tags$div(
          style = "
            background:#FFF8E7;
            padding:10px;
            border-left:5px solid #F39C12;
            border-radius:6px;
            margin-bottom:20px;
            font-weight:600;
            font-size:16px;
          ",
          icon("dice"),
          " Probabilistic Sensitivity Analysis"
        ),
        checkboxInput(
          "run_psa",
          "Enable PSA",
          value = FALSE
        ),
        numericInput(
          "PSA_Iterations",
          label = tagList(
            tagList(
              icon("rotate"),
              " PSA Iterations"
            ),
            actionButton(
              inputId = paste0("reset_", "PSA_Iterations"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 1000,
          min = 100,
          step = 100
        )
      )
    )
  ),
  br(),
  #====================================================
  # ACTION BUTTONS
  #====================================================
  fluidRow(
    column(
      width = 12,
      div(
        style = "
        background:white;
        border:1px solid #DDE4EA;
        border-radius:8px;
        padding:15px;
        box-shadow:0px 1px 3px rgba(0,0,0,0.05);
      ",
        # STATUS (top)
        div(
          style = "
          margin-bottom:10px;
          display:flex;
          justify-content:flex-end;
        ",
          uiOutput("model_run_status")
        ),
        # BUTTON (bottom)
        div(
          style = "
          display:flex;
          justify-content:flex-end;
          gap:10px;
        ",
          actionButton(
            "run_model",
            "Run Model",
            icon = icon("play"),
            class = "btn-success"
          )
        )
      )
    )
  )
)


