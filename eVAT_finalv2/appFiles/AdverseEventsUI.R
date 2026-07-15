AdverseEventsUI <- fluidPage(
  #==================================================
  # HEADER
  #==================================================
  fluidRow(
    box(
      width = 12,
      title = "Adverse Events (Treatment Toxicity Module)",
      status = "danger",
      solidHeader = TRUE,
      p(
        icon("triangle-exclamation"),
        " This module defines treatment-related adverse events (AEs), including grade 3/4 event probabilities, costs, and health disutilities. These are applied per cycle while patients remain on treatment and directly influence total costs and quality-adjusted life years (QALYs)."
      )
    )
  ),
  br(),
  #==================================================
  # MAIN CONTENT
  #==================================================
  fluidRow(
    #================================================
    # LEFT PANEL - ASSUMPTIONS
    #================================================
    column(
      width = 4,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          min-height:420px;
        ",
        tags$div(
          style = "
            background:#FDECEA;
            padding:10px;
            border-left:5px solid #C0392B;
            border-radius:6px;
            margin-bottom:20px;
            font-weight:600;
            font-size:16px;
          ",
          icon("shield-virus"),
          " AE Modelling Assumptions"
        ),
        checkboxInput(
          "include_ae_model",
          label = tagList(
            icon("gears"),
            " Include adverse events in economic model"
          ),
          value = TRUE
        ),
        helpText(
          "If unchecked, all AE probabilities, costs, and disutilities are set to zero."
        ),
        hr(),
        tags$div(
          style = "font-size:13px; color:#555;",
          tags$b("Application rules"),
          tags$ul(
            tags$li("Applied per cycle while on treatment"),
            tags$li("Grade 3/4 AEs affect costs and utilities")
          )
        )
      )
    ),
    #================================================
    # RIGHT PANEL - DATA INPUT
    #================================================
    column(
      width = 8,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
        ",
        
        #--------------------------------------------
        # HEADER
        #--------------------------------------------
        tags$div(
          style = "
            background:#E8F6EF;
            padding:10px;
            border-left:5px solid #27AE60;
            border-radius:6px;
            margin-bottom:20px;
            font-weight:600;
            font-size:16px;
          ",
          icon("file-import"),
          " Grade 3/4 Adverse Event Dataset"
        ),
        
        p(
          icon("database"),
          "Upload a structured CSV file where each row represents one adverse event with the corresponding event probabilities, costs and health disutilities."
        ),
        
        #--------------------------------------------
        # FILE INPUT LABEL
        #--------------------------------------------
        tags$label(
          
          tagList(
            
            icon("upload"),
            
            " Upload AE Dataset (CSV) ",
            
            div(
              style = "
                display:inline-flex;
                align-items:center;
                justify-content:center;
                width:22px;
                height:22px;
                border:1px solid #0073b7;
                border-radius:4px;
                cursor:pointer;
                margin-left:5px;
              ",
              
              tags$i(
                class = "fa fa-eye",
                
                title = paste(
                  
                  "Upload a CSV containing one row per adverse event.",
                  
                  "",
                  
                  "Probability inputs should be cumulative event probabilities observed over the study follow-up period (NOT per-cycle probabilities).",
                  
                  "",
                  
                  "For the required data structure, please refer to the provided Template_AE.csv file.",
                  
                  "",
                  
                  "For AE disutility uncertainty, provide either:",
                  
                  "• SE_disutility",
                  
                  "or",
                  
                  "• both LCI_disutility and UCI_disutility.",
                  
                  "",
                  
                  "If neither is supplied, a default standard error assumption will be used during probabilistic sensitivity analysis (PSA).",
                  
                  sep = "\n"
                ),
                
                style = "
                  color:#0073b7;
                  font-size:13px;
                "
              )
            )
          )
        ),
        fileInput(
          inputId = "ae_file",
          label = NULL,
          accept = ".csv"
        ),
        verbatimTextOutput("ae_data_source"),
        div(
          style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
          downloadButton(
            outputId = "download_ae_template",
            label = tagList(icon("download"), " Download Template CSV")
          ),
          actionButton(
            inputId = "reset_ae_file",
            label = "Reset to Default",
            icon = icon("undo"),
            class = "btn-reset-subtle"
          )
        ),
        br(), br(),
        # STRUCTURE GUIDE
        tags$div(
          style = "
            background:#FAFAFA;
            padding:12px;
            border-radius:6px;
            font-size:13px;
            color:#444;
          ",
          tags$b("Expected Dataset structure"),
          tags$hr(),
          tags$b(icon("tag"), " Event identification"),
          tags$ul(
            tags$li("Event name (e.g., Fatigue, Nausea)")
          ),
          tags$b(icon("chart-line"), " Probabilities (monthly)"),
          tags$ul(
            tags$li("Comparator probability"),
            tags$li("Comparator sample size (N)"),
            tags$li("Intervention probability"),
            tags$li("Intervention sample size (N)")
          ),
          tags$b(icon("coins"), " Costs"),
          tags$ul(
            tags$li("Cost per event (applied per occurrence)")
          ),
          tags$b(icon("heartbeat"), " Utilities"),
          tags$ul(
            tags$li("Disutility (negative utility decrement)"),
            tags$li("Standard error or 95% Confidence Intervals")
          )
        ),
      )
    )
  )
)