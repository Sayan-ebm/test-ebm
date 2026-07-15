#####################################################
#### PSA CARD #######################################
#####################################################
psaCard <- function(...) {
  div(
    style="
      background:#FFFFFF;
      border:1px solid #E5E7EB;
      border-radius:12px;
      padding:20px;
      margin-top:20px;
      box-shadow:0px 2px 8px rgba(0,0,0,0.06);
    ",
    ...
  )
}

#####################################################
#### PSA SECTION HEADER ##############################
#####################################################
psaHeader <- function(title, icon_name){
  tags$div(
    style="
      display:flex;
      align-items:center;
      gap:12px;
      background:#FFF5E8;
      border-left:5px solid #F39C12;
      border-radius:8px;
      padding:14px 18px;
      font-size:17px;
      font-weight:600;
    ",
    icon(icon_name, style="color:#F39C12;font-size:18px;"),
    tags$span(style="line-height:1.2;", title)
  )
}

#####################################################
#### PSA DOWNLOAD HEADER #############################
#####################################################
psaDownloadHeader <- function(title, icon_name, download_id){
  tags$div(
    style="
      display:flex;
      justify-content:space-between;
      align-items:center;
      margin-bottom:12px;
      gap:10px;
    ",
    tags$div(
      style="flex:1; min-width:0;",
      psaHeader(title, icon_name)
    ),
    tags$div(
      style="flex-shrink:0;",
      downloadButton(
        download_id,
        label = NULL,
        icon = icon("download"),
        class = "btn btn-default"
      )
    )
  )
}

#####################################################
#### SUMMARY CARD ####################################
#####################################################
psaSummaryCard <- function(output_id, background="#FFFFFF"){
  div(
    style=paste0("
      background:",background,";
      border-radius:14px;
      border:1px solid #E5E7EB;
      padding:22px;
      min-height:260px;
      display:flex;
      flex-direction:column;
      justify-content:center;
      box-shadow:0px 2px 10px rgba(0,0,0,0.06);
    "),
    uiOutput(output_id)
  )
}




PSAUI <- tabItem(
  tabName = "psa",
  
  #####################################################
  #### HEADER ##########################################
  #####################################################
  fluidRow(
    box(
      width = 12,
      solidHeader = FALSE,
      title = tags$div(
        style = "display:flex; align-items:center; gap:12px;",
        
        tags$div(
          style = "width:6px; height:20px; background:#F39C12; border-radius:3px;"
        ),
        
        tags$span(
          style = "font-size:18px; font-weight:600;",
          "Probabilistic Sensitivity Analysis"
        )
      ),
      
      p(
        style = "margin-top:8px;",
        icon("dice"),
        "The Probabilistic Sensitivity Analysis (PSA) evaluates parameter uncertainty by simultaneously sampling all uncertain model inputs over multiple simulations. Results are summarised using mean estimates, 95% confidence intervals and decision uncertainty metrics."
      )
    )
  ),
  #####################################################
  #### COST NOTE ######################################
  #####################################################
  fluidRow(
    column(
      12,
      uiOutput("cost_currency_note")
    )
  ),
  br(),
  #####################################################
  #### SUMMARY CARDS ###################################
  #####################################################
  fluidRow(
    column(3, psaSummaryCard("cardIncCost", background="#FFF7EF")),
    column(3, psaSummaryCard("cardIncQALY", background="#F2FFF6")),
    column(3, psaSummaryCard("cardICER", background="#F4F9FF")),
    column(3, psaSummaryCard("cardProbCE", background="#FAF5FF"))
  ),
  
  br(),
  
  #####################################################
  #### PSA SUMMARY TABLE ###############################
  #####################################################
  fluidRow(
    column(
      12,
      psaDownloadHeader(
        "PSA Summary Results",
        "table",
        "download_psa_summary"
      )
    )
  ),
  fluidRow(
    column(
      12,
      shinycssloaders::withSpinner(
        DTOutput("psaBaseCase"),
        type = 4
      ),
     # uiOutput("psaLYNote")
    )
  ),
  
  #####################################################
  #### COST RESULTS (TABLE ONLY) #######################
  #####################################################
  fluidRow(
    column(
      12,
      psaCard(
        psaDownloadHeader(
          "Disaggregated Costs",
          "sack-dollar",
          "download_psa_costs"
        ),
        DTOutput("psaCostBreakdown")
      )
    )
  ),
  #####################################################
  #### QALY RESULTS (TABLE ONLY) ######################
  #####################################################
  fluidRow(
    column(
      12,
      psaCard(
        psaDownloadHeader(
          tags$span(
            "Outcomes ",
            tags$span(
              style = "
      display:inline-flex;
      align-items:center;
      justify-content:center;
      width:22px;
      height:22px;
      border:1px solid #3C8DBC;
      border-radius:4px;
      cursor:pointer;
      color:#3C8DBC;
      margin-left:6px;
    ",
              title = paste0(
                "Methodology:\n\n",
                "evLY and HYT are anchored to the comparator QALY, which serves as the fixed baseline for both measures.\n\n",
                "Formulas:\n",
                "• evLY = QALY_comparator + 0.851 × (LY_intervention − LY_comparator)\n",
                "• HYT = QALY_intervention + (evLY_intervention − evLY_comparator)\n\n",
                "Key interpretation:\n",
                "The comparator QALY remains constant across both evLY and HYT calculations.\n",
                "Only incremental differences in life-years and QALYs drive variation in results."
              ),
              icon("eye", class = "fa-sm")
            )
          ),
          "person-walking",
          "download_psa_qaly"
        ),
        tags$div(
          style = "
          display:flex;
          align-items:center;
          justify-content:space-between;
          margin-bottom:8px;
          font-size:13px;
          color:#555;
        ",
          tags$span(
            "QALY outcomes are reported for both intervention and comparator across PSA iterations."
          )
        ),
        DTOutput("psaQALYBreakdown")
      )
    )
  ),
  #####################################################
  #### DECISION UNCERTAINTY ###########################
  #####################################################
  fluidRow(
    column(
      width = 6,
      psaCard(
        psaHeader(
          "Cost-Effectiveness Plane",
          "chart-scatter"
        ),
        br(),
        plotlyOutput(
          "cePlane",
          height = "420px"
        ),
      )
    ),
    column(
      width = 6,
      psaCard(
        psaHeader(
          "Cost-Effectiveness Acceptability Curve",
          "chart-line"
        ),
        br(),
        plotlyOutput("ceac", height = "420px")
      )
    )
  )
)

shinyBS::bsTooltip(
  id = "additional_outcomes_info",
  title = "evLY and HYT are anchored to comparator QALYs. Only incremental LY differences are applied, so comparator baseline remains fixed.",
  placement = "top",
  trigger = "hover"
)

tags$head(
  tags$style(HTML("
  
  table.dataTable{
      width:100% !important;
      border-collapse:collapse !important;
  }

  table.dataTable tbody td{
      text-align:center !important;
      vertical-align:middle !important;
      padding:12px 8px !important;
  }

  table.dataTable tbody td div{
      display:flex;
      flex-direction:column;
      justify-content:center;
      align-items:center;
      min-height:62px;
  }

  table.dataTable thead th{
      text-align:center !important;
      font-weight:600;
  }

  .dt-center{
      text-align:center !important;
  }

  "))
)