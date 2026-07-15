ResultsUI <- fluidPage(
  #####################################################
  #### HEADER #########################################
  #####################################################
  fluidRow(
    box(
      width = 12,
      title = "Base Case Results",
      status = "success",
      solidHeader = TRUE,
      p(
        icon("calculator"),
        "The Base Case Results section presents the primary outcomes of the economic evaluation using the default model inputs and assumptions. Results include total costs, health outcomes, incremental outcomes, ICERs and net monetary benefits."
      )
    )
  ),
  uiOutput("currency_note"),
  br(),
  #####################################################
  #### KPI SUMMARY ####################################
  #####################################################
  fluidRow(
    valueBoxOutput("incremental_cost_box", width = 3),
    valueBoxOutput("incremental_qaly_box", width = 3),
    valueBoxOutput("icer_box", width = 3),
    valueBoxOutput("nmb_box", width = 3)
  ),
  br(),
  #####################################################
  #### BASE CASE RESULTS ##############################
  #####################################################
  fluidRow(
    column(
      8,
      tags$div(
        style = "
        background:#EAF2FF;
        padding:10px;
        border-left:5px solid #3C8DBC;
        border-radius:6px;
        font-weight:600;
        font-size:16px;
      ",
        icon("table"),
        " Base Case Results"
      )
    ),
    column(
      4,
      align = "right",
      downloadButton(
        "download_basecase",
        "",
        icon = icon("download")
      )
    )
  ),
  br(),
  shinycssloaders::withSpinner(
    DT::DTOutput("basecase_table"),
    type = 4
  ),
  br(),
  fluidRow(
    column(
      8,
      tags$div(
        style = "
        background:#EAF2FF;
        padding:10px;
        border-left:5px solid #3C8DBC;
        border-radius:6px;
        font-weight:600;
        font-size:16px;
      ",
       # icon("sterling-sign"),
        " Economically Justifiable Price"
      )
    ),
    column(
      4,
      align = "right",
      downloadButton(
        "download_ejp",
        "",
        icon = icon("download")
      )
    )
  ),
  br(),
  DT::DTOutput("ejp_table"),
  uiOutput("ejp_note"),
  # #####################################################
  # #### ADDITIONAL ECONOMIC OUTCOMES ####################
  # #####################################################
  br(),
  fluidRow(
    column(
      8,
      tags$div(
        style = "
        background:#EAF2FF;
        padding:10px;
        border-left:5px solid #3C8DBC;
        border-radius:6px;
        font-weight:600;
        font-size:16px;
        display:flex;
        align-items:center;
        gap:10px;
      ",
        icon("chart-line"),
        "Additional Economic Outcomes",
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
      )
    ),
    column(
      4,
      align = "right",
      downloadButton(
        "download_additional_outcomes",
        "",
        icon = icon("download")
      )
    )
  ),
  br(),
  DT::DTOutput("additional_outcomes_table"),
  #####################################################
  #### COSTS ##########################################
  #####################################################
  fluidRow(
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        fluidRow(
          column(
            8,
            tags$div(
              style = "
                background:#EAF2FF;
                padding:10px;
                border-left:5px solid #3C8DBC;
                border-radius:6px;
                font-weight:600;
                font-size:16px;
              ",
              icon("sack-dollar"),
              " Disaggregated Costs"
            )
          ),
          column(
            4,
            align = "right",
            downloadButton(
              "download_costs",
              "",
              icon = icon("download")
            )
          )
        ),
        br(),
        DT::DTOutput("cost_breakdown_table")
      )
    ),
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        tags$div(
          style = "
            background:#EAF2FF;
            padding:10px;
            border-left:5px solid #3C8DBC;
            border-radius:6px;
            margin-bottom:15px;
            font-weight:600;
            font-size:16px;
          ",
          
          icon("chart-pie"),
          " Cost Breakdown"
        ),
        plotlyOutput(
          "cost_breakdown_plot",
          height = "400px"
        )
      )
    )
  ),
  #####################################################
  #### LIFE YEARS #####################################
  #####################################################
  fluidRow(
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        fluidRow(
          column(
            8,
            tags$div(
              style = "
                background:#EAF2FF;
                padding:10px;
                border-left:5px solid #3C8DBC;
                border-radius:6px;
                font-weight:600;
                font-size:16px;
              ",
              icon("heartbeat"),
              " Disaggregated Life Years"
            )
          ),
          column(
            4,
            align = "right",
            
            downloadButton(
              "download_lys",
              "",
              icon = icon("download")
            )
          )
        ),
        br(),
        div(
          style = "min-height:400px;",
          DT::DTOutput("ly_breakdown_table")
        )
      )
    ),
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        tags$div(
          style = "
            background:#EAF2FF;
            padding:10px;
            border-left:5px solid #3C8DBC;
            border-radius:6px;
            margin-bottom:15px;
            font-weight:600;
            font-size:16px;
          ",
          icon("chart-bar"),
          " Life Year Breakdown"
        ),
        plotlyOutput(
          "ly_breakdown_plot",
          height = "400px"
        )
      )
    )
  ),
  #####################################################
  #### QALYS ##########################################
  #####################################################
  fluidRow(
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        fluidRow(
          column(
            8,
            tags$div(
              style = "
                background:#EAF2FF;
                padding:10px;
                border-left:5px solid #3C8DBC;
                border-radius:6px;
                font-weight:600;
                font-size:16px;
              ",
              icon("person-walking"),
              " Disaggregated QALYs"
            )
          ),
          column(
            4,
            align = "right",
            downloadButton(
              "download_qalys",
              "",
              icon = icon("download")
            )
          )
        ),
        br(),
        div(
          style = "min-height:400px;",
          DT::DTOutput("qaly_breakdown_table")
        )
      )
    ),
    column(
      width = 6,
      div(
        style = "
          background:white;
          border:1px solid #DDE4EA;
          border-radius:8px;
          padding:18px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.05);
          margin-top:20px;
        ",
        tags$div(
          style = "
            background:#EAF2FF;
            padding:10px;
            border-left:5px solid #3C8DBC;
            border-radius:6px;
            margin-bottom:15px;
            font-weight:600;
            font-size:16px;
          ",
          icon("chart-column"),
          " QALY Breakdown"
        ),
        plotlyOutput(
          "qaly_breakdown_plot",
          height = "400px"
        )
      )
    )
  ),
)

shinyBS::bsTooltip(
  id = "additional_outcomes_info",
  title = "evLY and HYT are anchored to comparator QALYs. Only incremental LY differences are applied, so comparator baseline remains fixed.",
  placement = "top",
  trigger = "hover"
)

