#====================================================
# THERAPY COST BLOCK
#====================================================
therapyCostUI <- function(
    prefix,
    therapy_no,
    default_name,
    default_cost,
    accent_colour = "#3C8DBC"
) {
  div(
    style = "
      background:white;
      border:1px solid #DDE4EA;
      border-radius:8px;
      padding:18px;
      margin-bottom:20px;
      box-shadow:0px 1px 3px rgba(0,0,0,0.05);
    ",
    #================================================
    # THERAPY HEADER
    #================================================
    tags$div(
      style = paste0(        "
        background:#F4F8FC;
        padding:12px;
        border-radius:6px;
        margin-bottom:15px;
        border-left:5px solid ",
                             accent_colour,
                             ";
      "
      ),
      tags$h4(
        style = "margin:0;font-weight:600;",
        icon("capsules"),
        paste("Therapy", therapy_no)
      )
    ),
    #================================================
    # BASIC TREATMENT DETAILS
    #================================================
    fluidRow(
      column(
        6,
        textInput(
          inputId = paste0("Tx_name_", prefix, therapy_no),
          label = tagList(
            icon("capsules"),
            " Therapy Name"
          ),
          value = default_name
        )
      ),
      column(
        6,
        uiOutput(
          outputId = paste0("Tx_cost_ui_", prefix, therapy_no)
        )
      )
    ),
    hr(),
    #================================================
    # TREATMENT DURATION
    #================================================
    tags$div(
      style = paste0(
        "
        background-color:#EAF2FF;
        padding:8px;
        border-left:4px solid ",
        accent_colour,
        ";
        margin-bottom:15px;
        margin-top:10px;
        font-weight:600;
        font-size:16px;
      "
      ),
      icon("calendar-alt"),
      " Treatment Duration"
    ),
    fluidRow(
      column(
        6,
        numericInput(
          inputId = paste0("Tx_cycle_length_", prefix, therapy_no),
          label = tagList(
            icon("clock"),
            " Cycle Length (Weeks) ",
            actionLink(
              inputId = paste0(
                "info_cycle_length_",
                prefix,
                "_",
                therapy_no
              ),
              label = NULL,
              icon = icon("circle-info")
            )
          ),
          value = 4,
          min = 1
        )
      ),
      column(
        6,
        selectInput(
          inputId = paste0("Tx_max_cycles_flag_", prefix, therapy_no),
          label = tagList(
            "Treatment Stops After Fixed Number of Cycles? ",
            actionLink(
              inputId = paste0(
                "info_treatment_stop_",
                prefix,
                "_",
                therapy_no
              ),
              label = NULL,
              icon = icon("circle-info")
            )
          ),
          choices = c("No", "Yes"),
          selected = "Yes"
        )
      )
    ),
    conditionalPanel(
      condition = paste0(
        "input.Tx_max_cycles_flag_",
        prefix,
        therapy_no,
        " == 'Yes'"
      ),
      fluidRow(
        column(
          6,
          numericInput(
            inputId = paste0("Tx_max_cycles_", prefix, therapy_no),
            label = tagList(
              icon("repeat"),
              " Maximum Treatment Cycles"
            ),
            value = 6,
            min = 1
          ),
          helpText(
            "Treatment will stop after the specified number of cycles."
          )
        )
      )
    ),
    hr(),
    #================================================
    # COST ADJUSTMENTS
    #================================================
    tags$div(
      style = "
        background-color:#FFF4E5;
        padding:8px;
        border-left:4px solid #F39C12;
        margin-bottom:15px;
        margin-top:10px;
        font-weight:600;
        font-size:16px;
      ",
      icon("coins"),
      " Cost Adjustments"
    ),
    fluidRow(
      column(
        6,
        selectInput(
          inputId = paste0("Tx_cost_change_flag_", prefix, therapy_no),
          label = "Does Treatment Cost Change?",
          choices = c("No", "Yes"),
          selected = "No"
        )
      ),
      column(
        6,
        numericInput(
          inputId = paste0("Tx_discount_", prefix, therapy_no),
          label = tagList(
            icon("percent"),
            " Discount (%)"
          ),
          value = 0,
          min = 0,
          max = 100,
          step = 1
        ),
        helpText(
          "Applied to the cycle cost before treatment costs are accumulated."
        )
      )
    ),
    #================================================
    # COST CHANGE DETAILS
    #================================================
    conditionalPanel(
      condition = paste0(
        "input.Tx_cost_change_flag_",
        prefix,
        therapy_no,
        " == 'Yes'"
      ),
      div(
        style = "
          background:#FFF7E6;
          border-left:5px solid #F39C12;
          border-radius:6px;
          padding:15px;
          margin-top:10px;
          box-shadow:0px 1px 3px rgba(0,0,0,0.08);
        ",
        tags$h5(
          style = "
            color:#B9770E;
            font-weight:600;
            margin-top:0;
            margin-bottom:15px;
          ",
          icon("exclamation-circle"),
          " Cost Change Details"
        ),
        fluidRow(
          column(
            6,
            numericInput(
              inputId = paste0(
                "Tx_cost_change_cycle_",
                prefix,
                therapy_no
              ),
              label = "Cost Changes Starting from Cycle",
              value = 7,
              min = 1
            )
          ),
          column(
            6,
            numericInput(
              inputId = paste0(
                "Tx_cost_after_",
                prefix,
                therapy_no
              ),
              label = "Cost per Cycle After Change",
              value = default_cost,
              min = 0
            )
          )
        )
      )
    )
  )
}

