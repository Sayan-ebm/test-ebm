#========================================================
# SurvivalComparatorUI.R
#========================================================
SurvivalComparatorUI <- fluidPage(
  timeHorizonUI,
  #====================================================
  # HEADER
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "Survival Estimates - Comparator",
      status = "primary",
      solidHeader = TRUE,
      p("Estimate long-term survival for the comparator arm using:"),
      tags$ul(
        tags$li("Median OS/PFS estimates"),
        tags$li("Individual Patient Data (IPD; columns: time, event)"),
        tags$li("Digitized Kaplan-Meier data (Time, Surv, n_at_risk)")
      )
    )
  ),
  br(),
  #====================================================
  # BACKGROUND MORTALITY
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = tagList(
        "Population Settings",
        actionLink(
          "info_population",
          label = NULL,
          icon = icon("circle-info")
        )
      ),
      status = "danger",
      solidHeader = TRUE,
      p("Define population characteristics and background mortality assumptions used within the survival modelling framework."),
      checkboxInput(
        inputId = "apply_background_mortality",
        label = "Apply Background Mortality",
        value = FALSE
      ),
      fluidRow(
        column(
          3,
          numericInput(
            inputId = "starting_age",
            label = tagList(
              "Starting Age",
              actionButton(
                inputId = paste0("reset_", "starting_age"),
                label = NULL,
                icon = icon("rotate-left"),
                class = "btn-reset-icon-only",
                title = "Reset to default")
            ),
            value = 40,
            min = 0,
            max = 150
          )
        ),
        column(
          6,
          fileInput(
            inputId = "comp_background_mortality",
            label = "Upload Background Mortality CSV"
          ),
          verbatimTextOutput("background_mortality_source")
        ),
        column(
          3,
          br(),
          div(
            style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
            downloadButton(
              outputId = "download_background_mortality_example",
              label = "Example Background Mortality"
            ),
            actionButton(
              inputId = "reset_comp_background_mortality",
              label = "Reset to Default",
              icon = icon("undo"),
              class = "btn-reset-subtle"
            )
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # OVERALL SURVIVAL (OS)
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "Overall Survival",
      status = "info",
      solidHeader = TRUE,
      #------------------------------------------------
      # INPUT TYPE
      #------------------------------------------------
      fluidRow(
        column(
          4,
          selectInput(
            inputId = "comp_os_input_type",
            label = "OS Input Type",
            choices = c(
              "Median OS",
              "Survival Data"
            ),
            selected = "Median OS"
          )
        )
      ),
      #================================================
      # MEDIAN OS SECTION
      #================================================
      conditionalPanel(
        condition = "input.comp_os_input_type == 'Median OS'",
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "comp_median_os",
              label = tagList(
                "Median OS (Months)",
                actionButton(
                  inputId = paste0("reset_", "comp_median_os"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 30,
              min = 0
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Median OS Visualization",
            status = "primary",
            solidHeader = TRUE,
            plotly::plotlyOutput(
              outputId = "comp_median_os_plot",
              height = "500px",
              width = "100%"
            ),
            br(),
            downloadButton(
              outputId = "download_comp_median_os_predictions",
              label = "Download OS Predictions CSV"
            )
          )
        )
      ),
      #================================================
      # SURVIVAL DATA SECTION
      #================================================
      conditionalPanel(
        condition = "input.comp_os_input_type == 'Survival Data'",
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "comp_os_survival_type",
              label = "Survival Data Type",
              choices = c(
                "IPD",
                "Digitized KM Curve"
              )
            )
          )
        ),
      ),
      #================================================
      # OS IPD UPLOAD
      #================================================
      conditionalPanel(
        condition = "
        input.comp_os_input_type == 'Survival Data' &&
        input.comp_os_survival_type == 'IPD'
      ",
        fluidRow(
          column(
            8,
            tags$label(
              "Upload OS IPD CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload a CSV containing one row per patient.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "time = follow-up time (months)<br>",
                  "status = event indicator (1 = event, 0 = censored)<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>time,status
                    0,1
                    6,0
                    12,1</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              inputId = "comp_os_ipd",
              label = NULL
            ),
            verbatimTextOutput("comp_os_ipd_source")
          ),
          column(
            4,
            br(),
            div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
              downloadButton(
                outputId = "download_comp_os_ipd_example",
                label = "Example OS IPD"
              ),
              actionButton(
                inputId = "reset_comp_os_ipd",
                label = "Reset to Default",
                icon = icon("undo"),
                class = "btn-reset-subtle"
              )
            )
          )
        ),
      ),
      #================================================
      # OS KM CURVE UPLOAD
      #================================================
      conditionalPanel(
        condition = "
  input.comp_os_input_type == 'Survival Data' &&
  input.comp_os_survival_type == 'Digitized KM Curve'
  ",
        fluidRow(
          column(
            6,
            tags$label(
              "Upload OS Survival Probability CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload digitized Kaplan-Meier survival data.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "Time = follow-up time (months)<br>",
                  "Surv = survival probability (0–1)<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>Time,Surv
                  0,1.00
                  6,0.85
                  12,0.72</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              "comp_os_km_survival",
              label = NULL
            ),
            verbatimTextOutput("comp_os_survival_source"),
            downloadButton(
              "download_comp_os_survival_example",
              "Example Survival CSV",
              width = "100%"
            )
          ),
          column(
            6,
            tags$label(
              "Upload OS Number At Risk CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload numbers at risk corresponding to the KM curve.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "Time = follow-up time (months)<br>",
                  "n_at_risk = number at risk<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>Time,n_at_risk
                  0,250
                  6,180
                  12,120</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              "comp_os_km_risk",
              label = NULL
            ),
            verbatimTextOutput("comp_os_risk_source"),
            downloadButton(
              "download_comp_os_risk_example",
              "Example Risk CSV",
              width = "100%"
            )
          )
        ),
        br(),
        br(),
        fluidRow(
          column(
            8,
            div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
              downloadButton(
                "download_comp_os_reconstructed_ipd",
                "Download Reconstructed OS IPD"
              ),
              actionButton(
                inputId = "reset_comp_os_km",
                label = "Reset to Default",
                icon = icon("undo"),
                class = "btn-reset-subtle"
              )
            )
          )
        ),
        br(),
      ),
      #================================================
      # GOODNESS OF FIT + PLOT
      #================================================
      conditionalPanel(
        condition = "input.comp_os_input_type == 'Survival Data'",
        br(),
        fluidRow(
          #--------------------------------------------
          # AIC TABLE
          #--------------------------------------------
          column(
            width = 4,
            box(
              width = 12,
              title = tagList(
                "Goodness of Fit Table",
                actionLink(
                  "info_os_gof",
                  label = NULL,
                  icon = icon("circle-info")
                )
              ),
              status = "warning",
              solidHeader = TRUE,
              textOutput("comp_os_active_model"),
              tags$style(HTML(".box.box-warning .box-header .box-title {font-size: 20px !important;font-weight: bold;}")),
              div(
                style = "overflow-y:auto; max-height:500px; font-size: 18px; line-height: 1.6;",
                DT::DTOutput("comp_os_aic_table")
              ),
              downloadButton(
                "download_comp_os_aic_table",
                "Download CSV",
                width = "100%"
              ),
              br(),
              br(),
              downloadButton(
                "download_comp_os_model_output",
                "Download Parameters + Predictions",
                width = "100%"
              )
            )
          ),
          #--------------------------------------------
          # PLOT
          #--------------------------------------------
          column(
            width = 8,
            box(
              width = 12,
              title = "Parametric Distribution Fits",
              status = "success",
              solidHeader = TRUE,
              plotly::plotlyOutput(
                outputId = "comp_os_fit_plot",
                height = "500px"
              )
            )
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # PROGRESSION FREE SURVIVAL (PFS)
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "Progression Free Survival",
      status = "info",
      solidHeader = TRUE,
      #------------------------------------------------
      # INPUT TYPE
      #------------------------------------------------
      fluidRow(
        column(
          4,
          selectInput(
            inputId = "comp_pfs_input_type",
            label = "PFS Input Type",
            choices = c(
              "Median PFS",
              "Survival Data"
            ),
            selected = "Median PFS"
          )
        )
      ),
      #================================================
      # MEDIAN PFS SECTION
      #================================================
      conditionalPanel(
        condition = "input.comp_pfs_input_type == 'Median PFS'",
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "comp_median_pfs",
              label = tagList(
                "Median PFS (Months)",
                actionButton(
                  inputId = paste0("reset_", "comp_median_pfs"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 18,
              min = 0
            )
          ),
        ),
        br(),
        fluidRow(
          box(
            width = 12,
            title = "Median PFS Visualization",
            status = "primary",
            solidHeader = TRUE,
            plotly::plotlyOutput(
              outputId = "comp_median_pfs_plot",
              height = "500px",
              width = "100%"
            ),
            br(),
            downloadButton(
              outputId = "download_comp_median_pfs_predictions",
              label = "Download PFS Predictions CSV"
            )
          )
        )
      ),
      #================================================
      # SURVIVAL DATA SECTION
      #================================================
      conditionalPanel(
        condition = "input.comp_pfs_input_type == 'Survival Data'",
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "comp_pfs_survival_type",
              label = "Survival Data Type",
              choices = c(
                "IPD",
                "Digitized KM Curve"
              )
            )
          )
        ),
      ),
      #================================================
      # PFS IPD UPLOAD
      #================================================
      conditionalPanel(
        condition = "
          input.comp_pfs_input_type == 'Survival Data' &&
          input.comp_pfs_survival_type == 'IPD'
        ",
        fluidRow(
          column(
            8,
            tags$label(
              "Upload PFS IPD CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload a CSV containing one row per patient.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "time = follow-up time (months)<br>",
                  "status = event indicator (1 = event, 0 = censored)<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>time,status
                  0,1
                  6,0
                  12,1</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              inputId = "comp_pfs_ipd",
              label = NULL
            ),
            verbatimTextOutput("comp_pfs_ipd_source")
          ),
          column(
            4,
            br(),
            div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
              downloadButton(
                outputId = "download_comp_pfs_ipd_example",
                label = "Example PFS IPD"
              ),
              actionButton(
                inputId = "reset_comp_pfs_ipd",
                label = "Reset to Default",
                icon = icon("undo"),
                class = "btn-reset-subtle"
              )
            )
          )
        )
      ),
      #================================================
      # PFS KM CURVE UPLOAD
      #================================================
      conditionalPanel(
        condition = "
          input.comp_pfs_input_type == 'Survival Data' &&
          input.comp_pfs_survival_type == 'Digitized KM Curve'
        ",
        fluidRow(
          column(
            6,
            tags$label(
              "Upload PFS Survival Probability CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload digitized Kaplan-Meier survival data.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "Time = follow-up time (months)<br>",
                  "Surv = survival probability (0–1)<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>Time,Surv
                    0,1.00
                    6,0.85
                    12,0.72</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              "comp_pfs_km_survival",
              label = NULL
            ),
            verbatimTextOutput("comp_pfs_survival_source"),
            downloadButton(
              "download_comp_pfs_prob_survival",
              "Example Survival CSV",
              width = "100%"
            )
          ),
          column(
            6,
            tags$label(
              "Upload PFS Number At Risk CSV ",
              tags$i(
                class = "fa fa-comment-dots",
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                `data-html` = "true",
                title = paste(
                  "Upload numbers at risk corresponding to the KM curve.",
                  "<br><br>",
                  "<b>Required columns:</b><br>",
                  "Time = follow-up time (months)<br>",
                  "n_at_risk = number at risk<br><br>",
                  "<b>Example:</b><br>",
                  "<pre>Time,n_at_risk
                  0,250
                  6,180
                  12,120</pre>",
                  sep = ""
                ),
                style = "color:lightblue; cursor:pointer; margin-left:5px;"
              )
            ),
            fileInput(
              "comp_pfs_km_risk",
              label = NULL
            ),
            verbatimTextOutput("comp_pfs_risk_source"),
            downloadButton(
              "download_comp_pfs_natrisk",
              "Example Risk CSV",
              width = "100%"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            8,
            div(
              style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
              downloadButton(
                "download_comp_pfs_reconstructed_ipd",
                "Download Reconstructed PFS IPD"
              ),
              actionButton(
                inputId = "reset_comp_pfs_km",
                label = "Reset to Default",
                icon = icon("undo"),
                class = "btn-reset-subtle"
              )
            )
          )
        )
      ),
      #================================================
      # GOODNESS OF FIT + PLOT
      #================================================
      conditionalPanel(
        condition = "input.comp_pfs_input_type == 'Survival Data'",
        br(),
        fluidRow(
          #--------------------------------------------
          # AIC TABLE
          #--------------------------------------------
          column(
            width = 4,
            box(
              width = 12,
              title = tagList(
                "Goodness of Fit Table",
                actionLink(
                  "info_pfs_gof",
                  label = NULL,
                  icon = icon("circle-info")
                )
              ),
              status = "warning",
              solidHeader = TRUE,
              textOutput("comp_pfs_active_model"),
              tags$style(HTML(".box.box-warning .box-header .box-title {font-size: 20px !important;font-weight: bold;}")),
              div(
                style = "overflow-y:auto; max-height:500px; font-size: 18px; line-height: 1.6;",
                DT::DTOutput("comp_pfs_aic_table")
              ),
              downloadButton("download_comp_pfs_aic", "Download CSV"),
              br(),
              br(),
              downloadButton(
                "download_comp_pfs_model_output",
                "Download Parameters + Predictions",
                width = "100%"
              )
            )
          ),
          #--------------------------------------------
          # PLOT
          #--------------------------------------------
          column(
            width = 8,
            box(
              width = 12,
              title = "Parametric Distribution Fits",
              status = "success",
              solidHeader = TRUE,
              plotly::plotlyOutput(
                outputId = "comp_pfs_fit_plot",
                height = "500px"
              )
            )
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # LONG-TERM VALIDATION
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = tagList(
        "Long-Term Validation",
        actionLink(
          "info_longterm_validation",
          label = NULL,
          icon = icon("circle-info")
        )
      ),
      status = "warning",
      solidHeader = TRUE,
      fluidRow(
        #------------------------------------------------
        # LONG-TERM OS
        #------------------------------------------------
        column(
          6,
          box(
            width = 12,
            title =  "Long-Term OS Validation",
            status = "primary",
            solidHeader = TRUE,
            numericInput(
              inputId = "comp_longterm_os_prob",
              label = tagList(
                "OS Survival Probability",
                actionButton(
                  inputId = paste0("reset_", "comp_longterm_os_prob"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 0.05,
              min = 0,
              max = 1
            ),
            numericInput(
              inputId = "comp_longterm_os_time",
              label = tagList(
                "Time (Months)",
                actionButton(
                  inputId = paste0("reset_", "comp_longterm_os_time"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 60,
              min = 0
            ),
            fluidRow(
              column(
                8,
                fileInput(
                  inputId = "comp_longterm_os_file",
                  label = "Optional Long-Term OS CSV"
                )
              ),
              column(
                4,
                br(),
                downloadButton(
                  outputId = "download_comp_longterm_os_example",
                  label = "Example CSV",
                  width = "100%"
                )
              )
            )
          )
        ),
        #------------------------------------------------
        # LONG-TERM PFS
        #------------------------------------------------
        column(
          6,
          box(
            width = 12,
            title = "Long-Term PFS Validation",
            status = "primary",
            solidHeader = TRUE,
            numericInput(
              inputId = "comp_longterm_pfs_prob",
              label = tagList(
                "PFS Survival Probability",
                actionButton(
                  inputId = paste0("reset_", "comp_longterm_pfs_prob"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 0.02,
              min = 0,
              max = 1
            ),
            numericInput(
              inputId = "comp_longterm_pfs_time",
              label = tagList(
                "Time (Months)",
                actionButton(
                  inputId = paste0("reset_", "comp_longterm_pfs_time"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 60,
              min = 0
            ),
            fluidRow(
              column(
                8,
                fileInput(
                  inputId = "comp_longterm_pfs_file",
                  label = "Optional Long-Term PFS CSV"
                )
              ),
              column(
                4,
                br(),
                downloadButton(
                  outputId = "download_comp_longterm_pfs_example",
                  label = "Example CSV",
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # STATE OCCUPANCY
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "State Occupancy",
      status = "success",
      solidHeader = TRUE,
      p(
        "Download the estimated state occupancy results for the comparator arm. ",
        "The output includes the proportion of patients in progression-free survival (PFS), ",
        "post-progression survival (PPS), and death over the model time horizon."
      ),
      br(),
      downloadButton(
        outputId = "download_comp_state_occupancy",
        label = "Download CSV File",
        width = "250px"
      )
    )
  ),
  br()
)

tags$script(HTML("
$(function () {
  $('[data-toggle=\"tooltip\"]').tooltip({
    html: true,
    container: 'body'
  });
});
"))