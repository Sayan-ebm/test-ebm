#========================================================
# SurvivalInterventionUI.R
#========================================================
SurvivalInterventionUI <- fluidPage(
  #timeHorizonUI,
  #====================================================
  # HEADER
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "Survival Estimates - Intervention",
      status = "primary",
      solidHeader = TRUE,
      p("Estimate long-term survival for the comparator arm using:"),
      tags$ul(
        tags$li("Median OS/PFS estimates"),
        tags$li("Hazard Ratio (applied to selected Comparator survival model)"),
        tags$li("Individual Patient Data (IPD; columns: time, event)"),
        tags$li("Digitized Kaplan-Meier data (Time, Surv, n_at_risk)")
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
            inputId = "int_os_input_type",
            label = "OS Input Type",
            choices = c(
              "Median OS",
              "Survival Data",
              "Hazard Ratio"
            ),
            selected = "Median OS"
          )
        )
      ),
      #================================================
      # MEDIAN OS SECTION
      #================================================
      conditionalPanel(
        condition = "input.int_os_input_type == 'Median OS'",
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "int_median_os",
              label = tagList(
                "Median OS (Months)",
                actionButton(
                  inputId = paste0("reset_", "int_median_os"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 50,
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
              outputId = "int_median_os_plot",
              height = "500px",
              width = "100%"
            ),
            br(),
            downloadButton(
              outputId = "download_int_median_os_predictions",
              label = "Download OS Predictions CSV"
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.int_os_input_type == 'Hazard Ratio'",
        fluidRow(
          column(
            5,
            numericInput(
              inputId = "int_hr_os",
              label = tagList(
                "OS Hazard Ratio",
                actionButton(
                  inputId = paste0("reset_", "int_hr_os"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 0.5,
              min = 0
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "OS Hazard Ratio Projection",
            plotly::plotlyOutput(
              "int_hr_os_plot",
              height = "500px"
            )
          )
        )
      ),
      #================================================
      # SURVIVAL DATA SECTION
      #================================================
      conditionalPanel(
        condition = "input.int_os_input_type == 'Survival Data'",
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "int_os_survival_type",
              label = "Survival Data Type",
              choices = c(
                "IPD",
                "Digitized KM Curve"
              )
            )
          )
        ),
        #================================================
        # OS IPD UPLOAD
        #================================================
        conditionalPanel(
          condition = "
          input.int_os_input_type == 'Survival Data' &&
          input.int_os_survival_type == 'IPD'
        ",
          fluidRow(
            column(
              6,
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
                    "<pre>time,status\n0,1\n6,0\n12,1</pre>",
                    sep = ""
                  ),
                  style = "color:lightblue; cursor:pointer; margin-left:5px;"
                )
              ),
              fileInput(
                inputId = "int_os_ipd",
                label = NULL
              ),
              verbatimTextOutput(
                "int_os_ipd_source"
              ),
              div(
                style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
                downloadButton(
                  outputId = "download_os_ipd_example",
                  label = "Example OS IPD"
                ),
                actionButton(
                  inputId = "reset_int_os_ipd",
                  label = "Reset to Default",
                  icon = icon("undo"),
                  class = "btn-reset-subtle"
                )
              )
            )
          )
        ),
        #================================================
        # OS KM CURVE UPLOAD
        #================================================
        conditionalPanel(
          condition = "
        input.int_os_input_type == 'Survival Data' &&
        input.int_os_survival_type == 'Digitized KM Curve'
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
                    "<pre>Time,Surv\n0,1.00\n6,0.85\n12,0.72</pre>",
                    sep = ""
                  ),
                  style = "color:lightblue; cursor:pointer; margin-left:5px;"
                )
              ),
              fileInput(
                "int_os_km_survival",
                NULL
              ),
              verbatimTextOutput(
                "int_os_survival_source"
              ),
              downloadButton(
                "download_os_survival_example",
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
                    "<pre>Time,n_at_risk\n0,250\n6,180\n12,120</pre>",
                    sep = ""
                  ),
                  style = "color:lightblue; cursor:pointer; margin-left:5px;"
                )
              ),
              fileInput(
                "int_os_km_risk",
                NULL
              ),
              verbatimTextOutput(
                "int_os_risk_source"
              ),
              div(
                style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
                downloadButton(
                  "download_os_risk_example",
                  "Example Risk CSV",
                  width = "100%"
                ),
                actionButton(
                  inputId = "reset_int_os_km",
                  label = "Reset to Default",
                  icon = icon("undo"),
                  class = "btn-reset-subtle"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              downloadButton(
                "download_os_reconstructed_ipd",
                "Download Reconstructed OS IPD",
                width = "100%"
              )
            )
          )
        ),
        #================================================
        # GOODNESS OF FIT + PLOT
        #================================================
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
                  "info_os_gof_int",
                  label = NULL,
                  icon = icon("circle-info")
                )
              ),
              status = "warning",
              solidHeader = TRUE,
              textOutput("int_os_active_model"),
              tags$style(HTML(".box.box-warning .box-header .box-title {font-size: 20px !important;font-weight: bold;}")),
              div(
                style = "overflow-y:auto; max-height:500px; font-size: 18px; line-height: 1.6;",
                DT::DTOutput("int_os_aic_table")
              ),
              downloadButton(
                "download_int_os_aic_table",
                "Download CSV",
                width = "100%"
              ),
              br(),
              br(),
              downloadButton(
                outputId = "download_int_os_model_output",
                label = "Download Parameters + Predictions",
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
                outputId = "int_os_fit_plot",
                height = "500px"
              )
            )
          )
        )
      ) # end OS 'Survival Data' conditionalPanel
    )   # end OS box
  ),   # end OS fluidRow
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
            inputId = "int_pfs_input_type",
            label = "PFS Input Type",
            choices = c(
              "Median PFS",
              "Survival Data",
              "Hazard Ratio"
            ),
            selected = "Median PFS"
          )
        )
      ),
      #================================================
      # MEDIAN PFS SECTION
      #================================================
      conditionalPanel(
        condition = "input.int_pfs_input_type == 'Median PFS'",
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "int_median_pfs",
              label = tagList(
                "Median PFS (Months)",
                actionButton(
                  inputId = paste0("reset_", "int_median_pfs"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 25,
              min = 0
            )
          )
        ),
        br(),
        fluidRow(
          box(
            width = 12,
            title = "Median PFS Visualization",
            status = "primary",
            solidHeader = TRUE,
            plotly::plotlyOutput(
              outputId = "int_median_pfs_plot",
              height = "500px",
              width = "100%"
            ),
            br(),
            downloadButton(
              outputId = "download_int_median_pfs_predictions",
              label = "Download PFS Predictions CSV"
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.int_pfs_input_type == 'Hazard Ratio'",
        fluidRow(
          column(
            5,
            numericInput(
              inputId = "int_hr_pfs",
              label = tagList(
                "PFS Hazard Ratio",
                actionButton(
                  inputId = paste0("reset_", "int_hr_pfs"),
                  label = NULL,
                  icon = icon("rotate-left"),
                  class = "btn-reset-icon-only",
                  title = "Reset to default")
              ),
              value = 0.5,
              min = 0
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "PFS Hazard Ratio Projection",
            plotly::plotlyOutput(
              "int_hr_pfs_plot",
              height = "500px"
            )
          )
        )
      ),
      #================================================
      # SURVIVAL DATA SECTION
      #================================================
      conditionalPanel(
        condition = "input.int_pfs_input_type == 'Survival Data'",
        fluidRow(
          column(
            4,
            radioButtons(
              inputId = "int_pfs_survival_type",
              label = "Survival Data Type",
              choices = c(
                "IPD",
                "Digitized KM Curve"
              )
            )
          )
        ),
        #================================================
        # PFS IPD UPLOAD
        #================================================
        conditionalPanel(
          condition = "
          input.int_pfs_input_type == 'Survival Data' &&
          input.int_pfs_survival_type == 'IPD'
        ",
          fluidRow(
            column(
              6,
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
                inputId = "int_pfs_ipd",
                label = NULL
              ),
              verbatimTextOutput(
                "int_pfs_ipd_source"
              ),
              div(
                style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
                downloadButton(
                  outputId = "download_pfs_ipd_example",
                  label = "Example PFS IPD"
                ),
                actionButton(
                  inputId = "reset_int_pfs_ipd",
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
        input.int_pfs_input_type == 'Survival Data' &&
        input.int_pfs_survival_type == 'Digitized KM Curve'
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
                "int_pfs_km_survival",
                NULL
              ),
              verbatimTextOutput(
                "int_pfs_survival_source"
              ),
              downloadButton(
                "download_pfs_prob_survival",
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
                "int_pfs_km_risk",
                NULL
              ),
              verbatimTextOutput(
                "int_pfs_risk_source"
              ),
              div(
                style = "display:flex; align-items:center; gap:8px; flex-wrap:wrap;",
                downloadButton(
                  "download_pfs_natrisk",
                  "Example Risk CSV",
                  width = "100%"
                ),
                actionButton(
                  inputId = "reset_int_pfs_km",
                  label = "Reset to Default",
                  icon = icon("undo"),
                  class = "btn-reset-subtle"
                )
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              downloadButton(
                "download_pfs_reconstructed_ipd",
                "Download Reconstructed PFS IPD",
                width = "100%"
              )
            )
          )
        ),
        #================================================
        # GOODNESS OF FIT + PLOT
        #================================================
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
                  "info_pfs_gof_int",
                  label = NULL,
                  icon = icon("circle-info")
                )
              ),
              status = "warning",
              solidHeader = TRUE,
              textOutput("int_pfs_active_model"),
              tags$style(HTML(".box.box-warning .box-header .box-title {font-size: 20px !important;font-weight: bold;}")),
              div(
                style = "overflow-y:auto; max-height:500px; font-size: 18px; line-height: 1.6;",
                DT::DTOutput("int_pfs_aic_table")
              ),
              downloadButton("download_int_pfs_aic", "Download CSV"),
              br(),
              br(),
              downloadButton(
                outputId = "download_int_pfs_model_output",
                label = "Download Parameters + Predictions",
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
                outputId = "int_pfs_fit_plot",
                height = "500px"
              )
            )
          )
        )
      ) # end PFS 'Survival Data' conditionalPanel
    )   # end PFS box
  ),   # end PFS fluidRow
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
          "info_longterm_validation_int",
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
            title = "Long-Term OS Validation",
            status = "primary",
            solidHeader = TRUE,
            numericInput(
              inputId = "int_longterm_os_prob",
              label = tagList(
                "OS Survival Probability",
                actionButton(
                  inputId = paste0("reset_", "int_longterm_os_prob"),
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
              inputId = "int_longterm_os_time",
              label = tagList(
                "Time (Months)",
                actionButton(
                  inputId = paste0("reset_", "int_longterm_os_time"),
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
                  inputId = "int_longterm_os_file",
                  label = "Optional Long-Term OS CSV"
                )
              ),
              column(
                4,
                br(),
                downloadButton(
                  outputId = "download_longterm_os_example",
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
              inputId = "int_longterm_pfs_prob",
              label = tagList(
                "PFS Survival Probability",
                actionButton(
                  inputId = paste0("reset_", "int_longterm_pfs_prob"),
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
              inputId = "int_longterm_pfs_time",
              label = tagList(
                "Time (Months)",
                actionButton(
                  inputId = paste0("reset_", "int_longterm_pfs_time"),
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
                  inputId = "int_longterm_pfs_file",
                  label = "Optional Long-Term PFS CSV"
                )
              ),
              column(
                4,
                br(),
                downloadButton(
                  outputId = "download_longterm_pfs_example",
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
  # BACKGROUND MORTALITY
  #====================================================
  # fluidRow(
  #   box(
  #     width = 12,
  #     title = "Background Mortality",
  #     status = "danger",
  #     solidHeader = TRUE,
  #     p("Upload background mortality file for age-adjusted mortality estimation."),
  #     fluidRow(
  #       column(
  #         8,
  #         fileInput(
  #           inputId = "int_background_mortality",
  #           label = "Upload Background Mortality CSV"
  #         )
  #       ),
  #       column(
  #         4,
  #         br(),
  #         downloadButton(
  #           outputId = "download_background_mortality_example",
  #           label = "Example Background Mortality",
  #           width = "100%"
  #         )
  #       )
  #     )
  #   )
  # ),
  #br()
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
        "Download the estimated state occupancy results for the Intervention arm. ",
        "The output includes the proportion of patients in progression-free survival (PFS), ",
        "post-progression survival (PPS), and death over the model time horizon."
      ),
      br(),
      downloadButton(
        outputId = "download_int_state_occupancy",
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