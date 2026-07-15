#====================================================
# TREATMENT COSTS TAB
#====================================================
TreatmentCostsUI <- fluidPage(
  #==================================================
  # HEADER
  #==================================================
  fluidRow(
    box(
      width = 12,
      title = "Treatment Costs",
      status = "success",
      solidHeader = TRUE,
      p("On this tab, treatment costs for intervention and comparator therapies are specified using the cost per cycle, treatment cycle length, and treatment duration assumptions. Where a maximum number of treatment cycles is not defined, treatment is assumed to continue until disease progression. The model supports changes in treatment cost after a specified cycle and allows discounts to be applied independently to each therapy. All treatment cost inputs are defined separately for each treatment component.")
    )
  ),
  br(),
  #====================================================
  # MODEL CURRENCY
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = "Model Currency",
      status = "warning",
      solidHeader = TRUE,
      
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "currency",
            label = "Currency",
            choices = c(
              "Dollars ($)"      = "USD",
              "Euros (€)"           = "EUR",
              "Pounds (£)"  = "GBP"
              # "Swiss Franc (CHF)"       = "CHF",
              # "Swedish Krona (SEK)"     = "SEK",
              # "Norwegian Krone (NOK)"   = "NOK",
              # "Danish Krone (DKK)"      = "DKK"
            ),
            selected = "GBP"
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # INTERVENTION
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = tagList(
        "Intervention",
        actionButton(
          inputId = "reset_intervention_all",
          label = "Reset to Default",
          icon = icon("rotate-left"),
          class = "btn-reset-section-yellow",
          style = "position:absolute; top:8px; right:15px; z-index:1000;"
        )
      ),
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(
          4,
          selectInput(
            "num_therapy_int",
            "Number of Therapies",
            choices = c("1","2","3"),
            selected = "2"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          6,
          therapyCostUI(
            prefix = "int",
            therapy_no = 1,
            default_name = "Enzalutamide",
            default_cost = 3120
          )
        ),
        conditionalPanel(
          "input.num_therapy_int != '1'",
          column(
            6,
            therapyCostUI(
              prefix = "int",
              therapy_no = 2,
              default_name = "Radium",
              default_cost = 2973
            )
          )
        )
      ),
      conditionalPanel(
        "input.num_therapy_int == '3'",
        fluidRow(
          column(
            6,
            therapyCostUI(
              prefix = "int",
              therapy_no = 3,
              default_name = "Therapy 3",
              default_cost = 1000
            )
          )
        )
      )
    )
  ),
  br(),
  #====================================================
  # COMPARATOR
  #====================================================
  fluidRow(
    box(
      width = 12,
      title = tagList(
        "Comparator",
        actionButton(
          inputId = "reset_comparator_all",
          label = "Reset to Default",
          icon = icon("rotate-left"),
          class = "btn-reset-section-yellow",
          style = "position:absolute; top:8px; right:15px; z-index:1000;"
        )
      ),
      status = "info",
      solidHeader = TRUE,
      fluidRow(
        column(
          4,
          selectInput(
            "num_therapy_comp",
            "Number of Therapies",
            choices = c("1","2"),
            selected = "1"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          6,
          therapyCostUI(
            prefix = "comp",
            therapy_no = 1,
            default_name = "Radium",
            default_cost = 2973
          )
        ),
        conditionalPanel(
          "input.num_therapy_comp == '2'",
          column(
            6,
            therapyCostUI(
              prefix = "comp",
              therapy_no = 2,
              default_name = "Comparator Therapy 2",
              default_cost = 1000
            )
          )
        )
      )
    )
  )
)

