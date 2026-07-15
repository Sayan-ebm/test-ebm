#====================================================
# HEALTH STATE COSTS & UTILITIES TAB
#====================================================
HealthStateCostsUtilitiesUI <- fluidPage(
  
  #==================================================
  # HEADER
  #==================================================
  fluidRow(
    box(
      width = 12,
      title = "Health State Costs and Utilities",
      status = "success",
      solidHeader = TRUE,
      p(
        "Define health-state utilities and disease management costs used within the partitioned survival model. Utilities are entered on an annual scale and are applied to progression-free and progressed disease health states. Costs are applied separately for the first month in progression-free survival, subsequent progression-free months, and post-progression survival."
      )
    )
  ),
  
  br(),
  
  #==================================================
  # UTILITIES
  #==================================================
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
          min-height:340px;
        ",
        
        tags$div(
          style = "
            background:#EAF2FF;
            padding:10px;
            border-left:5px solid #3C8DBC;
            border-radius:6px;
            margin-bottom:20px;
            font-weight:600;
            font-size:18px;
          ",
          icon("heart"),
          " Health State Utilities"
        ),
        
        numericInput(
          inputId = "utility_pf",
          label = tagList(
            tagList(
              icon("person-walking"),
              " Progression-Free Utility"
            ),
            actionButton(
              inputId = paste0("reset_", "utility_pf"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 0.61,
          min = 0,
          max = 1,
          step = 0.01
        ),
        
        helpText(
          "Utility applied while patients remain progression-free."
        ),
        
        br(),
        
        numericInput(
          inputId = "utility_pd",
          label = tagList(
            tagList(
              icon("bed"),
              " Post-Progression Utility"
            ),
            actionButton(
              inputId = paste0("reset_", "utility_pd"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 0.51,
          min = 0,
          max = 1,
          step = 0.01
        ),
        
        helpText(
          "Utility applied following disease progression."
        )
      )
    ),
    
    #================================================
    # COSTS
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
          min-height:340px;
        ",
        
        ## Reactive header
        uiOutput("health_state_costs_header"),
        
        numericInput(
          inputId = "cost_pf_first",
          label = tagList(
            tagList(
              icon("calendar-day"),
              " PFS Cost - First Month"
            ),
            actionButton(
              inputId = paste0("reset_", "cost_pf_first"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 4519,
          min = 0
        ),
        
        helpText(
          "Applied once upon entering progression-free survival."
        ),
        
        br(),
        
        numericInput(
          inputId = "cost_pf",
          label = tagList(
            tagList(
              icon("calendar"),
              " PFS Cost - Subsequent Months"
            ),
            actionButton(
              inputId = paste0("reset_", "cost_pf"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 2178,
          min = 0
        ),
        
        helpText(
          "Applied monthly while patients remain progression-free."
        ),
        
        br(),
        
        numericInput(
          inputId = "cost_pd",
          label = tagList(
            tagList(
              icon("hospital"),
              " Post-Progression Cost"
            ),
            actionButton(
              inputId = paste0("reset_", "cost_pd"),
              label = NULL,
              icon = icon("rotate-left"),
              class = "btn-reset-icon-only",
              title = "Reset to default")
          ),
          value = 4034,
          min = 0
        ),
        
        helpText(
          "Applied monthly following progression."
        )
      )
    )
  ),
  
  br(),
  
  #==================================================
  # INFORMATION BOX
  #==================================================
  fluidRow(
    box(
      width = 12,
      status = "info",
      solidHeader = TRUE,
      title = tagList(
        icon("circle-info"),
        "Model Assumptions"
      ),
      
      tags$ul(
        tags$li(
          "Utilities are assumed to be constant within each health state."
        ),
        tags$li(
          "Health state costs are applied independently of treatment acquisition costs."
        ),
        tags$li(
          "Progression-free costs can differ between the first month and subsequent months."
        ),
        tags$li(
          "Post-progression costs are applied until death."
        )
      )
    )
  )
)