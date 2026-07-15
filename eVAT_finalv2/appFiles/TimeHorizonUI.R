#=====================================================
# TimeHorizonUI.R
#=====================================================

timeHorizonUI <- fluidRow(
  box(
    width = 12,
    
    numericInput(
      inputId = "time_horizon",
      label   = tagList(
        "Time Horizon (Months)",
        actionButton(
          inputId = paste0("reset_", "time_horizon"),
          label = NULL,
          icon = icon("rotate-left"),
          class = "btn-reset-icon-only",
          title = "Reset to default")
      ),
      value   = 240,
      min     = 1,
      max     = 1000
    )
  )
)