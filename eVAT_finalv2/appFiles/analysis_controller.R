#########################################################
#### Analysis Controller ################################
#########################################################
analysis_status <- reactiveVal("ready")
analysis_results <- reactiveVal(NULL)

#########################################################
#### Reset Analysis #####################################
#########################################################
observeEvent(
  list(
    input$starting_age,
    input$male_proportion,
    input$discount_rate_cost,
    input$discount_rate_qaly,
    input$time_horizon,
    input$enable_psa,
    input$psa_iterations,
    input$comp_background_mortality
  ),
  {
    analysis_status("ready")
  },
  ignoreInit = TRUE
)


