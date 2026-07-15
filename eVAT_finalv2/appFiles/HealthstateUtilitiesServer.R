#########################################################
#### Health State Cost & Utility Settings ###############
#########################################################
#### Guard: auto-correct blank/invalid manual entries ###
guardNumericInput(input, session, "utility_pf", default = 0.61, min = 0, max = 1, label = "Progression-Free Utility")
guardNumericInput(input, session, "utility_pd", default = 0.51, min = 0, max = 1, label = "Post-Progression Utility")
guardNumericInput(input, session, "cost_pf_first", default = 4519, min = 0, label = "PFS Cost - First Month")
guardNumericInput(input, session, "cost_pf", default = 2178, min = 0, label = "PFS Cost - Subsequent Months")
guardNumericInput(input, session, "cost_pd", default = 4034, min = 0, label = "Post-Progression Cost")

# health_state_settings <- reactive({
#   list(
#     utilities = list(
#       PF = input$utility_pf,
#       PD = input$utility_pd
#     ),
#     costs = list(
#       PF_First = input$cost_pf_first,
#       PF = input$cost_pf,
#       PD = input$cost_pd
#     )
#   )
# })

health_state_settings <- reactive({
  list(
    utilities = list(
      PF = list(
        mean = input$utility_pf,
        se = input$utility_pf_se,
        distribution = "beta"
      ),
      PD = list(
        mean = input$utility_pd,
        se = input$utility_pd_se,
        distribution = "beta"
      )
    ),
    costs = list(
      PF_First = list(
        mean = input$cost_pf_first,
        se = input$cost_pf_first_se,
        distribution = "gamma"
      ),
      PF = list(
        mean = input$cost_pf,
        se = input$cost_pf_se,
        distribution = "gamma"
      ),
      PD = list(
        mean = input$cost_pd,
        se = input$cost_pd_se,
        distribution = "gamma"
      )
    )
  )
})

output$health_state_costs_header <- renderUI({
  tags$div(
    style = "
      background:#FFF4E5;
      padding:10px;
      border-left:5px solid #F39C12;
      border-radius:6px;
      margin-bottom:20px;
      font-weight:600;
      font-size:18px;
    ",
    icon("coins"),
    paste0(" Health State Costs (", currency_symbol(), ")")
  )
})
