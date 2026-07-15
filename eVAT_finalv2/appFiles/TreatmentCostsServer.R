#========================================================
# TreatmentCostsServer.R
#========================================================
#########################################################
#### Intervention Therapy 1 #############################
#########################################################
int_therapy_1_settings <- reactive({
  build_therapy_settings(
    therapy_name = input$Tx_name_int1,
    cost_per_cycle = input$Tx_cost_int1,
    cycle_length_weeks = input$Tx_cycle_length_int1,
    max_cycles_flag = input$Tx_max_cycles_flag_int1,
    max_cycles = input$Tx_max_cycles_int1, 
    cost_change_flag = input$Tx_cost_change_flag_int1,
    cost_change_cycle = input$Tx_cost_change_cycle_int1,
    cost_after_change = input$Tx_cost_after_int1,
    discount = input$Tx_discount_int1
  )
})

#########################################################
#### Intervention Therapy 2 #############################
#########################################################
int_therapy_2_settings <- reactive({
  req(as.numeric(input$num_therapy_int) >= 2)
  build_therapy_settings(
    therapy_name = input$Tx_name_int2,
    cost_per_cycle = input$Tx_cost_int2,
    cycle_length_weeks = input$Tx_cycle_length_int2,
    max_cycles_flag = input$Tx_max_cycles_flag_int2,
    max_cycles = input$Tx_max_cycles_int2,
    cost_change_flag = input$Tx_cost_change_flag_int2,
    cost_change_cycle = input$Tx_cost_change_cycle_int2,
    cost_after_change = input$Tx_cost_after_int2,
    discount = input$Tx_discount_int2
  )
})

#########################################################
#### Intervention Therapy 3 #############################
#########################################################
int_therapy_3_settings <- reactive({
  req(input$num_therapy_int == "3")
  build_therapy_settings(
    therapy_name = input$Tx_name_int3,
    cost_per_cycle = input$Tx_cost_int3,
    cycle_length_weeks = input$Tx_cycle_length_int3,
    max_cycles_flag = input$Tx_max_cycles_flag_int3,
    max_cycles = input$Tx_max_cycles_int3,
    cost_change_flag = input$Tx_cost_change_flag_int3,
    cost_change_cycle = input$Tx_cost_change_cycle_int3,
    cost_after_change = input$Tx_cost_after_int3,
    discount = input$Tx_discount_int3
  )
})

#########################################################
#### Intervention Therapy List ##########################
#########################################################
int_therapy_settings_list <- reactive({
  therapies <- list()
  therapies[[1]] <- int_therapy_1_settings()
  if(
    as.numeric(
      input$num_therapy_int
    ) >= 2
  ){
    therapies[[2]] <- int_therapy_2_settings()
  }
  if(
    input$num_therapy_int == "3"
  ){
    therapies[[3]] <- int_therapy_3_settings()
  }
  therapies
})

#########################################################
#### Comparator Therapy 1 ###############################
#########################################################
comp_therapy_1_settings <- reactive({
  build_therapy_settings(
    therapy_name = input$Tx_name_comp1,
    cost_per_cycle = input$Tx_cost_comp1,
    cycle_length_weeks = input$Tx_cycle_length_comp1,
    max_cycles_flag = input$Tx_max_cycles_flag_comp1,
    max_cycles = input$Tx_max_cycles_comp1,
    cost_change_flag = input$Tx_cost_change_flag_comp1,
    cost_change_cycle = input$Tx_cost_change_cycle_comp1,
    cost_after_change = input$Tx_cost_after_comp1,
    discount = input$Tx_discount_comp1
  )
})

#########################################################
#### Comparator Therapy 2 ###############################
#########################################################
comp_therapy_2_settings <- reactive({
  req(input$num_therapy_comp == "2")
  build_therapy_settings(
    therapy_name = input$Tx_name_comp2,
    cost_per_cycle = input$Tx_cost_comp2,
    cycle_length_weeks = input$Tx_cycle_length_comp2,
    max_cycles_flag = input$Tx_max_cycles_flag_comp2,
    max_cycles = input$Tx_max_cycles_comp2,
    cost_change_flag = input$Tx_cost_change_flag_comp2,
    cost_change_cycle = input$Tx_cost_change_cycle_comp2,
    cost_after_change = input$Tx_cost_after_comp2,
    discount = input$Tx_discount_comp2
  )
})

#########################################################
#### Comparator Therapy Settings List ###################
#########################################################
comp_therapy_settings_list <- reactive({
  therapies <- list()
  #######################################################
  #### Therapy 1 ########################################
  #######################################################
  therapies[[1]] <- comp_therapy_1_settings()
  #######################################################
  #### Therapy 2 ########################################
  #######################################################
  if(
    input$num_therapy_comp == "2"
  ){
    therapies[[2]] <- comp_therapy_2_settings()
  }
  #######################################################
  #### Return ###########################################
  #######################################################
  therapies
})

#For Treatment Dosing Schedule

showCycleLengthInfo <- function() {
  showModal(
    modalDialog(
      title = "Treatment Administration Cycle Length",
      tagList(
        tags$p(
          "The treatment administration cycle length should be specified in weeks (e.g., every 4 weeks, every 5 weeks, every 6 weeks)."
        ),
        tags$p(
          "The economic model operates using monthly cycles. Therefore, treatment administration schedules entered in weeks are automatically converted into months so that treatment costs can be aligned with the model cycle length."
        ),
        tags$hr(),
        tags$h4("How are treatment costs applied?"),
        tags$p(
          "Treatment acquisition costs are applied in the model cycles during which administrations occur. After converting the treatment schedule from weeks to months, the model determines which administrations occur within each monthly cycle and applies costs accordingly."
        ),
        tags$hr(),
        tags$h4("Example"),
        tags$ul(
          tags$li("Treatment is administered every 4 weeks"),
          tags$li("Administration 1 occurs at Week 0"),
          tags$li("Administration 2 occurs at Week 4"),
          tags$li("One model cycle corresponds to approximately 4.34 weeks"),
          tags$li("Both administrations therefore occur within Cycle 0"),
          tags$li("As a result, two administrations and their associated costs are applied within Cycle 0")
        ),
        tags$p(
          "The same approach is used for all administration schedules. Weekly cycle lengths are converted to their equivalent timing in months, and treatment costs are applied to the model cycles in which administrations occur."
        ),
        tags$hr(),
        tags$p(
          tags$b("Impact on model results:")
        ),
        tags$ul(
          tags$li("Treatment acquisition costs")
        ),
        tags$p(
          "This setting does not affect survival outcomes, disease progression, health-state occupancy, adverse event probabilities, or utility values."
        )
      ),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close")
    )
  )
}


observeEvent(input$info_cycle_length_int_1, {
  showCycleLengthInfo()
})

observeEvent(input$info_cycle_length_int_2, {
  showCycleLengthInfo()
})

observeEvent(input$info_cycle_length_int_3, {
  showCycleLengthInfo()
})

observeEvent(input$info_cycle_length_comp_1, {
  showCycleLengthInfo()
})

observeEvent(input$info_cycle_length_comp_2, {
  showCycleLengthInfo()
})


##For treatment stopping explanation

showTreatmentStoppingInfo <- function() {
  showModal(
    modalDialog(
      title = "Treatment Stopping Rule",
      tagList(
        tags$p(
          "This setting determines whether treatment is assumed to stop after a fixed number of treatment cycles or continue until disease progression."
        ),
        tags$hr(),
        tags$p(
          tags$b("If treatment stops after a fixed number of cycles:")
        ),
        tags$ul(
          tags$li(
            "Treatment acquisition costs cease after the specified treatment duration"
          ),
          tags$li(
            "Treatment-related adverse event (AE) costs cease after treatment discontinuation"
          ),
          tags$li(
            "Treatment-related AE disutilities cease after treatment discontinuation"
          )
        ),
        tags$p(
          tags$b("The following are not affected by treatment stopping:")
        ),
        tags$ul(
          tags$li(
            "Disease progression and survival outcomes"
          ),
          tags$li(
            "State occupancy calculations"
          ),
          tags$li(
            "Background mortality assumptions"
          ),
          tags$li(
            "Disease management and health state costs, unless specified elsewhere in the model"
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Why is this important?")
        ),
        tags$p(
          "Many oncology and chronic disease treatments have a maximum treatment duration, treatment cap, or protocol-defined stopping rule. This setting allows the model to reflect real-world treatment exposure while continuing to project long-term clinical outcomes using the selected survival curves."
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  )
}


observeEvent(input$info_treatment_stop_int_1, {
  showTreatmentStoppingInfo()
})

observeEvent(input$info_treatment_stop_int_2, {
  showTreatmentStoppingInfo()
})

observeEvent(input$info_treatment_stop_int_3, {
  showTreatmentStoppingInfo()
})

observeEvent(input$info_treatment_stop_comp_1, {
  showTreatmentStoppingInfo()
})

observeEvent(input$info_treatment_stop_comp_2, {
  showTreatmentStoppingInfo()
})


#########################################################
#### For treatment cost correction ######################
#########################################################
currency_symbol <- reactive({
  switch(
    input$currency,
    "USD" = "$",
    "EUR" = "€",
    "GBP" = "£",
    # "CHF" = "CHF",
    # "SEK" = "SEK",
    # "NOK" = "NOK",
    # "DKK" = "DKK",
    "£"
  )
})

renderCostInput <- function(prefix, therapy_no, default_cost) {
  output[[paste0("Tx_cost_ui_", prefix, therapy_no)]] <- renderUI({
    current_val <- isolate(input[[paste0("Tx_cost_", prefix, therapy_no)]])
    if (is.null(current_val) || is.na(current_val) || current_val < 0) {
      current_val <- default_cost
    }
    numericInput(
      inputId = paste0("Tx_cost_", prefix, therapy_no),
      label = tagList(
        icon("coins"),
        paste0("Cost per Cycle (", currency_symbol(), ")")
      ),
      value = current_val,
      min = 0
    )
  })
}

#########################################################
#### Create the 5 Cost Inputs ###########################
#########################################################
renderCostInput("int", 1, 3120)
renderCostInput("int", 2, 2973)
renderCostInput("int", 3, 1000)
renderCostInput("comp", 1, 2973)
renderCostInput("comp", 2, 1000)

#########################################################
#### Ensure cost inputs render even if the Treatment ####
#### Costs tab has not yet been visited (fixes costs ####
#### defaulting to 0 when Run is clicked from another ###
#### tab) ################################################
#########################################################
outputOptions(output, "Tx_cost_ui_int1", suspendWhenHidden = FALSE)
outputOptions(output, "Tx_cost_ui_int2", suspendWhenHidden = FALSE)
outputOptions(output, "Tx_cost_ui_int3", suspendWhenHidden = FALSE)
outputOptions(output, "Tx_cost_ui_comp1", suspendWhenHidden = FALSE)
outputOptions(output, "Tx_cost_ui_comp2", suspendWhenHidden = FALSE)

#########################################################
#### Guard: auto-correct negative/blank cost inputs #####
#########################################################
guard_cost_input <- function(prefix, therapy_no) {
  input_id <- paste0("Tx_cost_", prefix, therapy_no)
  observeEvent(input[[input_id]], {
    val <- input[[input_id]]
    if (!is.null(val) && !is.na(val) && val < 0) {
      updateNumericInput(session, input_id, value = 0)
      therapy_label <- input[[paste0("Tx_name_", prefix, therapy_no)]]
      if (is.null(therapy_label) || therapy_label == "") {
        therapy_label <- "this therapy"
      }
      showNotification(
        paste0(
          "Cost per Cycle for ", therapy_label,
          " cannot be negative or blank. It has been reset to 0 - please enter a valid cost before running the model."
        ),
        type = "warning",
        duration = 6
      )
    }
  })
}

guard_cost_input("int", 1)
guard_cost_input("int", 2)
guard_cost_input("int", 3)
guard_cost_input("comp", 1)
guard_cost_input("comp", 2)

#########################################################
#### Guard: auto-correct all other manual numeric #######
#### entries in the treatment cost tables ###############
#########################################################
guard_therapy_numeric_inputs <- function(prefix, therapy_no) {
  guardNumericInput(input, session, paste0("Tx_cycle_length_", prefix, therapy_no),
                     default = 4, min = 1, label = "Cycle Length (Weeks)")
  guardNumericInput(input, session, paste0("Tx_max_cycles_", prefix, therapy_no),
                     default = 6, min = 1, label = "Maximum Treatment Cycles")
  guardNumericInput(input, session, paste0("Tx_discount_", prefix, therapy_no),
                     default = 0, min = 0, max = 100, label = "Discount (%)")
  guardNumericInput(input, session, paste0("Tx_cost_change_cycle_", prefix, therapy_no),
                     default = 7, min = 1, label = "Cost Changes Starting from Cycle")
  guardNumericInput(input, session, paste0("Tx_cost_after_", prefix, therapy_no),
                     default = 0, min = 0, label = "Cost per Cycle After Change")
}

guard_therapy_numeric_inputs("int", 1)
guard_therapy_numeric_inputs("int", 2)
guard_therapy_numeric_inputs("int", 3)
guard_therapy_numeric_inputs("comp", 1)
guard_therapy_numeric_inputs("comp", 2)












