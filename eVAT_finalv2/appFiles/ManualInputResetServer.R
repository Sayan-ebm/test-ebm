#========================================================
# ManualInputResetServer.R
#========================================================
# Generic reset-to-default handlers for the small icon-only
# reset buttons placed beside every manual numeric/text entry
# field in the app. Each button's id is "reset_<fieldId>".

observeEvent(input[[paste0("reset_", "utility_pf")]], {
  updateNumericInput(session, "utility_pf", value = 0.61)
})

observeEvent(input[[paste0("reset_", "utility_pd")]], {
  updateNumericInput(session, "utility_pd", value = 0.51)
})

observeEvent(input[[paste0("reset_", "cost_pf_first")]], {
  updateNumericInput(session, "cost_pf_first", value = 4519)
})

observeEvent(input[[paste0("reset_", "cost_pf")]], {
  updateNumericInput(session, "cost_pf", value = 2178)
})

observeEvent(input[[paste0("reset_", "cost_pd")]], {
  updateNumericInput(session, "cost_pd", value = 4034)
})

observeEvent(input[[paste0("reset_", "discount_rate_cost")]], {
  updateNumericInput(session, "discount_rate_cost", value = 3.5)
})

observeEvent(input[[paste0("reset_", "discount_rate_QALY")]], {
  updateNumericInput(session, "discount_rate_QALY", value = 3.5)
})

observeEvent(input[[paste0("reset_", "WTP")]], {
  updateNumericInput(session, "WTP", value = 35000)
})

observeEvent(input[[paste0("reset_", "PSA_Iterations")]], {
  updateNumericInput(session, "PSA_Iterations", value = 1000)
})

observeEvent(input[[paste0("reset_", "time_horizon")]], {
  updateNumericInput(session, "time_horizon", value = 240)
})

observeEvent(input[[paste0("reset_", "starting_age")]], {
  updateNumericInput(session, "starting_age", value = 40)
})

observeEvent(input[[paste0("reset_", "comp_median_os")]], {
  updateNumericInput(session, "comp_median_os", value = 30)
})

observeEvent(input[[paste0("reset_", "comp_median_pfs")]], {
  updateNumericInput(session, "comp_median_pfs", value = 18)
})

observeEvent(input[[paste0("reset_", "comp_longterm_os_prob")]], {
  updateNumericInput(session, "comp_longterm_os_prob", value = 0.05)
})

observeEvent(input[[paste0("reset_", "comp_longterm_os_time")]], {
  updateNumericInput(session, "comp_longterm_os_time", value = 60)
})

observeEvent(input[[paste0("reset_", "comp_longterm_pfs_prob")]], {
  updateNumericInput(session, "comp_longterm_pfs_prob", value = 0.02)
})

observeEvent(input[[paste0("reset_", "comp_longterm_pfs_time")]], {
  updateNumericInput(session, "comp_longterm_pfs_time", value = 60)
})

observeEvent(input[[paste0("reset_", "int_median_os")]], {
  updateNumericInput(session, "int_median_os", value = 50)
})

observeEvent(input[[paste0("reset_", "int_hr_os")]], {
  updateNumericInput(session, "int_hr_os", value = 0.5)
})

observeEvent(input[[paste0("reset_", "int_median_pfs")]], {
  updateNumericInput(session, "int_median_pfs", value = 25)
})

observeEvent(input[[paste0("reset_", "int_hr_pfs")]], {
  updateNumericInput(session, "int_hr_pfs", value = 0.5)
})

observeEvent(input[[paste0("reset_", "int_longterm_os_prob")]], {
  updateNumericInput(session, "int_longterm_os_prob", value = 0.05)
})

observeEvent(input[[paste0("reset_", "int_longterm_os_time")]], {
  updateNumericInput(session, "int_longterm_os_time", value = 60)
})

observeEvent(input[[paste0("reset_", "int_longterm_pfs_prob")]], {
  updateNumericInput(session, "int_longterm_pfs_prob", value = 0.02)
})

observeEvent(input[[paste0("reset_", "int_longterm_pfs_time")]], {
  updateNumericInput(session, "int_longterm_pfs_time", value = 60)
})

#########################################################
#### Therapy Cost blocks (dynamic ids: prefix + no.) ####
#########################################################
# therapyCostUI() is invoked once per therapy slot in
# TreatmentCostsUI.R; the field ids it generates follow the
# pattern "<field>_<prefix><therapy_no>". Rather than a reset
# icon per field, the Intervention and Comparator sections each
# have a single "Reset to Default" button in their box header
# (reset_intervention_all / reset_comparator_all) that resets
# every field for every therapy slot in that section, including
# the Number of Therapies dropdown.
resetTherapySlot <- function(session, prefix, therapy_no, default_name, default_cost) {
  id <- function(field) paste0(field, prefix, therapy_no)

  updateTextInput(session, id("Tx_name_"), value = default_name)
  updateNumericInput(session, id("Tx_cost_"), value = default_cost)
  updateNumericInput(session, id("Tx_cycle_length_"), value = 4)
  updateSelectInput(session, paste0("Tx_max_cycles_flag_", prefix, therapy_no), selected = "Yes")
  updateNumericInput(session, id("Tx_max_cycles_"), value = 6)
  updateSelectInput(session, paste0("Tx_cost_change_flag_", prefix, therapy_no), selected = "No")
  updateNumericInput(session, id("Tx_discount_"), value = 0)
  updateNumericInput(session, id("Tx_cost_change_cycle_"), value = 7)
  updateNumericInput(session, id("Tx_cost_after_"), value = default_cost)
}

observeEvent(input$reset_intervention_all, {
  updateSelectInput(session, "num_therapy_int", selected = "2")
  resetTherapySlot(session, "int", 1, "Enzalutamide", 3120)
  resetTherapySlot(session, "int", 2, "Radium",        2973)
  resetTherapySlot(session, "int", 3, "Therapy 3",     1000)
})

observeEvent(input$reset_comparator_all, {
  updateSelectInput(session, "num_therapy_comp", selected = "1")
  resetTherapySlot(session, "comp", 1, "Radium",                 2973)
  resetTherapySlot(session, "comp", 2, "Comparator Therapy 2",   1000)
})
