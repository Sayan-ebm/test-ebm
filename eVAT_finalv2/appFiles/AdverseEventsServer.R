#########################################################
#### RESET TO DEFAULT (AE DATASET) ######################
#########################################################
# See SurvivalComparatorServer.R for the rationale behind the
# flag + wrapper-reactive pattern (shinyjs::reset() alone does
# not reliably clear the server-side fileInput value).
ae_reset_flags <- reactiveValues(
  ae_file = FALSE
)

ae_file_file <- reactive({
  if (isTRUE(ae_reset_flags$ae_file)) return(NULL)
  input$ae_file
})

# Clear the flag as soon as the user picks a genuinely new file
observeEvent(input$ae_file, { ae_reset_flags$ae_file <- FALSE }, ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent(input$reset_ae_file, {
  ae_reset_flags$ae_file <- TRUE
  shinyjs::reset("ae_file")
  showNotification("AE dataset reset to default dataset.", type = "message")
})

#########################################################
#### AE DATASET #########################################
#########################################################
ae_dataset <- reactive({
  if(is.null(ae_file_file())){
    return(default_ae_probabilities)
  }
  read.csv(
    ae_file_file()$datapath,
    stringsAsFactors = FALSE
  )
})



observe({
  if(!isTRUE(input$include_ae_model)){
    return()
  }
  required_cols <- c("Event", "prob_comparator", "N_comparator", "prob_intervention", "N_intervention", "cost", "disutility")
  optional_cols <- c(
    "LCI_disutility",
    "UCI_disutility",
    "SE_disutility"
  )
  missing_cols <- setdiff(required_cols, names(ae_dataset()))
  validate(
    need(
      length(missing_cols) == 0,
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  )
})

output$ae_data_source <- renderText({
  if(is.null(ae_file_file())){
    "Currently using: Default AE dataset"
  } else {
    paste(
      "Currently using uploaded file:",
      ae_file_file()$name
    )
  }
})

observe({
  req(ae_dataset())
  cat("\n====================\n")
  cat("AE DATA LOADED\n")
  cat("====================\n")
  cat("\nRows:", nrow(ae_dataset()))
  cat("\nColumns:", ncol(ae_dataset()))
  cat("\n\nFirst few rows:\n")
  print(head(ae_dataset()))
})

output$download_ae_template <- downloadHandler(
  filename = function() {
    paste0("AE_Template_Default_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # always export DEFAULT dataset (not uploaded one)
    write.csv(
      default_ae_probabilities,
      file,
      row.names = FALSE
    )
  }
)

#########################################################
#### AE SETTINGS OBJECT #################################
#########################################################
ae_settings <- reactive({
  if(!isTRUE(input$include_ae_model)){
    return(
      list(ae_df = default_ae_probabilities,
           include_ae = FALSE))
    
  }
  build_ae_settings(ae_df = ae_dataset(), include_ae = TRUE)
})

#########################################################
#### AE OBJECT ##########################################
#########################################################
ae_object <- reactive({
  req(ae_settings())
  
  obj <- build_ae_object(ae_settings())
  
  cat("\nAE OBJECT CHECK:\n")
  print(head(obj$data))
  
  obj
})