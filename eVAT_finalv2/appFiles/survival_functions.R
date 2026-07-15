#########################################################
#### EXPONENTIAL SURVIVAL FUNCTION ######################
#########################################################
generate_exponential_survival <- function(
    median_months,
    model_times,
    model_name = "Median"
) {
  #------------------------------------------------------
  # VALIDATION
  #------------------------------------------------------
  if (is.null(median_months)) {
    stop("median_months cannot be NULL")
  }
  if (median_months <= 0) {
    stop("median_months must be > 0")
  }
  #------------------------------------------------------
  # EXPONENTIAL HAZARD
  #------------------------------------------------------
  lambda <- log(2) / median_months
  #------------------------------------------------------
  # SURVIVAL FUNCTION
  #------------------------------------------------------
  survival <- exp(-lambda * model_times)
  #------------------------------------------------------
  # OUTPUT DATAFRAME
  #------------------------------------------------------
  data.frame(
    time = model_times,
    survival = survival,
    model = model_name
  )
}
#------------------------------------------------------
# Data processing
#------------------------------------------------------
read_ipd_data <- function(file){
  read.csv(
    file$datapath,
    stringsAsFactors = FALSE
  )
}
validate_ipd <- function(ipd){
  cat("\n====================================\n")
  cat("VALIDATING IPD\n")
  cat("====================================\n")
  print(names(ipd))
  required <- c(
    "time",
    "status"
  )
  missing <- required[
    !required %in% names(ipd)
  ]
  if(length(missing) > 0){
    stop(
      paste(
        "Missing columns:",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  cat("\nRows:\n")
  print(nrow(ipd))
  TRUE
}



