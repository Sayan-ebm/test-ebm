#========================================================
# functions.R
# Core reusable functions for survival modeling
#========================================================

#========================================================
# REQUIRED PACKAGES
#========================================================

library(survival)
library(flexsurv)
library(dplyr)

#========================================================
# FUNCTION: READ CSV FILE
#========================================================

read_uploaded_csv <- function(file_input) {
  
  #----------------------------------------------
  # NULL CHECK
  #----------------------------------------------
  
  if (is.null(file_input)) {
    return(NULL)
  }
  
  #----------------------------------------------
  # READ CSV
  #----------------------------------------------
  
  data <- read.csv(
    file_input$datapath,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

#========================================================
# FUNCTION: GET DEFAULT OR UPLOADED DATA
#========================================================

get_uploaded_or_default <- function(
    uploaded_file,
    default_data
) {
  
  #----------------------------------------------
  # USE UPLOADED FILE IF AVAILABLE
  #----------------------------------------------
  
  if (!is.null(uploaded_file)) {
    
    uploaded_data <- read_uploaded_csv(
      uploaded_file
    )
    
    return(uploaded_data)
  }
  
  #----------------------------------------------
  # OTHERWISE USE DEFAULT DATA
  #----------------------------------------------
  
  return(default_data)
}

#========================================================
# FUNCTION: VALIDATE SURVIVAL DATA
#========================================================

validate_survival_data <- function(
    data,
    time_col = "time",
    event_col = "event"
) {
  
  #----------------------------------------------
  # NULL CHECK
  #----------------------------------------------
  
  if (is.null(data)) {
    stop("Uploaded data is NULL.")
  }
  
  #----------------------------------------------
  # COLUMN CHECK
  #----------------------------------------------
  
  required_cols <- c(
    time_col,
    event_col
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      paste(
        "Missing required columns:",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }
  
  #----------------------------------------------
  # NUMERIC CHECK
  #----------------------------------------------
  
  if (!is.numeric(data[[time_col]])) {
    
    stop(
      paste(
        time_col,
        "column must be numeric."
      )
    )
  }
  
  if (!is.numeric(data[[event_col]])) {
    
    stop(
      paste(
        event_col,
        "column must be numeric."
      )
    )
  }
  
  #----------------------------------------------
  # MISSING VALUE CHECK
  #----------------------------------------------
  
  if (any(is.na(data[[time_col]]))) {
    
    stop(
      paste(
        "Missing values found in",
        time_col,
        "column."
      )
    )
  }
  
  if (any(is.na(data[[event_col]]))) {
    
    stop(
      paste(
        "Missing values found in",
        event_col,
        "column."
      )
    )
  }
  
  #----------------------------------------------
  # NEGATIVE TIME CHECK
  #----------------------------------------------
  
  if (any(data[[time_col]] < 0)) {
    
    stop("Negative time values detected.")
  }
  
  #----------------------------------------------
  # EVENT VALUE CHECK
  #----------------------------------------------
  
  allowed_events <- c(0, 1)
  
  if (!all(data[[event_col]] %in% allowed_events)) {
    
    stop(
      paste(
        event_col,
        "must contain only 0 or 1."
      )
    )
  }
  
  return(TRUE)
}

#========================================================
# FUNCTION: CREATE SURVIVAL OBJECT
#========================================================

create_surv_object <- function(
    data,
    time_col = "time",
    event_col = "event"
) {
  
  surv_object <- Surv(
    time = data[[time_col]],
    event = data[[event_col]]
  )
  
  return(surv_object)
}

#========================================================
# FUNCTION: FIT PARAMETRIC MODELS
#========================================================

fit_parametric_models <- function(
    surv_object
) {
  
  fitted_models <- list()
  
  #----------------------------------------------
  # SAFE MODEL FITTER
  #----------------------------------------------
  
  safe_fit <- function(dist_name) {
    
    tryCatch({
      
      flexsurvreg(
        surv_object ~ 1,
        dist = dist_name
      )
      
    }, error = function(e) {
      
      message(
        paste(
          "Model failed:",
          dist_name
        )
      )
      
      return(NULL)
    })
  }
  
  #----------------------------------------------
  # FIT DISTRIBUTIONS
  #----------------------------------------------
  
  fitted_models$Exponential <- safe_fit("exp")
  
  fitted_models$Weibull <- safe_fit("weibull")
  
  fitted_models$Gompertz <- safe_fit("gompertz")
  
  fitted_models$LogNormal <- safe_fit("lognormal")
  
  fitted_models$LogLogistic <- safe_fit("llogis")
  
  fitted_models$GenGamma <- safe_fit("gengamma")
  
  #----------------------------------------------
  # REMOVE FAILED MODELS
  #----------------------------------------------
  
  fitted_models <- fitted_models[
    !sapply(fitted_models, is.null)
  ]
  
  return(fitted_models)
}

#========================================================
# FUNCTION: EXTRACT AIC TABLE
#========================================================

extract_aic_table <- function(
    fitted_models
) {
  
  #----------------------------------------------
  # EMPTY TABLE
  #----------------------------------------------
  
  aic_table <- data.frame(
    Distribution = character(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  #----------------------------------------------
  # LOOP THROUGH MODELS
  #----------------------------------------------
  
  for (model_name in names(fitted_models)) {
    
    model_aic <- tryCatch(
      
      AIC(fitted_models[[model_name]]),
      
      error = function(e) {
        NA
      }
    )
    
    aic_table <- rbind(
      aic_table,
      data.frame(
        Distribution = model_name,
        AIC = round(model_aic, 2)
      )
    )
  }
  
  #----------------------------------------------
  # SORT BY AIC
  #----------------------------------------------
  
  aic_table <- aic_table %>%
    arrange(AIC)
  
  return(aic_table)
}

#========================================================
# FUNCTION: PLOT PARAMETRIC FITS
#========================================================

plot_parametric_fits <- function(
    fitted_models,
    km_fit = NULL
) {
  
  #----------------------------------------------
  # EMPTY PLOT
  #----------------------------------------------
  
  plot(
    NULL,
    xlim = c(0, 100),
    ylim = c(0, 1),
    xlab = "Time",
    ylab = "Survival Probability",
    main = "Parametric Survival Fits"
  )
  
  #----------------------------------------------
  # PLOT KM CURVE
  #----------------------------------------------
  
  if (!is.null(km_fit)) {
    
    lines(
      km_fit,
      conf.int = FALSE,
      col = "black",
      lwd = 2
    )
  }
  
  #----------------------------------------------
  # MODEL COLORS
  #----------------------------------------------
  
  colors <- c(
    "red",
    "blue",
    "green",
    "purple",
    "orange",
    "brown"
  )
  
  #----------------------------------------------
  # ADD FITTED CURVES
  #----------------------------------------------
  
  i <- 1
  
  for (model_name in names(fitted_models)) {
    
    lines(
      fitted_models[[model_name]],
      col = colors[i],
      lwd = 2
    )
    
    i <- i + 1
  }
  
  #----------------------------------------------
  # LEGEND
  #----------------------------------------------
  
  legend(
    "topright",
    legend = names(fitted_models),
    col = colors[1:length(fitted_models)],
    lwd = 2,
    cex = 0.8
  )
}

#========================================================
# FUNCTION: PLOT MEDIAN SURVIVAL
#========================================================

plot_median_survival <- function(
    median_value,
    label = "Median Survival"
) {
  
  #----------------------------------------------
  # VALIDATE MEDIAN VALUE
  #----------------------------------------------
  
  if (is.null(median_value)) {
    
    stop("Median value is NULL.")
  }
  
  if (median_value <= 0) {
    
    stop(
      "Median survival must be greater than 0."
    )
  }
  
  #----------------------------------------------
  # EXPONENTIAL APPROXIMATION
  #----------------------------------------------
  
  lambda <- log(2) / median_value
  
  time_seq <- seq(
    0,
    median_value * 3,
    by = 0.5
  )
  
  survival_prob <- exp(
    -lambda * time_seq
  )
  
  #----------------------------------------------
  # PLOT
  #----------------------------------------------
  
  plot(
    time_seq,
    survival_prob,
    type = "l",
    lwd = 3,
    col = "blue",
    xlab = "Time (Months)",
    ylab = "Survival Probability",
    main = label
  )
  
  #----------------------------------------------
  # MEDIAN REFERENCE LINES
  #----------------------------------------------
  
  abline(
    h = 0.5,
    lty = 2,
    col = "red"
  )
  
  abline(
    v = median_value,
    lty = 2,
    col = "red"
  )
}