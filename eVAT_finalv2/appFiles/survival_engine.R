#########################################################
#### Build survival source ##############################
#########################################################
build_survival_source <- function(
    input_type,
    median_value = NULL,
    ipd = NULL,
    km_survival = NULL,
    km_risk = NULL
){
  if(input_type == "Median"){
    return(
      list(
        source = "median",
        median = median_value))
  }
  if(!is.null(ipd)){
    return(
      list(
        source = "ipd",
        ipd = ipd))
  }
  if(!is.null(km_survival) &&
     !is.null(km_risk)){
    reconstructed <- reconstruct_ipd(
      n_at_risk = km_risk,
      p_survival = km_survival
    )
    return(
      list(
        source = "km",
        ipd = reconstructed))
  }
  stop("No valid survival source")
}

#########################################################
#### Kaplan Meier #######################################
#########################################################
create_km_from_ipd <- function(ipd){
  stopifnot(
    all(c("time","status") %in% names(ipd))
  )
  fit <- survival::survfit(
    survival::Surv(time,status) ~ 1,
    data = ipd
  )
  data.frame(
    time = fit$time,
    surv = fit$surv,
    model = "Kaplan-Meier"
  )
}

#########################################################
#### Generate active survival curve #####################
#########################################################
generate_active_survival_curve <- function(survival_source, horizon, selected_model = NULL){
  if(survival_source$source == "median"){
    curve <- generate_exponential_survival(
      median_months =
        survival_source$median,
      model_times =
        seq(0, max(horizon, 0), 1)
    )
    names(curve)[2] <- "surv"
    return(curve)
  }
  ipd <- survival_source$ipd
  models <- fit_parametric_models(ipd)
  predictions <- generate_survival_predictions(models, horizon)
  if(is.null(selected_model)){
    fit_table <- create_fit_table(models)
    selected_model <- fit_table$Model[1]
  }
  get_model_survival_curve(
    predictions,
    selected_model
  )
}

#########################################################
#### Build Model package  ###############################
#########################################################
build_survival_package <- function(ipd, horizon){
  models <- fit_parametric_models(ipd)
  fit_table <- create_fit_table(models)
  km <- create_km_from_ipd(ipd)
  predictions <- generate_survival_predictions(
    models,
    horizon
  )
  pkg <- list(
    ipd = ipd,
    km = km,
    models = models,
    fit_table = fit_table,
    predictions = predictions
  )
  validate_survival_package(pkg)
  pkg
}

#########################################################
#### Build survival object  #############################
#########################################################
build_survival_object <- function(
    input_type,
    horizon,
    median_value = NULL,
    ipd = NULL,
    curve_name = "Median",
    km_survival = NULL,
    km_risk = NULL
){
  source <- build_survival_source(
    input_type = input_type,
    median_value = median_value,
    ipd = ipd,
    km_survival = km_survival,
    km_risk = km_risk
  )
  if(source$source == "median"){
    curve <- generate_exponential_survival(
      median_months = source$median,
      model_times = seq(0, max(horizon, 0), 1),
      model_name = curve_name
    )
    names(curve)[2] <- "surv"
    return(
      list(
        source = source,
        curve = curve,
        type = "median"
      )
    )
  }
  package <- build_survival_package(
    ipd = source$ipd,
    horizon = horizon
  )
  list(
    source = source,
    package = package,
    type = "survival"
  )
}

#########################################################
#### Select Active Model ###############################
#########################################################
select_active_model <- function(
    fit_table,
    selected_rows = NULL
){
  if(
    is.null(fit_table) ||
    nrow(fit_table) == 0
  ){
    return(NULL)
  }
  #######################################################
  #### No Selection -> Best AIC #########################
  #######################################################
  if(
    is.null(selected_rows) ||
    length(selected_rows) == 0
  ){
    return(fit_table$Model[1])
  }
  #######################################################
  #### Use First Selected Model #########################
  #######################################################
  selected_rows <- selected_rows[
    selected_rows <= nrow(fit_table)
  ]
  if(length(selected_rows) == 0){
    return(fit_table$Model[1])
  }
  fit_table$Model[selected_rows[1]]
}

#########################################################
#### Best Model #########################################
#########################################################
get_best_model <- function(
    package
){
  package$fit_table$Model[1]
}

#########################################################
#### Extract One Survival Curve #########################
#########################################################
extract_survival_curve <- function(
    package,
    selected_model = NULL
){
  if(is.null(package)){
    return(NULL)
  }
  #######################################################
  #### Default = Lowest AIC #############################
  #######################################################
  if(is.null(selected_model)){
    selected_model <- package$fit_table$Model[1]
  }
  package$predictions |>
    dplyr::filter(
      model == selected_model
    )
}

#########################################################
#### Extract Model Object ###############################
#########################################################
get_model_object <- function(
    package,
    model_name = NULL
){
  if(is.null(model_name)){
    model_name <- get_best_model(
      package
    )
  }
  package$models[[model_name]]
}

#########################################################
#### Get Active Curve ###################################
#########################################################
get_active_curve <- function(
    survival_object,
    selected_model = NULL
){
  if(
    survival_object$type ==
    "median"
  ){
    return(
      survival_object$curve
    )
  }
  extract_survival_curve(
    package = survival_object$package,
    selected_model = selected_model
  )
}

#########################################################
#### Get All Curves #####################################
#########################################################
get_all_survival_curves <- function(
    survival_object
){
  if(
    survival_object$type ==
    "median"
  ){
    return(
      survival_object$curve
    )
  }
  dplyr::bind_rows(
    survival_object$package$km,
    survival_object$package$predictions
  )
}

#########################################################
#### Plot Data ##########################################
#########################################################
get_survival_plot_data <- function(
    package,
    selected_models = NULL
){
  km <- package$km
  preds <- package$predictions
  if(is.null(selected_models)){
    preds <- preds[
      preds$model ==
        package$fit_table$Model[1],
    ]
  } else {
    preds <- preds[
      preds$model %in%
        selected_models,
    ]
  }
  dplyr::bind_rows(
    km,
    preds
  )
}

#########################################################
#### KM vs Model Comparison #############################
#########################################################
get_comparison_data <- function(
    package,
    selected_model = NULL
){
  km <- package$km
  model_curve <- extract_survival_curve(
    package,
    selected_model
  )
  dplyr::bind_rows(
    km,
    model_curve
  )
}


normalize_survival_curve <- function(df) {
  df <- df[, c("time", "surv", "model")]
  df$time <- as.numeric(df$time)
  df$surv <- as.numeric(df$surv)
  df
}
#########################################################
#### HR Utilities #######################################
#########################################################
apply_hr_to_survival <- function(surv_df, hr) {
  if (is.null(surv_df)) {
    stop("surv_df is NULL")
  }
  required_cols <- c("time", "surv")
  if (!all(required_cols %in% colnames(surv_df))) {
    stop("Survival curve must contain: time, surv")
  }
  df <- surv_df
  df$time <- as.numeric(df$time)
  df$surv <- as.numeric(df$surv)
  # SAFETY: clamp survival into valid range
  df$surv <- pmin(pmax(df$surv, 1e-10), 1)
  hr <- as.numeric(hr)
  # CORE HR transformation
  df$surv <- df$surv ^ hr
  # FINAL safety clamp
  df$surv <- pmin(pmax(df$surv, 1e-10), 1)
  base_model <- if (!is.null(unique(surv_df$model))) {
    unique(surv_df$model)[1]
  } else {
    "Model"
  }
  df$model <- paste0(base_model, "_HR_", hr)
  df
}

#########################################################
#### Survival At Time ###################################
#########################################################
get_survival_at_time <- function(
    curve,
    timepoint
){
  idx <- which.min(
    abs(
      curve$time -
        timepoint
    )
  )
  curve$surv[idx]
}

#########################################################
#### Mean Survival ######################################
#########################################################
calculate_mean_survival <- function(
    curve
){
  sum(
    diff(curve$time) *
      head(
        curve$surv,
        -1
      )
  )
}

#########################################################
#### Diagnostics ########################################
#########################################################
summarise_ipd <- function(
    ipd
){
  list(
    rows = nrow(ipd),
    events = sum(
      ipd$status == 1
    ),
    censored = sum(
      ipd$status == 0
    ),
    median_followup =
      median(ipd$time)
  )
}

#########################################################
#### Package Diagnostics ################################
#########################################################
summarise_survival_package <- function(
    package
){
  list(
    patients = nrow(package$ipd),
    events = sum(package$ipd$status == 1),
    censored = sum(package$ipd$status == 0),
    models = names(package$models),
    best_model = package$fit_table$Model[1]
  )
}

#########################################################
#### Validators #########################################
#########################################################
validate_survival_package <- function(
    pkg
){
  stopifnot(
    !is.null(pkg$ipd)
  )
  stopifnot(
    !is.null(pkg$km)
  )
  stopifnot(
    !is.null(pkg$models)
  )
  stopifnot(
    !is.null(pkg$fit_table)
  )
  stopifnot(
    !is.null(pkg$predictions)
  )
  TRUE
}

#########################################################
#### Type Checkers ######################################
#########################################################
is_median_source <- function(
    survival_object
){
  survival_object$type == "median"
}
is_survival_source <- function(
    survival_object
){
  survival_object$type == "survival"
}

#########################################################
#### Engine Information #################################
#########################################################
survival_engine_info <- function(){
  list(
    version = "0.1",
    supported_models = c(
      "Exponential",
      "Weibull",
      "Gompertz",
      "Lognormal",
      "Loglogistic",
      "Gamma"
    )
  )
}
print_survival_summary <- function(
    survival_object
){
  cat("\n====================\n")
  cat("TYPE:",survival_object$type,"\n")
  if(
    survival_object$type ==
    "survival"
  ){
    print(survival_object$package$fit_table)
  }
  cat("\n====================\n")
}

#########################################################
#### Extract Model Parameters ###########################
#########################################################
extract_extrapolation_parameters <- function(models){
  result <- list()
  for(model_name in names(models)){
    fit <- models[[model_name]]
    coefs <- as.numeric(coef(fit))
    param_names <- switch(
      model_name,
      "Exponential" =
        c("Log Rate"),
      "Weibull" =
        c("Log Shape","Log Scale"),
      "Gamma" =
        c("Log Shape","Log Rate"),
      "Lognormal" =
        c("Mu","Log Sigma"),
      "Loglogistic" =
        c("Log Shape","Log Scale"),
      "Gompertz" =
        c("Shape","Rate"),
      paste0("Param_",seq_along(coefs))
    )
    tmp <- data.frame(
      Distribution = model_name,
      Parameter = param_names[seq_along(coefs)],
      Value = coefs,
      row.names = NULL
    )
    result[[model_name]] <- tmp
  }
  dplyr::bind_rows(result)
}

