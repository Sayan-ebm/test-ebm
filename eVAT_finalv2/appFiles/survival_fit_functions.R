#########################################################
#### 1. ADAPTED REFERENCE RECONSTRUCTION FUNCTION #######
#########################################################
reconstruct_ipd <- function(n_at_risk, p_survival){
  n_at_risk <- as.data.frame(n_at_risk)
  p_survival <- as.data.frame(p_survival)
  colnames(n_at_risk) <- c("Time","n_at_risk")
  colnames(p_survival) <- c("Time","Surv")
  t_S <- as.numeric(p_survival$Time)
  S   <- as.numeric(p_survival$Surv)
  t_risk <- as.numeric(n_at_risk$Time)
  n_risk <- as.numeric(n_at_risk$n_at_risk)
  keep <- complete.cases(t_S,S)
  t_S <- t_S[keep]
  S   <- S[keep]
  ord <- order(t_S)
  t_S <- t_S[ord]
  S   <- S[ord]
  #################################################
  ## ADD TIME 0 IF MISSING
  #################################################
  if(min(t_S) > 0){
    t_S <- c(0, t_S)
    S   <- c(1, S)
  }
  #################################################
  ## FORCE MONOTONICITY
  #################################################
  for(i in 1:(length(S)-1)){
    if(t_S[i+1] <= t_S[i])
      t_S[i+1] <- t_S[i] + 1e-6
    if(S[i+1] > S[i])
      S[i+1] <- S[i]
  }
  #################################################
  ## INTERVAL MAPPING
  #################################################
  lower <- upper <- rep(NA,length(t_risk))
  for(i_time in seq_along(t_risk)){
    if(i_time != length(t_risk)){
      idx <- which(
        t_S < t_risk[i_time + 1] &
          t_S >= t_risk[i_time]
      )
      if(length(idx)==0) next
      lower[i_time] <- min(idx)
      upper[i_time] <- max(idx)
    } else {
      idx <- which(t_S >= t_risk[i_time])
      if(length(idx)==0){
        lower[i_time] <- length(t_S)
      } else {
        lower[i_time] <- min(idx)
      }
      upper[i_time] <- length(t_S)
    }
  }
  #################################################
  ## INITIALISE
  #################################################
  n_int <- length(n_risk)
  n_t   <- upper[n_int]
  n_censor <- rep(0,n_int)
  n_hat <- rep(n_risk[1] + 1,n_t + 1)
  cen <- rep(0,n_t)
  d <- rep(0,n_t)
  km_hat <- rep(1,n_t)
  last_i <- rep(1,n_int)
  #################################################
  ## MAIN RECONSTRUCTION
  #################################################
  if(n_int > 1){
    for(i in 1:(n_int-1)){
      n_censor[i] <-
        round(
          n_risk[i] *
            S[lower[i+1]] /
            max(S[lower[i]],1e-10)
          -
            n_risk[i+1]
        )
      .iter_guard <- 0L
      .iter_guard_max <- 1000L
      while(
        (
          n_hat[lower[i+1]] > n_risk[i+1]
        ) ||
        (
          n_hat[lower[i+1]] < n_risk[i+1] &&
          n_censor[i] > 0
        )
      ){
        .iter_guard <- .iter_guard + 1L
        if(.iter_guard > .iter_guard_max){
          warning(
            paste0(
              "reconstruct_ipd: reconstruction did not converge for interval ",
              i, " after ", .iter_guard_max,
              " iterations; using best available estimate."
            )
          )
          break
        }
        if(n_censor[i] <= 0){
          cen[lower[i]:upper[i]] <- 0
          n_censor[i] <- 0
        }
        if(n_censor[i] > 0){
          cen_t <- numeric(n_censor[i])
          for(j in seq_len(n_censor[i])){
            cen_t[j] <-
              t_S[lower[i]] +
              j *
              (
                t_S[lower[i+1]] -
                  t_S[lower[i]]
              ) /
              (n_censor[i] + 1)
          }
          cen[lower[i]:upper[i]] <-
            hist(
              cen_t,
              breaks = t_S[
                lower[i]:lower[i+1]
              ],
              plot = FALSE
            )$counts
        }
        n_hat[lower[i]] <- n_risk[i]
        last <- last_i[i]
        for(k in lower[i]:upper[i]){
          if(i==1 && k==lower[i]){
            d[k] <- 0
            km_hat[k] <- 1
          } else {
            d[k] <-
              round(
                n_hat[k] *
                  (
                    1 -
                      (
                        S[k] /
                          max(km_hat[last],1e-10)
                      )
                  )
              )
            d[k] <- max(d[k],0)
            km_hat[k] <-
              km_hat[last] *
              (
                1 -
                  d[k] /
                  max(n_hat[k],1e-10)
              )
          }
          n_hat[k+1] <-
            n_hat[k] -
            d[k] -
            cen[k]
          if(d[k] != 0)
            last <- k
        }
        n_censor[i] <-
          n_censor[i] +
          (
            n_hat[lower[i+1]] -
              n_risk[i+1]
          )
      }
      if(n_hat[lower[i+1]] < n_risk[i+1]){
        n_risk[i+1] <- n_hat[lower[i+1]]
      }
      last_i[i+1] <- last
    }
  }
  #################################################
  ## FINAL INTERVAL
  #################################################
  if(n_int > 1){
    n_censor[n_int] <-
      min(
        round(
          sum(n_censor[1:(n_int-1)]) *
            (
              t_S[upper[n_int]] -
                t_S[lower[n_int]]
            ) /
            max(
              t_S[upper[n_int-1]] -
                t_S[lower[1]],
              1e-10
            )
        ),
        n_risk[n_int]
      )
  } else {
    n_censor[n_int] <- 0
  }
  if(n_censor[n_int] > 0){
    cen_t <- numeric(n_censor[n_int])
    for(j in seq_len(n_censor[n_int])){
      cen_t[j] <-
        t_S[lower[n_int]] +
        j *
        (
          t_S[upper[n_int]] -
            t_S[lower[n_int]]
        ) /
        (n_censor[n_int] + 1)
    }
    cen[
      lower[n_int]:(upper[n_int]-1)
    ] <-
      hist(
        cen_t,
        breaks =
          t_S[
            lower[n_int]:
              upper[n_int]
          ],
        plot = FALSE
      )$counts
  }
  n_hat[lower[n_int]] <- n_risk[n_int]
  last <- last_i[n_int]
  for(k in lower[n_int]:upper[n_int]){
    if(km_hat[last] != 0){
      d[k] <-
        round(
          n_hat[k] *
            (
              1 -
                (
                  S[k] /
                    km_hat[last]
                )
            )
        )
      d[k] <- max(d[k],0)
    } else {
      d[k] <- 0
    }
    km_hat[k] <-
      km_hat[last] *
      (
        1 -
          d[k] /
          max(n_hat[k],1e-10)
      )
    n_hat[k+1] <-
      n_hat[k] -
      d[k] -
      cen[k]
    if(n_hat[k+1] < 0){
      n_hat[k+1] <- 0
      cen[k] <- n_hat[k] - d[k]
    }
    if(d[k] != 0)
      last <- k
  }
  #################################################
  ## EVENT/CENSOR TABLE
  #################################################
  reconstructed <- data.frame(
    time = t_S,
    event = d,
    censor = cen
  )
  reconstructed <-
    reconstructed[
      reconstructed$event > 0 |
        reconstructed$censor > 0,
    ]
  #################################################
  ## EXPAND TO PATIENT LEVEL IPD
  #################################################
  event_rows <- do.call(
    rbind,
    lapply(
      seq_len(nrow(reconstructed)),
      function(i){
        if(reconstructed$event[i] == 0)
          return(NULL)
        data.frame(
          time =
            rep(
              reconstructed$time[i],
              reconstructed$event[i]
            ),
          status = 1
        )
      }
    )
  )
  censor_rows <- do.call(
    rbind,
    lapply(
      seq_len(nrow(reconstructed)),
      function(i){
        if(reconstructed$censor[i] == 0)
          return(NULL)
        data.frame(
          time =
            rep(
              reconstructed$time[i],
              reconstructed$censor[i]
            ),
          status = 0
        )
      }
    )
  )
  final_ipd <- rbind(event_rows, censor_rows)
  final_ipd <-
    final_ipd[
      order(
        final_ipd$time,
        -final_ipd$status
      ),
    ]
  #################################################
  ## REMOVE ZERO TIMES
  #################################################
  final_ipd <- final_ipd[
    final_ipd$time > 0,
  ]
  rownames(final_ipd) <- NULL
  final_ipd
}

# fit_parametric_models <- function(ipd){
#   library(flexsurv)
#   library(survival)
#   safe_fit <- function(dist){
#     tryCatch({
#       cat("\nTrying:", dist, "\n")
#       fit <- flexsurvreg(
#         Surv(time, status) ~ 1,
#         data = ipd,
#         dist = dist
#       )
#       cat("SUCCESS:", dist, "\n")
#       fit
#     }, error = function(e){
#       cat(
#         "FAILED:",
#         dist,
#         "\nReason:",
#         e$message,
#         "\n\n"
#       )
#       NULL
#     })
#   }
#   models <- list(
#     Exponential = safe_fit("exp"),
#     Weibull     = safe_fit("weibull"),
#     Gompertz    = safe_fit("gompertz"),
#     Lognormal   = safe_fit("lnorm"),
#     Loglogistic = safe_fit("llogis"),
#     Gamma       = safe_fit("gamma")
#   )
#   models[!sapply(models, is.null)]
# }

fit_parametric_models <- function(ipd){
  safe_fit <- function(dist){
    tryCatch({
      cat("\nTrying:", dist, "\n")
      fit <- flexsurvreg(Surv(time, status) ~ 1, data = ipd, dist = dist)
      cat("SUCCESS:", dist, "\n")
      cat("\nParameters:\n")
      print(fit$res)
      fit
    }, error = function(e){
      cat("FAILED:", dist, "\nReason:", e$message, "\n")
      NULL
    })
  }
  models <- list(
    Exponential = safe_fit("exp"),
    Weibull     = safe_fit("weibull"),
    Gompertz    = safe_fit("gompertz"),
    Lognormal   = safe_fit("lnorm"),
    Loglogistic = safe_fit("llogis"),
    Gamma       = safe_fit("gamma")
  )
  models[!sapply(models, is.null)]
}

extract_model_parameters <- function(models){
  do.call(
    rbind,
    lapply(names(models), function(model_name){
      fit <- models[[model_name]]
      pars <- fit$res
      data.frame(
        Model = model_name,
        Parameter = rownames(pars),
        Estimate = pars[, "est"],
        L95 = pars[, "L95%"],
        U95 = pars[, "U95%"],
        SE = pars[, "se"],
        row.names = NULL
      )
    })
  )
}

#########################################################
#### Extract Selected Model Parameters ##################
#########################################################
extract_selected_model_parameters <- function(
    fit,
    model_name
){
  coef_values <- as.numeric(coef(fit))
  param_names <- switch(
    model_name,
    "Exponential" = c("Log Rate"),
    "Weibull" = c("Log Shape", "Log Scale"),
    "Gamma" = c("Log Shape", "Log Rate"),
    "Lognormal" = c("Mu", "Log Sigma"),
    "Loglogistic" = c("Log Shape", "Log Scale"),
    "Gompertz" = c("Shape", "Rate"),
    paste0("Param_", seq_along(coef_values))
  )
  coef_table <- data.frame(
    Model = model_name,
    Type = "Coefficient",
    Parameter = param_names[seq_along(coef_values)],
    Value = coef_values
  )
  res_table <- data.frame(
    Model = model_name,
    Type = "Estimate",
    Parameter = rownames(fit$res),
    Value = fit$res[, "est"]
  )
  rbind(coef_table, res_table)
}

#########################################################
#### Create fit table  ##################################
#########################################################
create_fit_table <- function(models){
  if (length(models) == 0) stop("No models fitted.")
  data.frame(
    Model = names(models),
    AIC = round(sapply(models, AIC), 2),
    BIC = round(sapply(models, BIC), 2)
  ) |> dplyr::arrange(AIC)
}

#########################################################
#### Survival Predictions ###############################
#########################################################
# generate_survival_predictions <- function(models, horizon){
#   grid <- seq(0, horizon, by = 1)
#   dplyr::bind_rows(lapply(names(models), function(m){
#     fit <- models[[m]]
#     tryCatch({
#       s <- summary(fit, t = grid, type = "survival")
#       data.frame(
#         time = s[[1]]$time,
#         surv = s[[1]]$est,
#         model = m
#       )
#     }, error = function(e) NULL)
#   }))
# }

generate_survival_predictions <- function(
    models,
    horizon,
    selected_models = NULL
){
  grid <- seq(0, max(horizon, 0), by = 1)
  if(is.null(selected_models)){
    selected_models <- names(models)
  }
  dplyr::bind_rows(
    lapply(selected_models, function(model_name){
      fit <- models[[model_name]]
      pars <- fit$res
      surv <- switch(
        model_name,
        "Exponential" = {
          rate <- pars["rate","est"]
          exp(-rate * grid)
        },
        "Weibull" = {
          shape <- pars["shape","est"]
          scale <- pars["scale","est"]
          exp(-(grid/scale)^shape)
        },
        "Gompertz" = {
          shape <- pars["shape","est"]
          rate  <- pars["rate","est"]
          exp(-(rate/shape) *(exp(shape * grid) - 1))
        },
        "Lognormal" = {
          meanlog <- pars["meanlog","est"]
          sdlog   <- pars["sdlog","est"]
          1 - plnorm(grid, meanlog, sdlog)
        },
        "Loglogistic" = {
          shape <- pars["shape","est"]
          scale <- pars["scale","est"]
          1 / (1 + (grid/scale)^shape)
        },
        "Gamma" = {
          shape <- pars["shape","est"]
          rate  <- pars["rate","est"]
          1 - pgamma(grid, shape = shape, rate = rate)
        }
      )
      data.frame(time = grid, surv = surv, model = model_name)
    })
  )
}

#########################################################
#### HR transformation ##################################
#########################################################
apply_hr_to_survival <- function(surv_df, hr){
  surv_df$surv <- surv_df$surv ^ hr
  surv_df
}
