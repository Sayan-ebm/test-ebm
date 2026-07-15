#########################################################
#### Plotting functions ######################
#########################################################
survival_colours <- c(
  Exponential = "#1f77b4",
  Weibull = "#ff7f0e",
  Gompertz = "#2ca02c",
  Lognormal = "#d62728",
  Loglogistic = "#9467bd",
  Gamma = "#8c564b"
)

plot_survival_models <- function(
    preds,
    selected_model = NULL,
    horizon = NULL,
    validation_data = NULL,
    validation_time = NULL,
    validation_prob = NULL
){
  p <- plotly::plot_ly()
  for(m in unique(preds$model)){
    df <- preds[preds$model == m, ]
    width <- 2
    opacity <- 0.8
    if(
      !is.null(selected_model) &&
      length(selected_model) == 1 &&
      m == selected_model
    ) {
      width <- 5
      opacity <- 1
    }
    p <- p %>%
      plotly::add_lines(
        data = df,
        x = ~time,
        y = ~surv,
        name = m,
        line = list(
          color = survival_colours[[m]] %||% "#000000",
          width = width
        ),
        opacity = opacity
      )
  }
  ###################################################
  # LONG TERM CURVE
  ###################################################
  if(!is.null(validation_data) &&
     nrow(validation_data) > 0){
    validation_data <- validation_data[
      order(validation_data$time),
    ]
    p <- p %>%
      plotly::add_lines(
        data = validation_data,
        x = ~time,
        y = ~surv,
        name = "Long-Term Validation",
        line = list(
          color = "purple",
          width = 4,
          dash = "dot"
        )
      )
  }
  ###################################################
  # VALIDATION POINT
  ###################################################
  if(!is.null(validation_time) &&
     !is.null(validation_prob)){
    p <- p %>%
      plotly::add_markers(
        x = validation_time,
        y = validation_prob,
        name = "Validation Point",
        marker = list(
          color = "red",
          size = 12,
          symbol = "x"
        )
      )
  }
  p %>%
    plotly::layout(
      xaxis = list(
        title = "Time (Months)",
        range = c(0, horizon)
      ),
      yaxis = list(
        title = "Survival Probability",
        range = c(0,1),
        tickmode = "array",
        tickvals = c(0, 0.25, 0.50, 0.75, 1.00),
        ticktext = sprintf("%.2f", seq(0, 1, by = 0.25)),
        ntick = 5,
        showgrid = TRUE,
        zeroline = FALSE
      )
    )
}
