#### server-side utility functions ####

# Manual numeric entry guard -----
#' Prevents a numericInput from ever staying blank/NA/out-of-range.
#'
#' Shiny's numericInput sends NA to the server the moment a user deletes
#' every digit in the box (before they type a replacement value). Any
#' reactive/observer that goes on to do arithmetic, indexing, or
#' comparisons directly on that NA (e.g. `if (input$x > 0)`,
#' `1:input$x`, `matrix(..., nrow = input$x)`) will error, and because
#' many of these live inside plain observers (which have no output to
#' absorb the error) that error can terminate the whole session, which
#' shows up to the user as the app freezing or disconnecting.
#'
#' This helper watches a single input and, whenever it becomes blank,
#' NA, non-numeric, or outside an allowed range, immediately snaps it
#' back to a safe default and lets the user know - so downstream code
#' never actually sees the invalid value.
#'
#' @param input Shiny's `input` object (pass through from the caller)
#' @param session Shiny's `session` object (pass through from the caller)
#' @param input_id The id of the numericInput to guard
#' @param default Value to reset to when the input is invalid
#' @param min Optional minimum allowed value
#' @param max Optional maximum allowed value
#' @param label Optional human-readable label used in the warning message
guardNumericInput = function(input, session, input_id, default = 0, min = NULL, max = NULL, label = NULL) {
  observeEvent(input[[input_id]], {
    val = input[[input_id]]
    invalid =
      is.null(val) || length(val) == 0 || !is.numeric(val) || is.na(val) ||
      (!is.null(min) && val < min) ||
      (!is.null(max) && val > max)
    if (invalid) {
      updateNumericInput(session, input_id, value = default)
      showNotification(
        paste0(
          if (!is.null(label)) paste0(label, ": ") else "",
          "This field cannot be left blank or out of range. It has been reset to ",
          default, " - please enter a valid value before running the model."
        ),
        type = "warning",
        duration = 6
      )
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
}

# Input data checking -----
checkInputData = function(dat, errors) {
  # Check for missing variables
  errors$missing[1] = all(is.na(dat[, 1]))  # time
  errors$missing[2] = all(is.na(dat[, 2]))  # event
  errors$missing[3] = all(is.na(dat[, 3]))  # treatment
  
  if (all(errors$missing == FALSE)) {
    # Check for insufficient amount of data
    errors$insuff[1] = sum(!is.na(dat[, 1])) <= 1
    errors$insuff[2] = sum(!is.na(dat[, 2])) <= 1
    errors$insuff[3] = sum(!is.na(dat[, 3])) <= 0
    
    # Error checking for sorting and NAs
    errors$sort[1] = is.unsorted(dat[, 1] %>% na.omit)  # time (should increase strictly)
    errors$sort[2] = is.unsorted(rev(dat[, 2] %>% na.omit), strictly = FALSE)  # event (should not increase)
    errors$sort[3] = FALSE # treatment, typically not sorted
    errors$NAs[1] = any(is.na(dat[, 1]))  # time
    errors$NAs[2] = any(is.na(dat[, 2]))  # event
    errors$NAs[3] = any(is.na(dat[, 3]))  # treatment
  }
  
  errors$allPassed = all(errors$missing == FALSE & errors$insuff == FALSE & errors$sort == FALSE & errors$NAs == FALSE)
  return(errors)
}

# Kaplan-Meier analysis -----
runKaplanMeierAnalysis = function(dat) {
  require(survival)
  fit <- survfit(Surv(time, event) ~ treatment, data = dat)
  return(list(fit = fit, data = dat))
}

# Hazard Ratio calculation -----
calculateHazardRatio = function(dat) {
  require(survival)
  cox_model <- coxph(Surv(time, event) ~ treatment, data = dat)
  hr <- summary(cox_model)$coefficients
  ci <- summary(cox_model)$conf.int
  return(list(hr = hr, ci = ci))
}

# Error message utility -----
errorMessage = function(session, text = "Incorrect input type entered, please try again", title = "Input error") {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "error"
  )
}

# Help message utility -----
helpMessage = function(session, text = "Help message", title = "Help") {
  shinyWidgets::sendSweetAlert(
    session = session,
    title = title,
    text = text,
    type = "info"
  )
}

# Plot enhancements -----
plotlyConfigDefault = function(...) {
  plotly::config(
    ...,
    edits = list(legendPosition = TRUE),
    displayModeBar = "hover",
    displaylogo = FALSE,
    modeBarButtons = list(list(
      "toImage",
      "zoomIn2d",
      "zoomOut2d",
      "resetScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian"))
  )
}

#' Produces a list of <p> tags based on an input list
#' @param xList List of text objects
#' @return List of <p> tags from the specified text
#' @author Kashif Siddiqui \email{kashif.siddiqui@ebmhealth.in}
pList = function(xList) {
  x = ""
  xTags = names(xList)
  if (is.null(xTags)) xTags = rep("p", length(xList))
  for (i in 1:length(xList)) {
    if (xTags[i] == "p")
      x = paste0(x, paste0(p(xList[[i]]), "\n"))
    else if (xTags[i] == "ul")
      x = paste0(x, tags$ul(lapply(xList[[i]], function(x) tags$li(x))))
    else if (xTags[i] == "ol")
      x = paste0(x, tags$ol(lapply(xList[[i]], function(x) tags$li(x))))
  }
  return(HTML(x))
}

#' Applies a CSS class to an empty inputId
#' @param inputId The input id to be validated
#' @param inputValue The value of the input to be validated
#' @param invalidCSS The CSS class to be applied if invalid. Removal is left to validateThenCSS
#' @return Nothing
#' @author kashif Siddiqui \email{kashif.siddiqui@ebmhealth.in}
emptyThenCSS = function(inputId, inputValue, invalidCSS = "invalidInput") {
  if (!is.null(inputValue)) {
    if (is.character(inputValue)) {
      if (inputValue == "") {
        addCssClass(inputId, invalidCSS)
        shiny:::reactiveStop(paste0(inputId, ", validation"))
      }
    } else {
      if (is.na(inputValue)) {
        addCssClass(inputId, invalidCSS)
        shiny:::reactiveStop(paste0(inputId, ", validation"))
      }
    }
  } else {
    warning(paste0("Input ", inputId, " is NULL but was passed to emptyThenCSS"))
  }
}


#' Help button widget
#' @param inputId Button widget input ID
#' @param padding CSS padding to be applied in the containing span of the button
#' @param label Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param ... additional parameters to be passed to the CSS style of the span containing the button
#' @return Help button widget
#' @author Kashif Siddiqui \email{Andrea.Berardi@@PAREXEL.com}
helpButton = function(inputId, padding = "0px 5px 0px 5px", label = "?", labIcon = NULL, size = "xs", style = "simple", color = "success", ...) {
  tags$span(
    style = paste0("padding: ", padding, ";"),
    actionBttnPlus(
      inputId = inputId,
      label = strong(label), icon = labIcon, size = size, style = style, color = color, ...
    )
  )
}

#' Reference button widget
#' @param inputId Button widget input ID
#' @param padding CSS padding to be applied in the containing span of the button
#' @param label Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param ... additional parameters to be passed to the CSS style of the span containing the button
#' @return Reference button widget
#' @author Kashif Siddiqui \email{Andrea.Berardi@@PAREXEL.com}
refButton = function(inputId, padding = "0px 5px 0px 5px", label = NULL, labIcon = icon("info"), size = "xs", style = "simple", color = "primary", ...) {
  tags$span(
    style = paste0("padding: ", padding, ";"),
    actionBttnPlus(
      inputId = inputId,
      label = label, icon = labIcon, size = size, style = style, color = color, ...
    )
  )
}

#' Parameter label including the corresponding reference button for a specific input widget, e.g. numericInput
#' @param id The input ID of the corresponding input widget
#' @param label Label text
#' @param bttnPadding CSS padding to be applied in the containing span of the button
#' @param bttnLabel Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param popover named list specifying the parameters passed to bsPopover
#' @param ... additional parameters to be passed to the returned tagList
#' @return tagList including the label, reference button and popover JS script
#' @author Kashif Siddiqui \email{Kashif.Siddiqui@ebmhealth.in}
refLabel = function(id, label = "", bttnPadding = "0px 5px 0px 5px", bttnLabel = NULL, labIcon = icon("info"), size = "xs", style = "simple", color = "primary", popover = NULL, ...) {
  if (is.null(popover) || length(popover) == 0) {
    popover = NULL
  } else {
    popover = bsPopover(
      id = paste0(id, "Ref"), 
      title = popover$title, content = popover$content, 
      placement = popover$placement, trigger = popover$trigger, 
      options = popover$options)
  }
  tagList(
    tags$label(
      `for` = id,
      label, class = "refLabel",
      refButton(paste0(id, "Ref"), padding = bttnPadding,
                label = bttnLabel, labIcon = labIcon, 
                size = size, style = style, color = color, 
                stylePlus = "vertical-align: top; font-size: 9px;")
    ),
    popover,
    ...
  )
}

#' Parameter label including a help button for a specific input widget, e.g. numericInput
#' @param id The input ID of the corresponding input widget
#' @param label Label text
#' @param bttnPadding CSS padding to be applied in the containing span of the button
#' @param bttnLabel Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param popover named list specifying the parameters passed to bsPopover
#' @param ... additional parameters to be passed to the returned tagList
#' @return tagList including the label, reference button and popover JS script
#' @author Kashif Siddiqui \email{Andrea.Berardi@@PAREXEL.com}
helpLabel = function(id, label = "", bttnPadding = "0px 5px 0px 5px", bttnLabel = "?", labIcon = NULL, size = "xs", style = "simple", color = "success", popover = NULL, ...) {
  if (is.null(popover) || length(popover) == 0) {
    popover = NULL
  } else {
    popover = bsPopover(
      id = paste0(id, "Help"), 
      title = popover$title, content = popover$content, 
      placement = popover$placement, trigger = popover$trigger, 
      options = popover$options)
  }
  tagList(
    tags$label(
      `for` = id,
      label, class = "refLabel",
      helpButton(paste0(id, "Help"), padding = bttnPadding,
                 label = bttnLabel, labIcon = labIcon, 
                 size = size, style = style, color = color, 
                 stylePlus = "vertical-align: top; font-size: 9px;")
    ),
    popover,
    ...
  )
}

#' Text including a reference button referred to a specific Ref parameter value, not necessarily associated to any specific individual input
#' @param id The input ID of the corresponding input, on which the Ref actionBttn is based as paste0(id, "Ref")
#' @param title Plain text to which the reference button should be attached
#' @param bttnPadding CSS padding to be applied in the containing span of the button
#' @param bttnLabel Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param popover named list specifying the parameters passed to bsPopover
#' @param smallFont Boolean, whether the button should use a 9px font-size CSS style
#' @param ... additional parameters to be passed to the returned tagList
#' @return tagList including the label, reference button and popover JS script
#' @author Kashif Siddiqui \email{Andrea.Berardi@@PAREXEL.com}
refText = function(id, title = "", bttnPadding = "0px 5px 0px 5px", bttnLabel = NULL, labIcon = icon("info"), size = "xs", style = "simple", color = "primary", popover = NULL, smallFont = TRUE, ...) {
  if (is.null(popover) || length(popover) == 0) {
    popover = NULL
  } else {
    popover = bsPopover(
      id = paste0(id, "Ref"), 
      title = popover$title, content = popover$content, 
      placement = popover$placement, trigger = popover$trigger, 
      options = popover$options)
  }
  tagList(
    tags$span(
      title,
      refButton(paste0(id, "Ref"), padding = bttnPadding,
                label = bttnLabel, labIcon = labIcon, 
                size = size, style = style, color = color, 
                stylePlus = paste0("vertical-align: top; max-height: 20px;", ifelse(smallFont, " font-size: 9px;", "")))
    ),
    popover,
    ...
  )
}

#' Parameter label including a help button for a specific input widget, e.g. numericInput
#' @param id The input ID of the corresponding input widget
#' @param label Label text
#' @param bttnPadding CSS padding to be applied in the containing span of the button
#' @param bttnLabel Button label
#' @param labIcon Button label icon
#' @param size actionBttn size, specified as string
#' @param style actionBttn style, as string
#' @param color actionBttn color, as string
#' @param popover named list specifying the parameters passed to bsPopover
#' @param ... additional parameters to be passed to the returned tagList
#' @return tagList including the label, reference button and popover JS script
#' @author Kashif Siddiqui \email{Andrea.Berardi@@PAREXEL.com}
helpLabel = function(id, label = "", bttnPadding = "0px 5px 0px 5px", bttnLabel = "?", labIcon = NULL, size = "xs", style = "simple", color = "success", popover = NULL, ...) {
  if (is.null(popover) || length(popover) == 0) {
    popover = NULL
  } else {
    popover = bsPopover(
      id = paste0(id, "Help"), 
      title = popover$title, content = popover$content, 
      placement = popover$placement, trigger = popover$trigger, 
      options = popover$options)
  }
  tagList(
    tags$label(
      `for` = id,
      label, class = "refLabel",
      helpButton(paste0(id, "Help"), padding = bttnPadding,
                 label = bttnLabel, labIcon = labIcon, 
                 size = size, style = style, color = color, 
                 stylePlus = "vertical-align: top; font-size: 9px;")
    ),
    popover,
    ...
  )
}

#' Text including a reference button referred to a specific Ref parameter value, not necessarily associated to any specific individual input
#' @param id The input ID of the corresponding input, on which the Ref actionBttn is based as paste0(id, "Ref")
#' @param label Label text
#' @param bttnPadding CSS padding to be applied in the containing span of the button
#' @param refBttnLabel Button label
#' @param refIcon Button label icon
#' @param refSize actionBttn size, specified as string
#' @param refStyle actionBttn style, as string
#' @param refColor actionBttn color, as string
#' @param refPopover named list specifying the parameters passed to bsPopover
#' @param refSmallFont Boolean, whether the button should use a 9px font-size CSS style
#' @param helpBttnLabel Button label
#' @param helpIcon Button label icon
#' @param helpSize actionBttn size, specified as string
#' @param helpStyle actionBttn style, as string
#' @param helpColor actionBttn color, as string
#' @param helpPopover named list specifying the parameters passed to bsPopover
#' @param helpSmallFont Boolean, whether the button should use a 9px font-size CSS style
#' @param ... additional parameters to be passed to the returned tagList
#' @param refFirst Boolean, should the reference or the help button come first? Defaults to reference first
#' @return tagList including the label, reference and help buttons and associated popover JS scripts
#' @author Kashif Siddiqui \email{Kashif.Siddiqui@ebmhealth.in}
refHelpLabel = function(id, label = "", refPadding = "0px 5px 0px 5px", helpPadding = "0px 5px 0px 0px",
                        refBttnLabel = NULL, refIcon = icon("info"), refSize = "xs", refStyle = "simple",
                        refColor = "primary", refPopover = NULL, refSmallFont = TRUE,
                        helpBttnLabel = "?", helpIcon = NULL, helpSize = "xs", helpStyle = "simple",
                        helpColor = "success", helpPopover = NULL, helpSmallFont = TRUE,
                        ..., refFirst = TRUE) {
  if (!is.logical(refFirst) || length(refFirst) == 0)
    refFirst = TRUE
  if (is.null(refPopover) || length(refPopover) == 0) {
    refPopover = NULL
  } else {
    refPopover = bsPopover(
      id = paste0(id, "Ref"), 
      title = refPopover$title, content = refPopover$content, 
      placement = refPopover$placement, trigger = refPopover$trigger, 
      options = refPopover$options)
  }
  if (is.null(helpPopover) || length(helpPopover) == 0) {
    helpPopover = NULL
  } else {
    helpPopover = bsPopover(
      id = paste0(id, "Help"),
      title = helpPopover$title, content = helpPopover$content, 
      placement = helpPopover$placement, trigger = helpPopover$trigger, 
      options = helpPopover$options)
  }
  refB = refButton(
    paste0(id, "Ref"), padding = refPadding,
    label = refBttnLabel, labIcon = refIcon, 
    size = refSize, style = refStyle, color = refColor, 
    stylePlus = paste0("vertical-align: top; max-height: 20px;", ifelse(refSmallFont, " font-size: 9px;", "")))
  helpB = helpButton(
    paste0(id, "Help"), padding = helpPadding,
    label = helpBttnLabel, labIcon = helpIcon, 
    size = helpSize, style = helpStyle, color = helpColor, 
    stylePlus = paste0("vertical-align: top; max-height: 20px;", ifelse(helpSmallFont, " font-size: 9px;", "")))
  if (refFirst) {
    return(
      tagList(
        tags$label(
          `for` = id,
          label, class = "refLabel",
          refB, helpB
        ),
        refPopover,
        helpPopover,
        ...
      )
    )
  } else {
    return(
      tagList(
        tags$label(
          `for` = id,
          label, class = "refLabel",
          helpB, refB
        ),
        refPopover,
        helpPopover,
        ...
      )
    )
  }
}

#' Returns 3-letter distribution name from full-length distribution name
#' @param distName Survival distribution name
#' @return 3-letter distribution name
#' @author Kashif Siddiqui \email{kashif.siddiqui@ebmhealth.in}
distNameTo3 = function(distName) {
  distName %<>% 
    tolower() %>%
    {gsub(pattern = "-* *", x = ., replacement = "")}
  if (distName %in% distNames3)
    return(distName)
  distName = switch(
    distName,
    exponential = "exp",
    weibull = "wei",
    gompertz = "gom",
    gamma = "gam",
    lognormal = "lno",
    loglogistic = "llo",
    gengamma = "gga",
    generalizedgamma = "gga",
    generalisedgamma = "gga",
    genf = "gef",
    generalizedf = "gef",
    generalisedf = "gef")
    return(distName)
}

#' Returns 3-letter distribution name from full-length spline names
#' @param distName Spline distribution name
#' @return 3-letter distribution name
#' @author Kashif Siddiqui \email{kashif.siddiqui@ebmhealth.in}
splineNameTo3 = function(sp) {
  one = "s"
  two = if (grepl("hazard", sp)) "H" 
  else if (grepl("odds", sp)) "O" 
  else if (grepl("(probit|normal)", sp)) "P" 
  else "?"
  three = gsub(".*?([[:digit:]]+).*","\\1",sp)
  return(paste0(one, two, three))
}

#' Returns 3-letter distribution name from full-length survival distribution names
#' Calling either distNameTo3 or splineNameTo3
#' @param survName Survival distribution name
#' @return 3-letter distribution name
#' @author Kashif Siddiqui \email{kashif.siddiqui@ebmhealth.in}
survNameTo3 = function(survName) {
  if (grepl("[Ss]pline", survName))
    return(splineNameTo3(survName))
  return(distNameTo3(survName))
}





hline.plotly <- function(y = 0, color = "grey50") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = color, dash = "dash"))
}

plotlyConfigDefault <- function(...) {
  if (utils::packageVersion("plotly") >= package_version("4.9.0")) {
    plotly::config(
      ...,
      edits = list(legendPosition = TRUE),
      displayModeBar = "hover",
      displaylogo = FALSE,
      modeBarButtons = list(list(
        "toImage",
        "zoomIn2d",
        "zoomOut2d",
        "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian")))
  } else {
    plotly::config(
      ...,
      edits = list(legendPosition = TRUE),
      displayModeBar = "hover",
      displaylogo = FALSE,
      collaborate = FALSE,
      modeBarButtons = list(list(
        "toImage",
        "zoomIn2d",
        "zoomOut2d",
        "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian")))
  }
}
