##### CSS #####

titleWidth = 300
sidebarWidth = 250

greyColour = "#555454"
greyColourText = "#454545"

smActionButtonStyle = function(color = "#FFFFFF", backgroundColour = greyColour,
                               fontSize = "12px", padding = "2px 6px 3px", verticalAlign = "top") {
  paste0(
    "color: ", color, "; background-color: ", backgroundColour, ";",
    "font-size: ", fontSize, "; padding: ", padding, "; vertical-align: ", verticalAlign, ";"
  )
}

mdActionButtonStyle = function(color = "#FFFFFF", backgroundColour = greyColour,
                               fontSize = "14px", padding = "auto", verticalAlign = "auto") {
  paste0(
    "color: ", color, "; background-color: ", backgroundColour, ";",
    "font-size: ", fontSize, "; padding: ", padding, "; vertical-align: ", verticalAlign, ";"
  )
}

numericInputRow <- function(inputId, label, value = "", min = NA, max = NA, step = NA, width = NULL) {
  inputTag = tags$input(
    id = inputId, type = "number", class = "input-small form-control", value = shiny:::formatNoSci(value)
  )
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step
  div(
    class = "form-group shiny-input-container",
    style = paste(
      "display:inline-block;",
      ifelse(
        !is.null(width),
        paste0("width: ",validateCssUnit(width),";"),
        "")
    ),
    tags$label(label, `for` = inputId), 
    inputTag
  )
}

numericInputPlus = function(inputId, label, value, min = NA, max = NA, step = NA, width = NULL,
                            divClass = "form-group shiny-input-container", inputClass = "form-control", inputStyle = "") {
  value <- restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "number", class = inputClass, style = inputStyle,
                         value = shiny:::formatNoSci(value))
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  div(class = divClass, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"),
    shiny:::`%AND%`(label,tags$label(label, `for` = inputId)),
    inputTag
  )
}

numericInputTagPlus = function(inputId, value, min = NA, max = NA, step = NA, width = NULL,
                               inputClass = "form-control", inputStyle = "") {
  value = restoreInput(id = inputId, default = value)
  inputTag = tags$input(id = inputId, type = "number", class = inputClass, style = inputStyle,
                        value = shiny:::formatNoSci(value))
  if (!is.na(min)) 
    inputTag$attribs$min = min
  if (!is.na(max)) 
    inputTag$attribs$max = max
  if (!is.na(step)) 
    inputTag$attribs$step = step
  return(inputTag)
}

CSS =
  tags$head(
    tags$style(HTML(paste0(
      "footer {
      position: fixed;
      left: 0;
      bottom: 0;
      width: 100%;
      text-align: right;
      font-size: small;
      display: table-row;
      width: 100%;
      color: grey;
      }

      .inactiveLink {
      pointer-events: none;
      cursor: default;
      }
      h1 {
      color: ", greyColourText, ";
      font-weight: bold;
      }
      h2 {
      color: ", greyColourText, ";
      font-weight: bold;
      }
      h2 {
      color: ", greyColourText, ";
      text-align: center;
      }
      h4 {
      color: ", greyColourText, ";
      font-weight: bold;
      }
      h4 {
      color: ", greyColourText, ";
      text-align: center;
      }
      h5 {
      color: ", greyColourText, ";
      }
      input[type= 'number'] {
      font-size:12px;
      }
      th {
      text-align: center;
      }
      .well {
      background: #FFFFFF;
      }

      .box-title {
      color: ",greyColourText,";
      }

      .box-solid .box-title {
      color: #FFFFFF;
      }

      .form-control {
      background: #F4F4F4;
      }

      .nav-tabs-custom .nav-tabs li.header {
      color: ", greyColourText, ";
      }

      .inlineInput {
      display: table;
      width: 100%;
      }

      .inlineInput label {
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      padding: 6px 6px 6px 0px;
      }

      .inlineInput .form-group {
      display: table-row;
      padding: 6px 12px;
      }

      .inlineSubInput {
      display: table;
      width: 100%;
      }

      .inlineSubInput label {
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      font-weight: 400;
      padding: 6px 12px;
      }

      .inlineSubInput .form-group {
      display: table-row;
      padding: 6px 12px;
      }

      .inlineSubInput .form-control {
      display: table-cell;
      }

      .dataTable thead {
      background-color: ",greyColourText,";
      color: #fff;
      }

      caption {
      padding-top: 8px;
      padding-bottom: 8px;
      color: #777;
      text-align: left;
      }

      p {
      text-align: justify;
      }

      .refLabel {
      height: 20px;
      }

      .refButton {
      font-size: 9px;
      }

      .handsontableEditor.autocompleteEditor, .handsontableEditor.autocompleteEditor .ht_master .wtHolder {
      min-height: 60px;
      }

      .invalidInput {
      background: red;
      }

      .greyBorder {
      border: 3px solid ", greyColour, ";
      padding: 15px;
      }

      .swal-text {
      text-align: justify;
      }
      .swal-title {
      color: ", greyColourText, ";
      }

      .swal-footer {
      text-align: center;
      }
      .dt-buttons {
      margin: 5px 0px 5px 0px;
      }

      /* Custom CSS for Navbar alignment */
      #navbar-custom {
        display: flex;
        justify-content: space-between;
        align-items: center;
        width: 100%;  /* Ensure it takes up full width */
      }

      #navbar-title {
        text-align: left;
        padding-left: 15px; /* Adjust for additional spacing */
      }

      #navbar-right {
        display: flex;
        align-items: center;
        margin-left: auto;  /* Push to the right */
        padding-right: 15px; /* Adjust for additional spacing */
      }

      #navbar-right .action-link {
        margin-right: 10px; /* Adjust for space between Help link and logo */
        color: ", greyColourText, ";
        text-decoration: none;
        font-size: 1em;
      }

      #navbar-right a img {
        margin-left: 10px; /* Adjust for space between Help link and logo */
      }

      /* Subtle, low-emphasis Reset to Default button used next to */
      /* Example download buttons on the Survival Data upload panels */
      .btn-reset-subtle {
      background-color: #eaf4fc;
      color: #5b7c99;
      border: 1px solid #cfe3f2;
      font-size: 11px;
      padding: 3px 10px;
      box-shadow: none;
      white-space: nowrap;
      }

      .btn-reset-subtle:hover,
      .btn-reset-subtle:focus {
      background-color: #d9ecfa;
      color: #3f6b8a;
      border-color: #b8daf0;
      }

      /* Icon-only reset-to-default button placed beside manual */
      /* numeric/text entry fields throughout the app. No label, */
      /* deliberately unobtrusive so it doesn't draw the eye. */
      .btn-reset-icon-only {
      background: transparent;
      border: none;
      color: #9aa7b4;
      padding: 2px 6px;
      font-size: 13px;
      line-height: 1;
      box-shadow: none;
      margin-left: 4px;
      vertical-align: middle;
      }

      .btn-reset-icon-only:hover,
      .btn-reset-icon-only:focus {
      color: #5b7c99;
      background-color: #eef3f7;
      border-radius: 50%;
      }

      /* Light-yellow 'Reset to Default' button used beside the */
      /* Intervention / Comparator box headers on the Treatment  */
      /* Costs tab, resetting every field within that section.   */
      .btn-reset-section-yellow {
      background-color: #FFF9E0;
      color: #8a6d1d;
      border: 1px solid #F5E1A4;
      font-size: 12px;
      font-weight: 600;
      padding: 4px 12px;
      box-shadow: none;
      white-space: nowrap;
      }

      .btn-reset-section-yellow:hover,
      .btn-reset-section-yellow:focus {
      background-color: #FFF2C2;
      color: #6b5316;
      border-color: #EBCD7A;
      }
      "
    ))),
    tags$script(HTML('
                     var dimension = [0, 0];
                     $(document).on("shiny:connected", function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange("dimension", dimension);
                     });
                     $(window).resize(function(e) {
                     dimension[0] = window.innerWidth;
                     dimension[1] = window.innerHeight;
                     Shiny.onInputChange("dimension", dimension);
                     });
                     '))
  )

# CSS = 
#   tags$head(
#     tags$style(HTML(paste0(
#       "footer {
#       position: fixed;
#       left: 0;
#       bottom: 0;
#       width: 100%;
#       text-align: right;
#       font-size: small;
#       display: table-row;
#       width: 100%;
#       color: grey;
#       }
#       
#       .inactiveLink {
#       pointer-events: none;
#       cursor: default;
#       }
# 
#       h1, h2, h4 {
#       color: ", greyColourText, ";
#       font-weight: bold;
#       text-align: center;
#       }
# 
#       h5 {
#       color: ", greyColourText, ";
#       }
# 
#       input[type= 'number'] {
#       font-size:12px;
#       }
# 
#       th {
#       text-align: center;
#       }
# 
#       .well {
#       background: #FFFFFF;
#       }
# 
#       .box-title {
#       color: ",greyColourText,";
#       }
# 
#       .box-solid .box-title {
#       color: #FFFFFF;
#       }
# 
#       .form-control {
#       background: #F4F4F4;
#       }
# 
#       .dataTable thead {
#       background-color: ",greyColourText,";
#       color: #fff;
#       }
# 
#       caption {
#       padding-top: 8px;
#       padding-bottom: 8px;
#       color: #777;
#       text-align: left;
#       }
# 
#       p {
#       text-align: justify;
#       }
# 
#       .greyBorder {
#       border: 3px solid ", greyColour, "; 
#       padding: 15px;
#       }
# 
#       .swal-title {
#       color: ", greyColourText, ";
#       }
# 
#       .dt-buttons {
#       margin: 5px 0px 5px 0px;
#       }
# 
#       /* ================================
#          INFO ICON SYSTEM (NEW)
#       ================================= */
# 
#       .info-icon {
#         cursor: pointer;
#         color: #3C8DBC;
#         margin-left: 6px;
#         font-size: 14px;
#         transition: all 0.2s ease-in-out;
#       }
# 
#       .info-icon:hover {
#         color: #1F4E79;
#         transform: scale(1.15);
#       }
# 
#       .tooltip-inner {
#         max-width: 320px;
#         text-align: left;
#         font-size: 12px;
#       }
# 
#       .info-box-light {
#         background: #F4F9FF;
#         border-left: 4px solid #3C8DBC;
#         padding: 10px;
#         border-radius: 6px;
#         font-size: 13px;
#         color: #444;
#       }
# 
#       /* Navbar */
#       #navbar-custom {
#         display: flex;
#         justify-content: space-between;
#         align-items: center;
#         width: 100%;
#       }
# 
#       #navbar-right {
#         display: flex;
#         align-items: center;
#         margin-left: auto;
#         padding-right: 15px;
#       }
#       "
#     ))),
#     tags$script(HTML('
#       var dimension = [0, 0];
#       $(document).on("shiny:connected", function(e) {
#         dimension[0] = window.innerWidth;
#         dimension[1] = window.innerHeight;
#         Shiny.onInputChange("dimension", dimension);
#       });
#       $(window).resize(function(e) {
#         dimension[0] = window.innerWidth;
#         dimension[1] = window.innerHeight;
#         Shiny.onInputChange("dimension", dimension);
#       });
#     '))
#   )


##### footer #####
footer = 
  tags$footer(
    paste0("Analysis running on Bayer ",appName," ",appVersion, ". "),
    HTML("Creator and maintainer: <a href='mailto:Kashif.Siddiqui@ebmhealth.in'>Bayer </a>.")
  )

##### logos #####
PXLLogo = list(
  img = function(height = "60px") {
    img(
      src = "EBM.svg",
      title = "EBM",
      height = height
    )
  },
  href = "https://ebmhealth.in/"
)

PXLLogoLi <- tags$li(
  class = "dropdown",
  tags$div(
    style = "display:flex; align-items:center; gap:12px; padding:5px 10px;",
    
    # EBM logo
    tags$a(
      href = PXLLogo$href,
      target = "_blank",
      id = "PXLLink",
      PXLLogo$img()
    ),
    
    # Bayer logo
    tags$a(
      href = "https://www.bayer.com/",
      target = "_blank",
      id = "BayerLink",
      tags$img(
        src = "bayer_logo.jpg",
        title = "Bayer",
        height = "40px"
      )
    )
  )
)

##### javascript CSS #####
JSTableFormatting = paste0(
  "function(settings, json) {","\n",
  "$(this.api().table().header()).css({'background-color':'",greyColour,"', 'color': '#fff'});","\n",
  "}"
)

##### debounce lengths #####
sDebounce = 500
mDebounce = 1000
lDebounce = 1500

##### modified actionBttn object #####
actionBttnPlus = function(inputId, label = NULL, icon = NULL, style = "unite", 
                          color = "default", size = "md", block = FALSE, no_outline = TRUE,
                          classPlus = "", stylePlus = "", ...) 
{
  value <- shiny::restoreInput(id = inputId, default = NULL)
  style <- match.arg(arg = style, choices = c("simple", "bordered", 
                                              "minimal", "stretch", "jelly", "gradient", "fill", "material-circle", 
                                              "material-flat", "pill", "float", "unite"))
  color <- match.arg(arg = color, choices = c("default", "primary", 
                                              "warning", "danger", "success", "royal"))
  size <- match.arg(arg = size, choices = c("xs", "sm", "md", "lg"))
  tagBttn <- htmltools::tags$button(
    id = inputId, type = "button", 
    class = classPlus, style = stylePlus,
    class = "action-button bttn", `data-val` = value, 
    class = paste0("bttn-", style), class = paste0("bttn-", size), class = paste0("bttn-", color), 
    list(icon, label), 
    class = if (block) "bttn-block", class = if (no_outline) "bttn-no-outline",
    ...)
  shinyWidgets:::attachShinyWidgetsDep(tagBttn, "bttn")
}

# Javascript rhandsontable validator function -----
hotColEmptyMinValidator = function(min, timeOut = 250) {
  paste0(
    "function(value, callback) {
      setTimeout(function(){
        if(value === '') {
          callback(true);
        }
        if(value < ",min,") {
          callback(false);
        }
        else {
          callback(true);
        }
      }, ",timeOut,")
    }"
  )
}
