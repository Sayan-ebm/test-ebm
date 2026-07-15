##### Welcome page UI #####

welcomePageNextButton = 
  "Click to enter"

tourText = c(
  "1" = HTML("<p>This is the landing page of the app. The application can be accessed by clicking the button below</p><p>To navigate this tour, press Enter or click Next</p>"),
  "2" = "Users can specify the unit of time of the input data my checking the relevant option",
  "3" = "Data files can be uploaded by clicking this button. A template file can be download from the application",
  "4" = "These buttons can be used to download data template, download the live data (if any modifications are made), and reset the app with the default data respectively. The default dataset can be used as a template for new files",
  "5" = "The uploaded data can be viewed in this table. Validation issues will be highlighted in red",
  "6" = "The kaplan-meier curve can be checked visually using this plot. The Hazard ratio and 95% CI from the IPD data is also reported for additional validation",
  "7" = "Once the dataset is uploaded on the platform, click this button. If the data pass all the validation checks, the survival analysis will be run according to the user's specifications"
)

welcomePageUI =
  wellPanel(
    fluidRow(column(
      width = 12,
      introBox(
         fluidRow(
        #    column(
        #   width = 8,offset = 2,
        #   img(
        #     src = "EBM.svg",
        #     style = "align-content: center; max-height: 100px; max-width: 100%; display: block; margin-left: auto; margin-right: auto;")
        # ),
        column(
          width = 12,
          tags$h2(
            "Welcome to the early Value Assessment Tool"
          )
        )),
        fluidRow(column(
          width = 12,
          div(
            tags$h4(
              "The purpose of this application is to assess the cost-effectiveness of an intervention against comparator"
            )
          )
        )),
        fluidRow(column(
          width = 8, offset = 2,
          tags$img(
            src = "model_diagram.jpg", 
            style = "align-content: center; max-height: 500px; max-width: 100%; display: block; margin-left: auto; margin-right:auto;")
        )),
        br(),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            actionButton(
              inputId = "bttnWelcomeNext",
              label = "Click to enter the eVAT",
              width = "100%",
              class = "btn-primary",
              icon = icon("arrow-right")
            )
          ),
          data.step = 1,
          data.intro = "Click here to enter the eVAT application."
        )
      ),
      tags$hr(),
      box(
        collapsible = FALSE, width = 12, status = "warning",
        p(
          "Please use a modern web browser such as ",
          a(href = "https://www.mozilla.org", "Mozilla Firefox", target = "_blank")," or ",
          a(href = "https://www.google.com/chrome/", "Google Chrome", target = "_blank"), "to view this application."),
        p(
          "Using Internet Explorer or the RStudio viewer might result in some application elements not being rendered correctly. Please see the user guide for additional details."
        )
      )
    ))
  )

# Welcome intro modal -----

welcomeModal <- modalDialog(
  fluidRow(column(
    width = 10, offset = 1,
    tags$h4(
      "Survival analysis platform help page",
      style = "align-text: center;"
    ),
    tags$hr(),
    fluidRow(
      column(
        6, actionButton(
          class = "actionButton",
          "about", "About",
          icon("question-circle"),
          width = "100%",
          class = "btn-info",
          style = mdActionButtonStyle()
        )
      ),
      column(
        6, actionButton(
          class = "actionButton",
          "tour", "Tour",
          icon("map"),
          width = "100%",
          class = "btn-info",
          style = mdActionButtonStyle()
        )
      )
    ),
    tags$br(),
    fluidRow(
      column(
        6, downloadButton(
          "downloadSpecDoc", "Documentation",
          style = "width: 100%;",
          class = "btn-info",
          style = mdActionButtonStyle()
        )
      ),
      column(
        6, actionButton(
          "quit", "Quit",
          icon("power-off"),
          onclick = "setTimeout(function(){window.close();},200);",
          width = "100%",
          class = "btn-info",
          style = mdActionButtonStyle()
        )
      )
    )
  )),
  size = "m", align = "center",
  easyClose = TRUE, fade = T, footer = NULL,
  style = "padding: 5%;"
)

# About modal -----

aboutModal <- modalDialog(
  fluidRow(column(
    width = 10,
    offset = 1,
    tags$h4("About",
            style = "align-text: center;"),
    tags$hr(),
    fluidRow(column(
      12,
      p(
        "This platform takes IPD data and then perform the analysis for dependent and independent parametric and spline models "
      ),
      tags$hr(),
      p(
        "Developed on latest version of R (v4.0.2) by ",
        a(href = "https://ebmhealth.in/", "EBM Health", target = "_blank")
      ),
      p(
        "For any issues related to this platform, contact ",
        HTML(
          "<a href='mailto:Raja.Rajeeswari@ebmhealth.in'>Raja Rajeeswari C</a>."
        )
      )
    ))
  )),
  size = "m",
  align = "center",
  easyClose = TRUE,
  fade = T,
  footer = NULL,
  style = "padding: 2.5%;"
)

