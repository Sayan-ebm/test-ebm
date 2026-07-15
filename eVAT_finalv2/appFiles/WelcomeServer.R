# Welcome modal server -----

# observeEvent(input$goToTool,{
#   removeModal()
#   updateTabItems(session, "appBox",selected = "homePanel")
# })
# 
# observeEvent(input$quit,{
#   removeModal()
#   stopApp()
# })
# 
# observeEvent(input$tour,{
#   removeModal()
#   introjs(
#     session,
#     events = list(onbeforechange = readCallback("switchTabs"))
#   )
# })

# Observer to show help modal

# observeEvent(input$appHelpModal,{
#   removeModal()
#   showModal(welcomeModal)
# })
# 
# # Observer to show about modal
# 
# observeEvent(input$about,{
#   removeModal()
#   showModal(aboutModal)
# })
#   
# output$downloadSpecDoc = downloadHandler(
#   filename = function() {
#     "Application Specification Document_v2.0 20190903.pptx"
#   },
#   content = function(file) {
#     filePath = "www\\Survival platform_Application Specification Document_v2.0 20190903.pptx"
#     file.copy(filePath, file)
#   }
# )

# Welcome page server -----

observeEvent(input$bttnWelcomeNext, {
  updateTabsetPanel(
    session = session,
    inputId = "appBox",
    selected = "survivalComparatorPanel"
  )
})

