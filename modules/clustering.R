
clustering_UI <- function(id) {
  ns <- NS(id)
  
  actionButton(ns("btn_debug"), "", icon = icon("bug"))
  
}

clustering_server <- function(input, output, session) {
  
  observeEvent(input$btn_debug, {
    browser()
  })
  
  
  
  
}