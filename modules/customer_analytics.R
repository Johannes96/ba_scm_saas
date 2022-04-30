customer_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info_2")))),
    fluidRow(
      box(title="Customer data", status = "primary", width=12, collapsible = TRUE, collapsed = FALSE,
          DT::DTOutput(ns("tbl_saas_data"))),
    ),
    fluidRow(
      box(title="Address data", status = "primary", width=12, collapsible = TRUE, collapsed = TRUE,
          DT::DTOutput(ns("tbl_saas_adr"))),
    )
  )
  
  
  
  
  
  
  
  
  
  
}

customer_analytics_server <- function(input, output, session) {
  
  
  output$report_info_2 <- renderText({
    HTML(
      "This sheet displays the data tables.")
  })
  
  output$tbl_saas_data <- DT::renderDT({
    saas_data_2
  })

  output$tbl_saas_adr <- DT::renderDT({
    saas_adr
  })
  
  
}