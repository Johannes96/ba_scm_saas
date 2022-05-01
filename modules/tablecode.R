xxx_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width=12, verbatimTextOutput(ns("report_info_2")))),
    fluidRow(
      box(title="DT Table", status = "primary", width=12, collapsible = TRUE, collapsed = FALSE,
          DT::DTOutput(ns("tbl_saas_1"))),
    ),
    fluidRow(
      box(title="Reactable", status = "primary", width=12, collapsible = TRUE, collapsed = FALSE,
          reactableOutput(ns("tbl_saas_2"), width = "auto", height = "auto", inline = FALSE)),
    )
  )
  
}

xxx_server <- function(input, output, session) {
  
  
  output$report_info_2 <- renderText({
    HTML(
      "This sheet displays the data tables.")
  })
  
  output$tbl_saas_1 <-renderDT({
    saas_data
  })
  
  output$tbl_saas_2 <-renderReactable({
    reactable(saas_data)
  })

  
}