employee_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      rpivotTableOutput(ns("pivot_saas"))   
    )
  )
  
}

employee_analytics_server <- function(input, output, session) {
  
  
  output$pivot_saas <-renderRpivotTable({
    rpivotTable(saas_data, 
                rows = c("SalesChannel", "SalesTyp", "SalesRep"),
                cols = "Period",
                aggregatorName = "Sum",
                vals = "ARR",
                rendererName = "Heatmap")
  })
  
}