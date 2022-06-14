employee_analytics_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    #tags$style(type="text/css",".pvtRows, .pvtCols, .c3-bar { background: #000080 none repeat scroll 0 0; }" ),
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